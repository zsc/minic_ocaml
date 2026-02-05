下面给你一个**“可落地实现”的 C 子集 → LLVM IR（IL）代码生成**的 spec（偏实现导向：语法范围、语义约束、类型系统、运行时/ABI、以及 IR 生成规则）。你可以直接拿它做 parser/AST/类型检查/IR lowering 的路线图。

---

## 1) 目标与非目标

### 目标

* 输入：一个限制版 C（下称 **MiniC**）源文件
* 输出：可被 `llvm-as`/`llc`/`clang` 工具链处理的 LLVM IR
* 设计偏向：**简单、可预测、容易实现**（最少 UB、最少复杂类型）

### 非目标（明确不支持）

* 预处理器（`#include/#define` 等）
* 浮点（`float/double/long double`）
* 结构体/联合体/位域
* 指针算术（除 `&` 和 `*` 的基本用法外）
* 变长数组、`goto`、`switch`、`do-while`
* 多文件链接模型（先做单文件 module）

---

## 2) 词法与标识符

* 标识符：`[A-Za-z_][A-Za-z0-9_]*`
* 关键字：`int`, `char`, `void`, `if`, `else`, `while`, `return`, `break`, `continue`
* 整数字面量：十进制 `0|[1-9][0-9]*`（可选支持 `-` 作为一元运算）
* 字符字面量：可选（若支持：仅 `'a'`、`'\n'` 这类简单转义）

---

## 3) 语法（建议 EBNF）

### 顶层

```
program     := (function_def)* EOF ;
function_def:= type ident "(" param_list? ")" block ;

type        := "int" | "char" | "void" | type "*" ;   // 指针可选
param_list  := param ("," param)* ;
param       := type ident ;
```

### 语句

```
block       := "{" (decl_or_stmt)* "}" ;
decl_or_stmt:= declaration | statement ;

declaration := type ident ("=" expr)? ";" ;

statement   := block
            | "if" "(" expr ")" statement ("else" statement)?
            | "while" "(" expr ")" statement
            | "return" expr? ";"
            | "break" ";"
            | "continue" ";"
            | expr ";"                // 表达式语句
            | ";"                     // 空语句
            ;
```

### 表达式（优先级从低到高）

```
expr        := assign ;
assign      := logic_or (("=") assign)? ;          // 右结合
logic_or    := logic_and ( "||" logic_and )* ;
logic_and   := equality ( "&&" equality )* ;
equality    := rel ( ("=="|"!=") rel )* ;
rel         := add ( ("<"|"<="|">"|">=") add )* ;
add         := mul ( ("+"|"-") mul )* ;
mul         := unary ( ("*"|"/"|"%") unary )* ;
unary       := ("+"|"-"|"!"|"&"|"*") unary | primary ;
primary     := int_lit
            | ident
            | ident "(" arg_list? ")"              // 函数调用
            | "(" expr ")"
            ;
arg_list    := expr ("," expr)* ;
```

> **实现建议**：先做无指针版本（去掉 `&`、`*`、`type*`），功能闭环后再加指针。

---

## 4) 类型系统与约束

### 支持类型

* `int`：32-bit 有符号（LLVM `i32`）
* `char`：8-bit 有符号或无符号（二选一，建议**有符号**，LLVM `i8`）
* `void`：仅用于函数返回类型
* （可选）指针 `T*`：LLVM `ptr`（或 `i8*`/`T*`，取决于你用的是 opaque pointer 还是 typed pointer）

### 隐式转换（尽量少）

* `char` 在算术/比较时 **提升**为 `int`
* 赋值时允许 `int -> char`（截断），`char -> int`（扩展）
* `void` 不能参与表达式（除非你允许调用返回 void 的函数作为语句）

### 运算符类型规则（核心）

* 算术 `+ - * / %`：操作数先整型提升到 `int`，结果 `int`
* 比较/相等：结果类型 `int`，值为 0 或 1
* 逻辑 `&& || !`：把操作数视作布尔（非 0 为真），结果 `int` 0/1；`&&/||` 必须短路
* 赋值 `=`：左侧必须是可写 lvalue（变量或 `*p`）

### 作用域

* 块级作用域：`{}` 引入新 scope；允许 shadowing（建议允许但实现要小心符号表栈）
* 变量必须声明后使用

---

## 5) 程序结构与符号

### 函数

* 仅支持**函数定义**（先不支持仅声明/extern，除非你要调用 libc）
* 参数按值传递：`int/char` 直接传
* 返回：`int/char/void`

### 全局变量（建议第一版不做）

* 若做：只允许 `int/char` 的常量初始化

---

## 6) 语义约束（为了简单而“定死”）

* 未初始化局部变量的读取：编译时报错（比 C 更严格）
* 除 0：可先不做运行时检查（标注为 UB），或加一个简单 trap
* `break/continue` 只能在循环内部
* `return`：

  * `void` 函数必须 `return;` 或隐式 return（允许 `ret void`）
  * 非 void 函数必须所有路径返回值（可做简单控制流检查，不做也行但建议做）

---

## 7) LLVM IR 生成规范（核心）

### 模块与数据布局

* 目标无关：可以不写 `target triple/datalayout`，让下游工具补
* 使用 LLVM 推荐的 SSA 形式：

  * **方案 A（最简单）**：先生成“内存形式”IR（alloca + load/store），再依赖 `mem2reg` 做 SSA
  * **方案 B**：直接 SSA + phi（实现复杂）

> 强烈建议 **A**：每个局部变量一个 `alloca`，在入口块分配，然后对变量读写用 `load/store`。

### 局部变量与参数

* 在函数 entry block：

  * 对每个局部变量：`%x.addr = alloca i32`
  * 对每个参数：也分配一个地址并 store 进去（方便统一成 lvalue）

    * `%a.addr = alloca i32`
    * `store i32 %a, ptr %a.addr`

### 表达式 lowering

* 常量：`i32` 立即数
* 变量引用（rvalue）：`load` 变量地址
* 赋值：

  * 先生成 RHS 值
  * `store` 到 LHS 地址
  * 赋值表达式的结果值：通常是 RHS（与 C 一致）

### 算术与比较

* `+` → `add`
* `-` → `sub`
* `*` → `mul`
* `/` → `sdiv`（有符号）
* `%` → `srem`
* 比较：

  * `<` → `icmp slt`
  * `<=` → `icmp sle`
  * `>` → `icmp sgt`
  * `>=` → `icmp sge`
  * `==` → `icmp eq`
  * `!=` → `icmp ne`
* 注意：`icmp` 产物是 `i1`，但 spec 里布尔结果是 `int`（`i32` 0/1）

  * 用 `zext i1 to i32`

### 逻辑短路

`&&` 和 `||` 必须用基本块+分支实现：

* `a && b`：

  1. 计算 `a` → 转成 `i1`（`icmp ne` 与 0）
  2. `br i1 a_bool, label %rhs, label %false`
  3. `rhs:` 计算 `b_bool`，跳到 `merge`
  4. `false:` 直接跳 `merge`
  5. `merge:` 用 `phi i1` 选择 `false` 或 `b_bool`
  6. 最后 `zext` 到 `i32`

`||` 类似，只是短路真分支。

### 控制流语句

* `if (cond) S1 else S2`：

  * cond → `i1`（非0）
  * `br i1 cond, %then, %else`
  * then/else 末尾都 `br %merge`（除非已 `ret`）
* `while (cond) body`：

  * 经典三块：`cond`/`body`/`end`
  * `continue` → 跳 `cond`，`break` → 跳 `end`

### return

* `return expr;` → `ret i32 %v`（或 `i8`，必要时 zext/trunc）
* `return;` in void → `ret void`

### 函数调用

* 先计算实参（值）
* `call` 指令
* void 返回的 call 只能作为语句

---

## 8) 标准库 / 内建函数（可选但很实用）

为了做 demo，你可以内置几个“外部函数声明”（不属于 MiniC 语法层面的 `#include`）：

* `int putchar(int c);`
* `int getchar(void);`
* `void* malloc(int n);`（如果你引入指针/heap）
* `void free(void* p);`

在 IR 里直接 `declare i32 @putchar(i32)` 之类即可。

---

## 9) 错误处理规范（编译期）

至少给出这些诊断：

* 语法错误：行列 + 期望 token
* 未声明标识符
* 重复声明（同一 scope）
* 类型不匹配（例如把 `void` 用在表达式里）
* 非 lvalue 赋值（`(a+b)=1`）
* break/continue 不在循环中

---

## 10) 建议的实现里程碑（按难度递增）

1. 仅 `int`、局部变量、算术表达式、`return`
2. `if/else`
3. `while` + `break/continue`
4. 函数调用 + 参数
5. `char` + int/char 转换
6. 逻辑短路 `&& || !`
7. （可选）指针 `&` `*` + `malloc/free`
## Gemini Added Memories
- 用 python 不用 python3
- The user's operating system is: darwin
- may use Apple Silicon MPS for acceleration
- [a meta request bout the working process] use `echo $'\a'` to notify the user when waiting for input
