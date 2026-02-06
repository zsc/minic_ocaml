# minic_ocaml

MiniC (C 子集) → LLVM IR 的小编译器实现（OCaml），实现细节参考 `CLAUDE.md`。

## Build

```sh
dune build
```

如果你在 Apple Silicon 上用 Rosetta/x86_64 终端导致链接报错，改用原生 arm64 终端或：

```sh
arch -arm64 dune build
```

## Run

编译 MiniC 源码到 LLVM IR：

```sh
dune exec -- minic input.c -o out.ll
```

验证 / 执行（Homebrew LLVM）：

```sh
/opt/homebrew/opt/llvm/bin/llvm-as out.ll -o out.bc
/opt/homebrew/opt/llvm/bin/lli out.bc
```

## Toplevel 交互模式

MiniC 提供交互式 toplevel（REPL）模式，适合快速测试代码片段、学习语言特性或调试程序。

### 启动方式

```sh
# 方式一：使用 --toplevel 参数
dune exec -- minic --toplevel

# 方式二：使用 -i 参数（简写）
dune exec -- minic -i
```

### 使用方法

1. **输入程序**：逐行输入完整的 MiniC 程序（可包含多个函数）
2. **结束输入**：单独输入一行 `;;` 表示程序结束
3. **执行**：系统自动编译为 LLVM IR 并执行，输出结果

### 示例会话

#### 示例 1：Hello World

```
minic> void puts(char* s) { while (*s) { putchar(*s); s = s + 1; } }
| int main() { puts("Hello, World!\n"); return 0; }
| ;;
[LLVM IR]
declare i32 @putchar(i32)
declare i32 @getchar()
declare ptr @malloc(i32)
declare void @free(ptr)

@.str.0 = private unnamed_addr constant [14 x i8] c"Hello, World!\0A\00", align 1

define void @puts(ptr %s) {
entry:
  %t0 = alloca ptr
  store ptr %s, ptr %t0
  br label %while.cond.0
while.cond.0:
  %t1 = load ptr, ptr %t0
  %t2 = load i8, ptr %t1
  %t3 = sext i8 %t2 to i32
  %t4 = icmp ne i32 %t3, 0
  br i1 %t4, label %while.body.1, label %while.end.2
while.body.1:
  %t5 = load ptr, ptr %t0
  %t6 = load i8, ptr %t5
  %t7 = sext i8 %t6 to i32
  %t8 = call i32 @putchar(i32 %t7)
  %t9 = load ptr, ptr %t0
  %t10 = sext i32 1 to i64
  %t11 = ptrtoint ptr %t9 to i64
  %t12 = add i64 %t11, %t10
  %t13 = inttoptr i64 %t12 to ptr
  store ptr %t13, ptr %t0
  br label %while.cond.0
while.end.2:
  ret void
}

define i32 @main() {
entry:
  call void @puts(ptr @.str.0)
  ret i32 0
}

[Execution Output]
Hello, World!

[Exit Code] 0
```

#### 示例 2：递归函数 + 字符串输出

使用指针算术和字符串字面量实现 `puts` 函数：

```
minic> void puts(char* s) {
|   while (*s) {
|     putchar(*s);
|     s = s + 1;
|   }
| }
| 
| int fac(int n) {
|   if (n <= 1) return 1;
|   return n * fac(n - 1);
| }
| 
| int main() {
|   puts("4! = ");
|   putchar(48 + fac(4) / 10);
|   putchar(48 + fac(4) % 10);
|   puts("\n");
|   return 0;
| }
| ;;
[LLVM IR]
declare i32 @putchar(i32)
declare i32 @getchar()
declare ptr @malloc(i32)
declare void @free(ptr)

@.str.0 = private unnamed_addr constant [6 x i8] c"4! = \00", align 1
@.str.1 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1

define void @puts(ptr %s) {
entry:
  %t0 = alloca ptr
  store ptr %s, ptr %t0
  br label %while.cond.0
while.cond.0:
  %t1 = load ptr, ptr %t0
  %t2 = load i8, ptr %t1
  %t3 = sext i8 %t2 to i32
  %t4 = icmp ne i32 %t3, 0
  br i1 %t4, label %while.body.1, label %while.end.2
while.body.1:
  %t5 = load ptr, ptr %t0
  %t6 = load i8, ptr %t5
  %t7 = sext i8 %t6 to i32
  %t8 = call i32 @putchar(i32 %t7)
  %t9 = load ptr, ptr %t0
  %t10 = sext i32 1 to i64
  %t11 = ptrtoint ptr %t9 to i64
  %t12 = add i64 %t11, %t10
  %t13 = inttoptr i64 %t13 to ptr
  store ptr %t13, ptr %t0
  br label %while.cond.0
while.end.2:
  ret void
}

define i32 @fac(i32 %n) { ... }

define i32 @main() {
entry:
  call void @puts(ptr @.str.0)
  %t0 = call i32 @fac(i32 4)
  %t1 = sdiv i32 %t0, 10
  %t2 = add i32 48, %t1
  %t3 = call i32 @putchar(i32 %t2)
  %t4 = call i32 @fac(i32 4)
  %t5 = srem i32 %t4, 10
  %t6 = add i32 48, %t5
  %t7 = call i32 @putchar(i32 %t6)
  call void @puts(ptr @.str.1)
  ret i32 0
}

[Execution Output]
4! = 24

[Exit Code] 0
```

### 支持的命令

| 命令 | 说明 |
|------|------|
| `:quit` 或 `:q` | 退出 toplevel |
| `:help` 或 `:h` | 显示帮助信息 |

### 行编辑功能

在终端交互模式下支持以下编辑功能：
- **↑ / ↓**：浏览历史输入
- **← / →**：移动光标
- **Backspace**：删除字符
- **Enter**：换行继续输入

## Tests

```sh
arch -arm64 dune runtest
```

## 支持的语言特性（摘要）

- `int` / `char` / `void`、指针 `T*`
- `struct` / `union` 类型定义与局部变量，成员访问 `.` / `->`
- 块作用域、shadowing
- 语句：`if/else`、`while`、`return`、`break`、`continue`
- 字面量：整数字面量、字符字面量（如 `'A'`）、字符串字面量（如 `"hello"`）
- 表达式：赋值、算术（含 `ptr +/- int`、`ptr - ptr`）、比较、`&&`/`||` 短路、`!`、`&`、`*`、函数调用
- 内建外部函数声明：`putchar/getchar/malloc/free`
- 当前限制：`struct/union` 暂不支持按值传参、按值返回和整体赋值
