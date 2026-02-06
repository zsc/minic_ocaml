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

## Tests

```sh
arch -arm64 dune runtest
```

## 支持的语言特性（摘要）

- `int` / `char` / `void`、指针 `T*`
- 块作用域、shadowing
- 语句：`if/else`、`while`、`return`、`break`、`continue`
- 表达式：赋值、算术、比较、`&&`/`||` 短路、`!`、`&`、`*`、函数调用
- 内建外部函数声明：`putchar/getchar/malloc/free`
