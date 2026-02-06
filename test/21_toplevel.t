  $ cat > repl.in <<'EOF_IN'
  > int main() {
  >   putchar(65);
  >   putchar(10);
  >   return 0;
  > }
  > ;;
  > :quit
  > EOF_IN

  $ ../src/main.exe --toplevel < repl.in > repl.out

  $ grep -F "define i32 @main()" repl.out
  define i32 @main() {

  $ grep -Fx "A" repl.out
  A

  $ grep -F "[Exit Code] 0" repl.out
  [Exit Code] 0
