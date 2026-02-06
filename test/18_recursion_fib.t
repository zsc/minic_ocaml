  $ cat > main.c <<'EOF'
  > int fib(int n) {
  >   if (n < 2) return n;
  >   return fib(n - 1) + fib(n - 2);
  > }
  > 
  > int main() {
  >   int x = fib(5);
  >   putchar(48 + x);
  >   putchar(10);
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe main.c -o out.ll
  $ /opt/homebrew/opt/llvm/bin/llvm-as out.ll -o out.bc
  $ /opt/homebrew/opt/llvm/bin/lli out.bc
  5

