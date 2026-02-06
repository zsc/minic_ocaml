  $ cat > main.c <<'EOF'
  > int main() {
  >   int x = 2 + 3 * 4;
  >   putchar(48 + (x / 10));
  >   putchar(48 + (x % 10));
  >   putchar(10);
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe main.c -o out.ll
  $ /opt/homebrew/opt/llvm/bin/llvm-as out.ll -o out.bc
  $ /opt/homebrew/opt/llvm/bin/lli out.bc
  14
