  $ cat > main.c <<'EOF'
  > int main() {
  >   int x = 0;
  >   if (1) {
  >     x = 1;
  >   } else {
  >     x = 2;
  >   }
  >   putchar(48 + x);
  >   putchar(10);
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe main.c -o out.ll
  $ /opt/homebrew/opt/llvm/bin/llvm-as out.ll -o out.bc
  $ /opt/homebrew/opt/llvm/bin/lli out.bc
  1
