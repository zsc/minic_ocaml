  $ cat > main.c <<'EOF'
  > int main() {
  >   int x = 1;
  >   {
  >     int x = 2;
  >     putchar(48 + x);
  >   }
  >   putchar(48 + x);
  >   putchar(10);
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe main.c -o out.ll
  $ /opt/homebrew/opt/llvm/bin/llvm-as out.ll -o out.bc
  $ /opt/homebrew/opt/llvm/bin/lli out.bc
  21
