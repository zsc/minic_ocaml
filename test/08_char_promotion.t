  $ cat > main.c <<'EOF'
  > int main() {
  >   char c = 65;
  >   int x = c + 1;
  >   putchar(x);
  >   putchar(10);
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe main.c -o out.ll
  $ /opt/homebrew/opt/llvm/bin/llvm-as out.ll -o out.bc
  $ /opt/homebrew/opt/llvm/bin/lli out.bc
  B
