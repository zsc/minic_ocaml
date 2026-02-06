  $ cat > main.c <<'EOF'
  > int main() {
  >   putchar(g());
  >   putchar(10);
  >   return 0;
  > }
  > 
  > int g() {
  >   return 65;
  > }
  > EOF

  $ ../src/main.exe main.c -o out.ll
  $ /opt/homebrew/opt/llvm/bin/llvm-as out.ll -o out.bc
  $ /opt/homebrew/opt/llvm/bin/lli out.bc
  A

