  $ cat > main.c <<'EOF'
  > void inc(int* p) {
  >   *p = *p + 1;
  >   return;
  > }
  > int main() {
  >   int x = 0;
  >   inc(&x);
  >   putchar(48 + x);
  >   putchar(10);
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe main.c -o out.ll
  $ /opt/homebrew/opt/llvm/bin/llvm-as out.ll -o out.bc
  $ /opt/homebrew/opt/llvm/bin/lli out.bc
  1
