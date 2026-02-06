  $ cat > main.c <<'EOF'
  > int main() {
  >   int* p = malloc(4);
  >   *p = 65;
  >   putchar(*p);
  >   putchar(10);
  >   free(p);
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe main.c -o out.ll
  $ /opt/homebrew/opt/llvm/bin/llvm-as out.ll -o out.bc
  $ /opt/homebrew/opt/llvm/bin/lli out.bc
  A
