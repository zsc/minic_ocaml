  $ cat > main.c <<'EOF'
  > int main() {
  >   void* p = malloc(4);
  >   int* q = p;
  >   void* r = q;
  >   *q = 65;
  >   putchar(*q);
  >   putchar(10);
  >   if (p == r) {
  >     putchar(49);
  >   } else {
  >     putchar(48);
  >   }
  >   putchar(10);
  >   free(p);
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe main.c -o out.ll
  $ /opt/homebrew/opt/llvm/bin/llvm-as out.ll -o out.bc
  $ /opt/homebrew/opt/llvm/bin/lli out.bc
  A
  1

