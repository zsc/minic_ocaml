  $ cat > main.c <<'EOF_C'
  > int main() {
  >   int* p = malloc(16);
  >   *p = 65;
  >   int* q = p + 1;
  >   *q = 66;
  >   putchar(*p);
  >   putchar(*q);
  >   putchar(10);
  > 
  >   int d = q - p;
  >   putchar(48 + d);
  >   putchar(10);
  > 
  >   int* r = q - 1;
  >   putchar(*r);
  >   putchar(10);
  > 
  >   int* s = 1 + p;
  >   if (s == q) {
  >     putchar(49);
  >   } else {
  >     putchar(48);
  >   }
  >   putchar(10);
  >   free(p);
  >   return 0;
  > }
  > EOF_C

  $ ../src/main.exe main.c -o out.ll
  $ /opt/homebrew/opt/llvm/bin/llvm-as out.ll -o out.bc
  $ /opt/homebrew/opt/llvm/bin/lli out.bc
  AB
  1
  A
  1
