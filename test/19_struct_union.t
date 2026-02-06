  $ cat > main.c <<'EOF_C'
  > struct Pair {
  >   int a;
  >   char b;
  > };
  > 
  > union U {
  >   int i;
  >   char c;
  > };
  > 
  > struct Packed {
  >   char c;
  >   int x;
  > };
  > 
  > int main() {
  >   struct Pair p;
  >   p.a = 65;
  >   p.b = 10;
  >   putchar(p.a);
  >   putchar(p.b);
  > 
  >   struct Pair* pp = &p;
  >   pp->a = 66;
  >   putchar(pp->a);
  >   putchar(10);
  > 
  >   union U u;
  >   u.c = 67;
  >   putchar(u.c);
  >   putchar(10);
  > 
  >   union U* up = &u;
  >   up->c = 68;
  >   putchar(up->c);
  >   putchar(10);
  > 
  >   struct Packed q;
  >   q.c = 0;
  >   q.x = 69;
  >   putchar(q.x);
  >   putchar(10);
  >   return 0;
  > }
  > EOF_C

  $ ../src/main.exe main.c -o out.ll
  $ /opt/homebrew/opt/llvm/bin/llvm-as out.ll -o out.bc
  $ /opt/homebrew/opt/llvm/bin/lli out.bc
  A
  B
  C
  D
  E
