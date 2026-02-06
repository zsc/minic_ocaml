  $ cat > main.c <<'EOF'
  > int side(int x) {
  >   putchar(x);
  >   return 1;
  > }
  > int main() {
  >   int a = 0;
  >   if (a && side(88)) {
  >     putchar(49);
  >   } else {
  >     putchar(48);
  >   }
  >   putchar(10);
  >   a = 1;
  >   if (a || side(89)) {
  >     putchar(50);
  >   }
  >   putchar(10);
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe main.c -o out.ll
  $ /opt/homebrew/opt/llvm/bin/llvm-as out.ll -o out.bc
  $ /opt/homebrew/opt/llvm/bin/lli out.bc
  0
  2
