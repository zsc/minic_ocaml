  $ cat > main.c <<'EOF'
  > int main() {
  >   int a = 3;
  >   int b = 3;
  >   int x = (a == b);
  >   int y = (a != b);
  >   int z = !(a - 3);
  >   int w = (a < 4) && (a >= 3);
  >   putchar(48 + x);
  >   putchar(48 + y);
  >   putchar(48 + z);
  >   putchar(48 + w);
  >   putchar(10);
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe main.c -o out.ll
  $ /opt/homebrew/opt/llvm/bin/llvm-as out.ll -o out.bc
  $ /opt/homebrew/opt/llvm/bin/lli out.bc
  1011

