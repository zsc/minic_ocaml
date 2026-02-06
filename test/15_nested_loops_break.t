  $ cat > main.c <<'EOF'
  > int main() {
  >   int i = 0;
  >   while (i < 2) {
  >     int j = 0;
  >     while (j < 3) {
  >       if (j == 1) break;
  >       putchar(48 + i);
  >       putchar(48 + j);
  >       j = j + 1;
  >     }
  >     putchar(10);
  >     i = i + 1;
  >   }
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe main.c -o out.ll
  $ /opt/homebrew/opt/llvm/bin/llvm-as out.ll -o out.bc
  $ /opt/homebrew/opt/llvm/bin/lli out.bc
  00
  10

