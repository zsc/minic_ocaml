  $ cat > main.c <<'EOF'
  > int main() {
  >   int i = 0;
  >   while (i < 5) {
  >     if (i == 2) {
  >       i = i + 1;
  >       continue;
  >     }
  >     putchar(48 + i);
  >     i = i + 1;
  >   }
  >   putchar(10);
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe main.c -o out.ll
  $ /opt/homebrew/opt/llvm/bin/llvm-as out.ll -o out.bc
  $ /opt/homebrew/opt/llvm/bin/lli out.bc
  0134
