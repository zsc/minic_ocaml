  $ cat > main.c <<'EOF_C'
  > int emit(char* p) {
  >   putchar(*p);
  >   return 0;
  > }
  > 
  > int main() {
  >   char c = 'A';
  >   putchar(c);
  >   putchar('\n');
  > 
  >   emit("B");
  >   putchar('\n');
  > 
  >   char* s = "C";
  >   putchar(*s);
  >   putchar('\n');
  > 
  >   char q = '\'';
  >   putchar(q);
  >   putchar('\n');
  > 
  >   char bs = '\\';
  >   putchar(bs);
  >   putchar('\n');
  >   return 0;
  > }
  > EOF_C

  $ ../src/main.exe main.c -o out.ll
  $ grep -F "private unnamed_addr constant" out.ll
  @.str.0 = private unnamed_addr constant [2 x i8] c"B\00", align 1
  @.str.1 = private unnamed_addr constant [2 x i8] c"C\00", align 1
  $ /opt/homebrew/opt/llvm/bin/llvm-as out.ll -o out.bc
  $ /opt/homebrew/opt/llvm/bin/lli out.bc
  A
  B
  C
  '
  \
