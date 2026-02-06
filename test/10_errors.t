  $ cat > uninit.c <<'EOF'
  > int main() {
  >   int x;
  >   putchar(x);
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe uninit.c -o out.ll 2>&1
  3:11: error: use of uninitialized variable x
  [1]

  $ cat > missing_return.c <<'EOF'
  > int f(int x) {
  >   if (x) return 1;
  > }
  > EOF

  $ ../src/main.exe missing_return.c -o out.ll 2>&1
  1:1: error: missing return value
  [1]

  $ cat > break_outside.c <<'EOF'
  > int main() {
  >   break;
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe break_outside.c -o out.ll 2>&1
  2:3: error: break not in loop
  [1]

  $ cat > void_return_value.c <<'EOF'
  > void f() {
  >   return 1;
  > }
  > EOF

  $ ../src/main.exe void_return_value.c -o out.ll 2>&1
  2:3: error: void function cannot return a value
  [1]
