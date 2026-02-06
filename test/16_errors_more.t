  $ cat > continue_outside.c <<'EOF'
  > int main() {
  >   continue;
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe continue_outside.c -o out.ll 2>&1
  2:3: error: continue not in loop
  [1]

  $ cat > void_var.c <<'EOF'
  > int main() {
  >   void x;
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe void_var.c -o out.ll 2>&1
  2:3: error: cannot declare variable of type void
  [1]

  $ cat > deref_void.c <<'EOF'
  > int main() {
  >   void* p = malloc(4);
  >   int x = *p;
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe deref_void.c -o out.ll 2>&1
  3:11: error: cannot dereference void*
  [1]

  $ cat > unknown_func.c <<'EOF'
  > int main() {
  >   foo();
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe unknown_func.c -o out.ll 2>&1
  2:3: error: call to unknown function foo
  [1]

  $ cat > wrong_args.c <<'EOF'
  > int main() {
  >   putchar(1, 2);
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe wrong_args.c -o out.ll 2>&1
  2:3: error: function putchar expects 1 args, got 2
  [1]

  $ cat > dup_decl.c <<'EOF'
  > int main() {
  >   int x = 1;
  >   int x = 2;
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe dup_decl.c -o out.ll 2>&1
  3:3: error: duplicate declaration of x
  [1]

  $ cat > ptr_plus_ptr.c <<'EOF'
  > int main() {
  >   int x = 0;
  >   int y = 0;
  >   int* p = &x;
  >   int* q = &y;
  >   p + q;
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe ptr_plus_ptr.c -o out.ll 2>&1
  6:3: error: pointer arithmetic requires ptr +/- int
  [1]

  $ cat > assign_non_lvalue.c <<'EOF'
  > int main() {
  >   int a = 1;
  >   a + 1 = 2;
  >   return 0;
  > }
  > EOF

  $ ../src/main.exe assign_non_lvalue.c -o out.ll 2>&1
  3:3: error: expected lvalue
  [1]
