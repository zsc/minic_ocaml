  $ cat > unknown_field.c <<'EOF_C'
  > struct S {
  >   int x;
  > };
  > 
  > int main() {
  >   struct S s;
  >   s.y = 1;
  >   return 0;
  > }
  > EOF_C

  $ ../src/main.exe unknown_field.c -o out.ll 2>&1
  7:3: error: unknown field y in struct S
  [1]

  $ cat > by_value_param.c <<'EOF_C'
  > struct S {
  >   int x;
  > };
  > 
  > int f(struct S s) {
  >   return 0;
  > }
  > EOF_C

  $ ../src/main.exe by_value_param.c -o out.ll 2>&1
  5:16: error: parameter s cannot be struct/union by value
  [1]

  $ cat > aggregate_assign.c <<'EOF_C'
  > struct S {
  >   int x;
  > };
  > 
  > int main() {
  >   struct S a;
  >   struct S b;
  >   a = b;
  >   return 0;
  > }
  > EOF_C

  $ ../src/main.exe aggregate_assign.c -o out.ll 2>&1
  8:3: error: aggregate assignment is not supported
  [1]
