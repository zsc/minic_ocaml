  $ cat > bad_escape_char.c <<'EOF_C'
  > int main() {
  >   char c = '\x';
  >   return 0;
  > }
  > EOF_C

  $ ../src/main.exe bad_escape_char.c -o out.ll 2>&1
  2:12: error: unsupported escape sequence '\x'
  [1]

  $ cat > unterminated_string.c <<'EOF_C'
  > int main() {
  >   char* s = "abc;
  >   return 0;
  > }
  > EOF_C

  $ ../src/main.exe unterminated_string.c -o out.ll 2>&1
  2:13: error: unterminated string literal
  [1]

  $ cat > multi_char.c <<'EOF_C'
  > int main() {
  >   char c = 'ab';
  >   return 0;
  > }
  > EOF_C

  $ ../src/main.exe multi_char.c -o out.ll 2>&1
  2:12: error: multi-character char literal is not supported
  [1]
