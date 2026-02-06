let compile_to_llvm (src : string) : string =
  let lx = Lexer.make src in
  let parser = Parser.make lx in
  let prog = Parser.parse_program parser in
  let tprog = Typecheck.typecheck prog in
  Codegen.gen_program tprog

