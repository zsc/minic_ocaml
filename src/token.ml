type kind =
  | Ident of string
  | Int_lit of int
  | Kw_int
  | Kw_char
  | Kw_void
  | Kw_if
  | Kw_else
  | Kw_while
  | Kw_return
  | Kw_break
  | Kw_continue
  | L_paren
  | R_paren
  | L_brace
  | R_brace
  | Comma
  | Semi
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Bang
  | Amp
  | Eq
  | Eq_eq
  | Bang_eq
  | Lt
  | Lt_eq
  | Gt
  | Gt_eq
  | Amp_amp
  | Pipe_pipe
  | Eof

type t =
  { kind : kind
  ; loc : Loc.t
  }

let kind_to_string = function
  | Ident s -> Printf.sprintf "identifier(%s)" s
  | Int_lit n -> Printf.sprintf "int(%d)" n
  | Kw_int -> "int"
  | Kw_char -> "char"
  | Kw_void -> "void"
  | Kw_if -> "if"
  | Kw_else -> "else"
  | Kw_while -> "while"
  | Kw_return -> "return"
  | Kw_break -> "break"
  | Kw_continue -> "continue"
  | L_paren -> "("
  | R_paren -> ")"
  | L_brace -> "{"
  | R_brace -> "}"
  | Comma -> ","
  | Semi -> ";"
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | Percent -> "%"
  | Bang -> "!"
  | Amp -> "&"
  | Eq -> "="
  | Eq_eq -> "=="
  | Bang_eq -> "!="
  | Lt -> "<"
  | Lt_eq -> "<="
  | Gt -> ">"
  | Gt_eq -> ">="
  | Amp_amp -> "&&"
  | Pipe_pipe -> "||"
  | Eof -> "<eof>"

