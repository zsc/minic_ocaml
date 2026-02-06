type t =
  { src : string
  ; len : int
  ; mutable i : int
  ; mutable line : int
  ; mutable col : int
  }

let make (src : string) : t = { src; len = String.length src; i = 0; line = 1; col = 1 }

let current_pos (lx : t) : Loc.position = { offset = lx.i; line = lx.line; col = lx.col }

let bump (lx : t) : unit =
  if lx.i < lx.len then (
    match lx.src.[lx.i] with
    | '\n' ->
        lx.i <- lx.i + 1;
        lx.line <- lx.line + 1;
        lx.col <- 1
    | _ ->
        lx.i <- lx.i + 1;
        lx.col <- lx.col + 1)

let peek (lx : t) : char option = if lx.i >= lx.len then None else Some lx.src.[lx.i]

let peek2 (lx : t) : char option =
  if lx.i + 1 >= lx.len then None else Some lx.src.[lx.i + 1]

let rec skip_ws_and_comments (lx : t) : unit =
  let rec skip_ws () =
    match peek lx with
    | Some (' ' | '\t' | '\r' | '\n') ->
        bump lx;
        skip_ws ()
    | _ -> ()
  in
  skip_ws ();
  match (peek lx, peek2 lx) with
  | Some '/', Some '/' ->
      while
        match peek lx with
        | None -> false
        | Some '\n' -> false
        | Some _ ->
            bump lx;
            true
      do
        ()
      done;
      skip_ws_and_comments lx
  | Some '/', Some '*' ->
      bump lx;
      bump lx;
      let rec skip_block () =
        match (peek lx, peek2 lx) with
        | None, _ -> Util.error Loc.none "unterminated block comment"
        | Some '*', Some '/' ->
            bump lx;
            bump lx
        | _ ->
            bump lx;
            skip_block ()
      in
      skip_block ();
      skip_ws_and_comments lx
  | _ -> ()

let is_ident_start = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false

let is_ident_continue = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
  | _ -> false

let rec lex_ident (lx : t) (buf : Buffer.t) : unit =
  match peek lx with
  | Some c when is_ident_continue c ->
      Buffer.add_char buf c;
      bump lx;
      lex_ident lx buf
  | _ -> ()

let rec lex_int (lx : t) (acc : int) : int =
  match peek lx with
  | Some ('0' .. '9' as c) ->
      bump lx;
      let digit = Char.code c - Char.code '0' in
      let acc' = (acc * 10) + digit in
      lex_int lx acc'
  | _ -> acc

let keyword_or_ident (s : string) : Token.kind =
  match s with
  | "int" -> Kw_int
  | "char" -> Kw_char
  | "void" -> Kw_void
  | "struct" -> Kw_struct
  | "union" -> Kw_union
  | "if" -> Kw_if
  | "else" -> Kw_else
  | "while" -> Kw_while
  | "return" -> Kw_return
  | "break" -> Kw_break
  | "continue" -> Kw_continue
  | _ -> Ident s

let next (lx : t) : Token.t =
  skip_ws_and_comments lx;
  let start = current_pos lx in
  let mk kind =
    let end_ = current_pos lx in
    { Token.kind; loc = { Loc.start; end_ } }
  in
  match peek lx with
  | None -> mk Eof
  | Some c -> (
      match c with
      | '(' ->
          bump lx;
          mk L_paren
      | ')' ->
          bump lx;
          mk R_paren
      | '{' ->
          bump lx;
          mk L_brace
      | '}' ->
          bump lx;
          mk R_brace
      | ',' ->
          bump lx;
          mk Comma
      | ';' ->
          bump lx;
          mk Semi
      | '.' ->
          bump lx;
          mk Dot
      | '+' ->
          bump lx;
          mk Plus
      | '-' ->
          bump lx;
          (match peek lx with
          | Some '>' ->
              bump lx;
              mk Arrow
          | _ -> mk Minus)
      | '*' ->
          bump lx;
          mk Star
      | '/' ->
          bump lx;
          mk Slash
      | '%' ->
          bump lx;
          mk Percent
      | '!' -> (
          bump lx;
          match peek lx with
          | Some '=' ->
              bump lx;
              mk Bang_eq
          | _ -> mk Bang)
      | '&' -> (
          bump lx;
          match peek lx with
          | Some '&' ->
              bump lx;
              mk Amp_amp
          | _ -> mk Amp)
      | '|' -> (
          bump lx;
          match peek lx with
          | Some '|' ->
              bump lx;
              mk Pipe_pipe
          | _ -> Util.error { Loc.start; end_ = current_pos lx } "unexpected token '|'")
      | '=' -> (
          bump lx;
          match peek lx with
          | Some '=' ->
              bump lx;
              mk Eq_eq
          | _ -> mk Eq)
      | '<' -> (
          bump lx;
          match peek lx with
          | Some '=' ->
              bump lx;
              mk Lt_eq
          | _ -> mk Lt)
      | '>' -> (
          bump lx;
          match peek lx with
          | Some '=' ->
              bump lx;
              mk Gt_eq
          | _ -> mk Gt)
      | '0' .. '9' ->
          let n = lex_int lx 0 in
          mk (Int_lit n)
      | _ when is_ident_start c ->
          let buf = Buffer.create 16 in
          Buffer.add_char buf c;
          bump lx;
          lex_ident lx buf;
          let s = Buffer.contents buf in
          mk (keyword_or_ident s)
      | _ ->
          bump lx;
          Util.error { Loc.start; end_ = current_pos lx } "unexpected character '%c'" c)
