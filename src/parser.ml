open Ast

type t =
  { lx : Lexer.t
  ; mutable cur : Token.t
  }

let make (lx : Lexer.t) : t =
  let cur = Lexer.next lx in
  { lx; cur }

let bump (p : t) : unit = p.cur <- Lexer.next p.lx

let expect (p : t) (k : Token.kind) : Token.t =
  if p.cur.kind = k then (
    let t = p.cur in
    bump p;
    t)
  else Util.error p.cur.loc "expected %s, got %s" (Token.kind_to_string k)
         (Token.kind_to_string p.cur.kind)

let accept (p : t) (k : Token.kind) : Token.t option =
  if p.cur.kind = k then (
    let t = p.cur in
    bump p;
    Some t)
  else None

let current_loc (p : t) : Loc.t = p.cur.loc

let span_from (a : Loc.t) (b : Loc.t) : Loc.t = { Loc.start = a.start; end_ = b.end_ }

let expect_ident (p : t) ~(what : string) : string * Loc.t =
  match p.cur.kind with
  | Token.Ident s ->
      let loc = p.cur.loc in
      bump p;
      (s, loc)
  | _ -> Util.error p.cur.loc "%s, got %s" what (Token.kind_to_string p.cur.kind)

let parse_tag_head (p : t) : Ast.tag_kind * string * Ast.ty =
  match p.cur.kind with
  | Token.Kw_struct ->
      bump p;
      let name, _ = expect_ident p ~what:"expected struct tag name" in
      (Ast.Struct, name, Ast.TStruct name)
  | Token.Kw_union ->
      bump p;
      let name, _ = expect_ident p ~what:"expected union tag name" in
      (Ast.Union, name, Ast.TUnion name)
  | _ -> Util.error p.cur.loc "expected struct/union tag head"

let rec parse_ptr_suffix (p : t) (ty : Ast.ty) : Ast.ty =
  match accept p Token.Star with
  | Some _ -> parse_ptr_suffix p (Ast.TPtr ty)
  | None -> ty

let rec parse_program (p : t) : Ast.program =
  let rec loop acc =
    match p.cur.kind with
    | Token.Eof -> List.rev acc
    | _ ->
        let top = parse_top p in
        loop (top :: acc)
  in
  loop []

and parse_top (p : t) : Ast.top =
  match p.cur.kind with
  | Token.Kw_struct | Token.Kw_union ->
      let start_loc = current_loc p in
      let tag_kind, tag_name, tag_ty = parse_tag_head p in
      if p.cur.kind = Token.L_brace then
        Ast.TopTypeDef (parse_type_def_after_head p start_loc tag_kind tag_name)
      else
        let ret_ty = parse_ptr_suffix p tag_ty in
        Ast.TopFunc (parse_function_with_ret p start_loc ret_ty)
  | _ ->
      let f = parse_function p in
      Ast.TopFunc f

and parse_type_def_after_head (p : t) (start_loc : Loc.t) (tag_kind : Ast.tag_kind)
    (tag_name : string) : Ast.type_def =
  ignore (expect p Token.L_brace);
  let rec parse_fields acc =
    match p.cur.kind with
    | Token.R_brace ->
        ignore (expect p Token.R_brace);
        let semi = expect p Token.Semi in
        { Ast.tag_kind; tag_name; fields = List.rev acc; loc = span_from start_loc semi.loc }
    | Token.Eof -> Util.error p.cur.loc "unexpected end of file in struct/union definition"
    | _ ->
        let field_start = current_loc p in
        let fty = parse_type p in
        let fname, _ = expect_ident p ~what:"expected field name" in
        let semi = expect p Token.Semi in
        let floc = span_from field_start semi.loc in
        parse_fields ((fty, fname, floc) :: acc)
  in
  parse_fields []

and parse_function (p : t) : Ast.func =
  let start_loc = current_loc p in
  let ret_ty = parse_type p in
  parse_function_with_ret p start_loc ret_ty

and parse_function_with_ret (p : t) (start_loc : Loc.t) (ret_ty : Ast.ty) : Ast.func =
  let name, _ =
    expect_ident p ~what:"expected function name"
  in
  ignore (expect p Token.L_paren);
  let params =
    match p.cur.kind with
    | Token.R_paren ->
        bump p;
        []
    | _ ->
        let ps = parse_param_list p in
        ignore (expect p Token.R_paren);
        ps
  in
  let body = parse_block p in
  { Ast.ret_ty; name; params; body; loc = span_from start_loc body.sloc }

and parse_param_list (p : t) : Ast.param list =
  let rec loop acc =
    let param = parse_param p in
    match accept p Token.Comma with
    | Some _ -> loop (param :: acc)
    | None -> List.rev (param :: acc)
  in
  loop []

and parse_param (p : t) : Ast.param =
  let ty = parse_type p in
  let name, loc = expect_ident p ~what:"expected parameter name" in
  (ty, name, loc)

and parse_type (p : t) : Ast.ty =
  let base =
    match p.cur.kind with
    | Token.Kw_int ->
        bump p;
        Ast.TInt
    | Token.Kw_char ->
        bump p;
        Ast.TChar
    | Token.Kw_void ->
        bump p;
        Ast.TVoid
    | Token.Kw_struct | Token.Kw_union ->
        let _, _, ty = parse_tag_head p in
        ty
    | _ -> Util.error p.cur.loc "expected type, got %s" (Token.kind_to_string p.cur.kind)
  in
  parse_ptr_suffix p base

and parse_block (p : t) : Ast.stmt =
  let lbrace = expect p Token.L_brace in
  let rec loop (acc : Ast.decl_or_stmt list) : Ast.stmt =
    match p.cur.kind with
    | Token.R_brace ->
        let rbrace = p.cur in
        bump p;
        { snode = Block (List.rev acc); sloc = span_from lbrace.loc rbrace.loc }
    | Token.Eof -> Util.error p.cur.loc "unexpected end of file in block"
    | _ ->
        let item = parse_decl_or_stmt p in
        loop (item :: acc)
  in
  loop []

and parse_decl_or_stmt (p : t) : Ast.decl_or_stmt =
  match p.cur.kind with
  | Token.Kw_int | Token.Kw_char | Token.Kw_void | Token.Kw_struct | Token.Kw_union ->
      let start_loc = current_loc p in
      let ty = parse_type p in
      let name, _ = expect_ident p ~what:"expected identifier in declaration" in
      let init =
        match accept p Token.Eq with
        | Some _ -> Some (parse_expr p)
        | None -> None
      in
      let semi = expect p Token.Semi in
      Decl (ty, name, init, span_from start_loc semi.loc)
  | _ -> Stmt (parse_stmt p)

and parse_stmt (p : t) : Ast.stmt =
  match p.cur.kind with
  | Token.L_brace -> parse_block p
  | Token.Kw_if -> parse_if p
  | Token.Kw_while -> parse_while p
  | Token.Kw_return -> parse_return p
  | Token.Kw_break ->
      let t = p.cur in
      bump p;
      ignore (expect p Token.Semi);
      { snode = Break; sloc = t.loc }
  | Token.Kw_continue ->
      let t = p.cur in
      bump p;
      ignore (expect p Token.Semi);
      { snode = Continue; sloc = t.loc }
  | Token.Semi ->
      let t = p.cur in
      bump p;
      { snode = Empty; sloc = t.loc }
  | _ ->
      let e = parse_expr p in
      let semi = expect p Token.Semi in
      { snode = Expr e; sloc = span_from e.eloc semi.loc }

and parse_if (p : t) : Ast.stmt =
  let kw = expect p Token.Kw_if in
  ignore (expect p Token.L_paren);
  let cond = parse_expr p in
  ignore (expect p Token.R_paren);
  let then_s = parse_stmt p in
  let else_s =
    match accept p Token.Kw_else with
    | Some _ -> Some (parse_stmt p)
    | None -> None
  in
  let loc =
    match else_s with
    | None -> span_from kw.loc then_s.sloc
    | Some s -> span_from kw.loc s.sloc
  in
  { snode = If (cond, then_s, else_s); sloc = loc }

and parse_while (p : t) : Ast.stmt =
  let kw = expect p Token.Kw_while in
  ignore (expect p Token.L_paren);
  let cond = parse_expr p in
  ignore (expect p Token.R_paren);
  let body = parse_stmt p in
  { snode = While (cond, body); sloc = span_from kw.loc body.sloc }

and parse_return (p : t) : Ast.stmt =
  let kw = expect p Token.Kw_return in
  let expr_opt =
    match p.cur.kind with
    | Token.Semi -> None
    | _ -> Some (parse_expr p)
  in
  let semi = expect p Token.Semi in
  { snode = Return expr_opt; sloc = span_from kw.loc semi.loc }

and parse_expr (p : t) : Ast.expr = parse_assign p

and parse_assign (p : t) : Ast.expr =
  let lhs = parse_logic_or p in
  match accept p Token.Eq with
  | Some _eq_tok ->
      let rhs = parse_assign p in
      { enode = Assign (lhs, rhs); eloc = span_from lhs.eloc rhs.eloc }
  | None -> lhs

and parse_logic_or (p : t) : Ast.expr =
  let rec loop (acc : Ast.expr) : Ast.expr =
    match accept p Token.Pipe_pipe with
    | Some _ ->
        let rhs = parse_logic_and p in
        let loc = span_from acc.eloc rhs.eloc in
        let acc' = { enode = Binop (LOr, acc, rhs); eloc = loc } in
        loop acc'
    | None -> acc
  in
  let first = parse_logic_and p in
  loop first

and parse_logic_and (p : t) : Ast.expr =
  let rec loop (acc : Ast.expr) : Ast.expr =
    match accept p Token.Amp_amp with
    | Some _ ->
        let rhs = parse_equality p in
        let loc = span_from acc.eloc rhs.eloc in
        let acc' = { enode = Binop (LAnd, acc, rhs); eloc = loc } in
        loop acc'
    | None -> acc
  in
  let first = parse_equality p in
  loop first

and parse_equality (p : t) : Ast.expr =
  let rec loop (acc : Ast.expr) : Ast.expr =
    match p.cur.kind with
    | Token.Eq_eq ->
        bump p;
        let rhs = parse_rel p in
        let loc = span_from acc.eloc rhs.eloc in
        loop { enode = Binop (Eq, acc, rhs); eloc = loc }
    | Token.Bang_eq ->
        bump p;
        let rhs = parse_rel p in
        let loc = span_from acc.eloc rhs.eloc in
        loop { enode = Binop (Ne, acc, rhs); eloc = loc }
    | _ -> acc
  in
  let first = parse_rel p in
  loop first

and parse_rel (p : t) : Ast.expr =
  let rec loop (acc : Ast.expr) : Ast.expr =
    match p.cur.kind with
    | Token.Lt ->
        bump p;
        let rhs = parse_add p in
        let loc = span_from acc.eloc rhs.eloc in
        loop { enode = Binop (Lt, acc, rhs); eloc = loc }
    | Token.Lt_eq ->
        bump p;
        let rhs = parse_add p in
        let loc = span_from acc.eloc rhs.eloc in
        loop { enode = Binop (Le, acc, rhs); eloc = loc }
    | Token.Gt ->
        bump p;
        let rhs = parse_add p in
        let loc = span_from acc.eloc rhs.eloc in
        loop { enode = Binop (Gt, acc, rhs); eloc = loc }
    | Token.Gt_eq ->
        bump p;
        let rhs = parse_add p in
        let loc = span_from acc.eloc rhs.eloc in
        loop { enode = Binop (Ge, acc, rhs); eloc = loc }
    | _ -> acc
  in
  let first = parse_add p in
  loop first

and parse_add (p : t) : Ast.expr =
  let rec loop (acc : Ast.expr) : Ast.expr =
    match p.cur.kind with
    | Token.Plus ->
        bump p;
        let rhs = parse_mul p in
        let loc = span_from acc.eloc rhs.eloc in
        loop { enode = Binop (Add, acc, rhs); eloc = loc }
    | Token.Minus ->
        bump p;
        let rhs = parse_mul p in
        let loc = span_from acc.eloc rhs.eloc in
        loop { enode = Binop (Sub, acc, rhs); eloc = loc }
    | _ -> acc
  in
  let first = parse_mul p in
  loop first

and parse_mul (p : t) : Ast.expr =
  let rec loop (acc : Ast.expr) : Ast.expr =
    match p.cur.kind with
    | Token.Star ->
        bump p;
        let rhs = parse_unary p in
        let loc = span_from acc.eloc rhs.eloc in
        loop { enode = Binop (Mul, acc, rhs); eloc = loc }
    | Token.Slash ->
        bump p;
        let rhs = parse_unary p in
        let loc = span_from acc.eloc rhs.eloc in
        loop { enode = Binop (Div, acc, rhs); eloc = loc }
    | Token.Percent ->
        bump p;
        let rhs = parse_unary p in
        let loc = span_from acc.eloc rhs.eloc in
        loop { enode = Binop (Mod, acc, rhs); eloc = loc }
    | _ -> acc
  in
  let first = parse_unary p in
  loop first

and parse_unary (p : t) : Ast.expr =
  let start_loc = current_loc p in
  match p.cur.kind with
  | Token.Plus ->
      bump p;
      let e = parse_unary p in
      { enode = Unop (Pos, e); eloc = span_from start_loc e.eloc }
  | Token.Minus ->
      bump p;
      let e = parse_unary p in
      { enode = Unop (Neg, e); eloc = span_from start_loc e.eloc }
  | Token.Bang ->
      bump p;
      let e = parse_unary p in
      { enode = Unop (Not, e); eloc = span_from start_loc e.eloc }
  | Token.Amp ->
      bump p;
      let e = parse_unary p in
      { enode = Unop (AddrOf, e); eloc = span_from start_loc e.eloc }
  | Token.Star ->
      bump p;
      let e = parse_unary p in
      { enode = Unop (Deref, e); eloc = span_from start_loc e.eloc }
  | _ -> parse_postfix p

and parse_postfix (p : t) : Ast.expr =
  let rec loop (acc : Ast.expr) : Ast.expr =
    match p.cur.kind with
    | Token.Dot ->
        bump p;
        let field, floc = expect_ident p ~what:"expected member name after '.'" in
        let loc = span_from acc.eloc floc in
        loop { enode = Member (acc, field); eloc = loc }
    | Token.Arrow ->
        bump p;
        let field, floc = expect_ident p ~what:"expected member name after '->'" in
        let loc = span_from acc.eloc floc in
        loop { enode = PtrMember (acc, field); eloc = loc }
    | _ -> acc
  in
  loop (parse_primary p)

and parse_primary (p : t) : Ast.expr =
  match p.cur.kind with
  | Token.Int_lit n ->
      let t = p.cur in
      bump p;
      { enode = IntLit n; eloc = t.loc }
  | Token.Char_lit n ->
      let t = p.cur in
      bump p;
      { enode = CharLit n; eloc = t.loc }
  | Token.String_lit s ->
      let t = p.cur in
      bump p;
      { enode = StringLit s; eloc = t.loc }
  | Token.Ident s ->
      let id_tok = p.cur in
      bump p;
      let loc_start = id_tok.loc in
      let node, end_loc =
        match accept p Token.L_paren with
        | Some _ ->
            let args, rparen_loc =
              match p.cur.kind with
              | Token.R_paren ->
                  let rp = expect p Token.R_paren in
                  ([], rp.loc)
              | _ ->
                  let xs = parse_arg_list p in
                  let rp = expect p Token.R_paren in
                  (xs, rp.loc)
            in
            (Ast.Call (s, args), rparen_loc)
        | None -> (Ast.Var s, loc_start)
      in
      { enode = node; eloc = span_from loc_start end_loc }
  | Token.L_paren ->
      ignore (expect p Token.L_paren);
      let e = parse_expr p in
      ignore (expect p Token.R_paren);
      e
  | _ -> Util.error p.cur.loc "unexpected token in expression: %s" (Token.kind_to_string p.cur.kind)

and parse_arg_list (p : t) : Ast.expr list =
  let rec loop acc =
    let e = parse_expr p in
    match accept p Token.Comma with
    | Some _ -> loop (e :: acc)
    | None -> List.rev (e :: acc)
  in
  loop []
