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

let rec parse_program (p : t) : Ast.program =
  let rec loop acc =
    match p.cur.kind with
    | Token.Eof -> List.rev acc
    | _ ->
        let f = parse_function p in
        loop (f :: acc)
  in
  loop []

and parse_function (p : t) : Ast.func =
  let start_loc = current_loc p in
  let ret_ty = parse_type p in
  let name =
    match p.cur.kind with
    | Token.Ident s ->
        bump p;
        s
    | _ -> Util.error p.cur.loc "expected function name, got %s" (Token.kind_to_string p.cur.kind)
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
  match p.cur.kind with
  | Token.Ident s ->
      let loc = p.cur.loc in
      bump p;
      (ty, s, loc)
  | _ -> Util.error p.cur.loc "expected parameter name"

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
    | _ -> Util.error p.cur.loc "expected type, got %s" (Token.kind_to_string p.cur.kind)
  in
  let rec stars ty =
    match accept p Token.Star with
    | Some _ -> stars (Ast.TPtr ty)
    | None -> ty
  in
  stars base

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
  | Token.Kw_int | Token.Kw_char | Token.Kw_void ->
      let start_loc = current_loc p in
      let ty = parse_type p in
      let name =
        match p.cur.kind with
        | Token.Ident s ->
            bump p;
            s
        | _ -> Util.error p.cur.loc "expected identifier in declaration"
      in
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
  | _ -> parse_primary p

and parse_primary (p : t) : Ast.expr =
  match p.cur.kind with
  | Token.Int_lit n ->
      let t = p.cur in
      bump p;
      { enode = IntLit n; eloc = t.loc }
  | Token.Ident s ->
      let id_tok = p.cur in
      bump p;
      let loc_start = id_tok.loc in
      let node =
        match accept p Token.L_paren with
        | Some _ ->
            let args =
              match p.cur.kind with
              | Token.R_paren ->
                  bump p;
                  []
              | _ ->
                  let args = parse_arg_list p in
                  ignore (expect p Token.R_paren);
                  args
            in
            Ast.Call (s, args)
        | None -> Ast.Var s
      in
      let loc =
        match node with
        | Ast.Call (_, args) -> (
            match List.rev args with
            | [] -> loc_start
            | last :: _ -> span_from loc_start last.eloc)
        | _ -> loc_start
      in
      { enode = node; eloc = loc }
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
