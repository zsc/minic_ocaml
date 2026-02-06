module IntSet = Set.Make (Int)

type ty = Ast.ty
type var_id = int

type func_sig =
  { ret_ty : ty
  ; params : ty list
  }

type texpr =
  { e_node : texpr_node
  ; e_ty : ty
  ; e_loc : Loc.t
  }

and texpr_node =
  | EIntLit of int
  | EVar of var_id
  | ECall of string * texpr list
  | EAssign of tlvalue * texpr
  | EBinop of Ast.binop * texpr * texpr
  | EUnop of Ast.unop * texpr
  | EAddrOf of tlvalue
  | ECast of ty * texpr

and tlvalue =
  { lv_node : lvalue_node
  ; lv_ty : ty
  ; lv_loc : Loc.t
  }

and lvalue_node =
  | LVar of var_id
  | LDeref of texpr

type tstmt =
  { st_node : tstmt_node
  ; st_loc : Loc.t
  }

and tstmt_node =
  | SBlock of tdecl_or_stmt list
  | SIf of texpr * tstmt * tstmt option
  | SWhile of texpr * tstmt
  | SReturn of texpr option
  | SBreak
  | SContinue
  | SExpr of texpr
  | SEmpty

and tdecl_or_stmt =
  | DDecl of var_id * ty * string * texpr option * Loc.t
  | DStmt of tstmt

type tparam =
  { pid : var_id
  ; pty : ty
  ; pname : string
  ; ploc : Loc.t
  }

type tfunc =
  { ret_ty : ty
  ; name : string
  ; params : tparam list
  ; body : tstmt
  ; locals : (var_id * ty * string) list
  ; loc : Loc.t
  }

type tprogram = tfunc list

type env =
  { funcs : (string, func_sig) Hashtbl.t
  ; mutable next_var : int
  }

type var_sym =
  { vid : var_id
  ; vty : ty
  ; vname : string
  ; vloc : Loc.t
  }

type venv = (string, var_sym) Hashtbl.t list

let push_scope (st : venv) : venv = Hashtbl.create 16 :: st

let add_var (st : venv) (sym : var_sym) : unit =
  match st with
  | [] -> assert false
  | tbl :: _ ->
      if Hashtbl.mem tbl sym.vname then Util.error sym.vloc "duplicate declaration of %s" sym.vname;
      Hashtbl.add tbl sym.vname sym

let find_var (st : venv) (name : string) : var_sym option =
  let rec go = function
    | [] -> None
    | tbl :: tl -> (
        match Hashtbl.find_opt tbl name with
        | Some v -> Some v
        | None -> go tl)
  in
  go st

let rec equal_ty (a : ty) (b : ty) : bool =
  match (a, b) with
  | Ast.TInt, Ast.TInt -> true
  | Ast.TChar, Ast.TChar -> true
  | Ast.TVoid, Ast.TVoid -> true
  | Ast.TPtr x, Ast.TPtr y -> equal_ty x y
  | _ -> false

let rec string_of_ty = function
  | Ast.TInt -> "int"
  | Ast.TChar -> "char"
  | Ast.TVoid -> "void"
  | Ast.TPtr t -> Printf.sprintf "%s*" (string_of_ty t)

let is_integer = function
  | Ast.TInt | Ast.TChar -> true
  | _ -> false

let is_scalar = function
  | Ast.TInt | Ast.TChar -> true
  | Ast.TPtr _ -> true
  | Ast.TVoid -> false

let can_convert ~(dst : ty) ~(src : ty) : bool =
  if equal_ty dst src then true
  else
    match (dst, src) with
    | Ast.TInt, Ast.TChar -> true
    | Ast.TChar, Ast.TInt -> true
    | Ast.TPtr d, Ast.TPtr s -> equal_ty d s || equal_ty d Ast.TVoid || equal_ty s Ast.TVoid
    | _ -> false

let check_convert ~(dst : ty) ~(src : ty) ~(loc : Loc.t) : unit =
  if not (can_convert ~dst ~src) then
    Util.error loc "cannot convert %s to %s" (string_of_ty src) (string_of_ty dst)

let cast_expr ~(dst : ty) (e : texpr) : texpr =
  if equal_ty dst e.e_ty then e else { e_node = ECast (dst, e); e_ty = dst; e_loc = e.e_loc }

let promote_int (e : texpr) : texpr =
  match e.e_ty with
  | Ast.TInt -> e
  | Ast.TChar -> cast_expr ~dst:Ast.TInt e
  | _ -> Util.error e.e_loc "expected integer expression"

type stmt_result =
  { inits : IntSet.t
  ; falls_through : bool
  }

let merge_inits (a : stmt_result) (b : stmt_result) : IntSet.t =
  match (a.falls_through, b.falls_through) with
  | false, false -> IntSet.empty
  | true, false -> a.inits
  | false, true -> b.inits
  | true, true -> IntSet.inter a.inits b.inits

let builtin_funcs () : (string * func_sig) list =
  [ ("putchar", { ret_ty = Ast.TInt; params = [ Ast.TInt ] })
  ; ("getchar", { ret_ty = Ast.TInt; params = [] })
  ; ("malloc", { ret_ty = Ast.TPtr Ast.TVoid; params = [ Ast.TInt ] })
  ; ("free", { ret_ty = Ast.TVoid; params = [ Ast.TPtr Ast.TVoid ] })
  ]

let build_func_env (prog : Ast.program) : env =
  let funcs = Hashtbl.create 32 in
  List.iter (fun (n, s) -> Hashtbl.add funcs n s) (builtin_funcs ());
  List.iter
    (fun (f : Ast.func) ->
      if Hashtbl.mem funcs f.name then Util.error f.loc "duplicate function %s" f.name;
      let param_tys = List.map (fun (ty, _, _) -> ty) f.params in
      Hashtbl.add funcs f.name { ret_ty = f.ret_ty; params = param_tys })
    prog;
  { funcs; next_var = 0 }

let fresh_var (env : env) : var_id =
  let id = env.next_var in
  env.next_var <- env.next_var + 1;
  id

let rec tc_expr (env : env) (venv : venv) (inits : IntSet.t) (e : Ast.expr) :
    texpr * IntSet.t =
  let loc = e.eloc in
  match e.enode with
  | Ast.IntLit n -> ({ e_node = EIntLit n; e_ty = Ast.TInt; e_loc = loc }, inits)
  | Ast.Var name -> (
      match find_var venv name with
      | None -> Util.error loc "undeclared identifier %s" name
      | Some sym ->
          if not (IntSet.mem sym.vid inits) then Util.error loc "use of uninitialized variable %s" name;
          ({ e_node = EVar sym.vid; e_ty = sym.vty; e_loc = loc }, inits))
  | Ast.Call (fname, args) -> (
      match Hashtbl.find_opt env.funcs fname with
      | None -> Util.error loc "call to unknown function %s" fname
      | Some fsig ->
          if List.length args <> List.length fsig.params then
            Util.error loc "function %s expects %d args, got %d" fname (List.length fsig.params)
              (List.length args);
          let targs, inits' = tc_args env venv inits args fsig.params in
          ({ e_node = ECall (fname, targs); e_ty = fsig.ret_ty; e_loc = loc }, inits'))
  | Ast.Assign (lhs, rhs) ->
      let tlv, inits1 = tc_lvalue env venv inits lhs in
      let trhs, inits2 = tc_expr env venv inits1 rhs in
      check_convert ~dst:tlv.lv_ty ~src:trhs.e_ty ~loc:trhs.e_loc;
      let trhs' = cast_expr ~dst:tlv.lv_ty trhs in
      let inits3 =
        match tlv.lv_node with
        | LVar id -> IntSet.add id inits2
        | LDeref _ -> inits2
      in
      ( { e_node = EAssign (tlv, trhs'); e_ty = tlv.lv_ty; e_loc = loc }
      , inits3 )
  | Ast.Binop (op, a, b) -> tc_binop env venv inits loc op a b
  | Ast.Unop (Ast.AddrOf, inner) ->
      let lv, inits' = tc_lvalue env venv inits inner in
      ({ e_node = EAddrOf lv; e_ty = Ast.TPtr lv.lv_ty; e_loc = loc }, inits')
  | Ast.Unop (uop, inner) ->
      let te, inits' = tc_expr env venv inits inner in
      tc_unop env venv inits' loc uop te

and tc_args (env : env) (venv : venv) (inits : IntSet.t) (args : Ast.expr list)
    (param_tys : ty list) : texpr list * IntSet.t =
  let rec loop acc inits args param_tys =
    match (args, param_tys) with
    | [], [] -> (List.rev acc, inits)
    | a :: args_tl, pty :: pty_tl ->
        let ta, inits' = tc_expr env venv inits a in
        check_convert ~dst:pty ~src:ta.e_ty ~loc:ta.e_loc;
        let ta' = cast_expr ~dst:pty ta in
        loop (ta' :: acc) inits' args_tl pty_tl
    | _ -> assert false
  in
  loop [] inits args param_tys

and tc_lvalue (env : env) (venv : venv) (inits : IntSet.t) (e : Ast.expr) :
    tlvalue * IntSet.t =
  let loc = e.eloc in
  match e.enode with
  | Ast.Var name -> (
      match find_var venv name with
      | None -> Util.error loc "undeclared identifier %s" name
      | Some sym ->
          ({ lv_node = LVar sym.vid; lv_ty = sym.vty; lv_loc = loc }, inits))
  | Ast.Unop (Ast.Deref, inner) ->
      let te, inits' = tc_expr env venv inits inner in
      let pointee =
        match te.e_ty with
        | Ast.TPtr t -> t
        | _ -> Util.error loc "cannot dereference non-pointer type %s" (string_of_ty te.e_ty)
      in
      if equal_ty pointee Ast.TVoid then Util.error loc "cannot dereference void*";
      ({ lv_node = LDeref te; lv_ty = pointee; lv_loc = loc }, inits')
  | _ -> Util.error loc "expected lvalue"

and tc_binop (env : env) (venv : venv) (inits : IntSet.t) (loc : Loc.t) (op : Ast.binop)
    (a : Ast.expr) (b : Ast.expr) : texpr * IntSet.t =
  let ta, inits1 = tc_expr env venv inits a in
  let tb, inits2 = tc_expr env venv inits1 b in
  match op with
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Mod ->
      if not (is_integer ta.e_ty && is_integer tb.e_ty) then Util.error loc "arithmetic requires int/char";
      let ta' = promote_int ta in
      let tb' = promote_int tb in
      ({ e_node = EBinop (op, ta', tb'); e_ty = Ast.TInt; e_loc = loc }, inits2)
  | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge ->
      if not (is_integer ta.e_ty && is_integer tb.e_ty) then
        Util.error loc "comparison requires int/char";
      let ta' = promote_int ta in
      let tb' = promote_int tb in
      ({ e_node = EBinop (op, ta', tb'); e_ty = Ast.TInt; e_loc = loc }, inits2)
  | Ast.Eq | Ast.Ne -> (
      match (ta.e_ty, tb.e_ty) with
      | Ast.TPtr _, Ast.TPtr _ ->
          ({ e_node = EBinop (op, ta, tb); e_ty = Ast.TInt; e_loc = loc }, inits2)
      | _ ->
          if not (is_integer ta.e_ty && is_integer tb.e_ty) then
            Util.error loc "equality requires both operands be int/char or pointers";
          let ta' = promote_int ta in
          let tb' = promote_int tb in
          ({ e_node = EBinop (op, ta', tb'); e_ty = Ast.TInt; e_loc = loc }, inits2))
  | Ast.LAnd | Ast.LOr ->
      if not (is_scalar ta.e_ty && is_scalar tb.e_ty) then Util.error loc "&&/|| requires scalar operands";
      let inits_out = IntSet.inter inits1 inits2 in
      ({ e_node = EBinop (op, ta, tb); e_ty = Ast.TInt; e_loc = loc }, inits_out)

and tc_unop (_env : env) (_venv : venv) (inits : IntSet.t) (loc : Loc.t) (uop : Ast.unop)
    (te : texpr) : texpr * IntSet.t =
  match uop with
  | Ast.Pos ->
      if not (is_integer te.e_ty) then Util.error loc "unary + requires int/char";
      let te' = promote_int te in
      ({ e_node = EUnop (Ast.Pos, te'); e_ty = Ast.TInt; e_loc = loc }, inits)
  | Ast.Neg ->
      if not (is_integer te.e_ty) then Util.error loc "unary - requires int/char";
      let te' = promote_int te in
      ({ e_node = EUnop (Ast.Neg, te'); e_ty = Ast.TInt; e_loc = loc }, inits)
  | Ast.Not ->
      if not (is_scalar te.e_ty) then Util.error loc "unary ! requires scalar";
      ({ e_node = EUnop (Ast.Not, te); e_ty = Ast.TInt; e_loc = loc }, inits)
  | Ast.Deref ->
      let pointee =
        match te.e_ty with
        | Ast.TPtr t -> t
        | _ -> Util.error loc "cannot dereference non-pointer type %s" (string_of_ty te.e_ty)
      in
      if equal_ty pointee Ast.TVoid then Util.error loc "cannot dereference void*";
      ({ e_node = EUnop (Ast.Deref, te); e_ty = pointee; e_loc = loc }, inits)
  | Ast.AddrOf -> assert false

let rec tc_stmt (env : env) (venv : venv) (inits : IntSet.t) ~(in_loop : bool) ~(ret_ty : ty)
    ~(locals_rev : (var_id * ty * string) list ref) (s : Ast.stmt) : tstmt * stmt_result =
  let loc = s.sloc in
  match s.snode with
  | Ast.Empty -> ({ st_node = SEmpty; st_loc = loc }, { inits; falls_through = true })
  | Ast.Expr e ->
      let te, inits' = tc_expr env venv inits e in
      ({ st_node = SExpr te; st_loc = loc }, { inits = inits'; falls_through = true })
  | Ast.Return eo -> (
      match (ret_ty, eo) with
      | Ast.TVoid, None -> ({ st_node = SReturn None; st_loc = loc }, { inits; falls_through = false })
      | Ast.TVoid, Some _ -> Util.error loc "void function cannot return a value"
      | _, None -> Util.error loc "non-void function must return a value"
      | _, Some e ->
          let te, _ = tc_expr env venv inits e in
          check_convert ~dst:ret_ty ~src:te.e_ty ~loc:te.e_loc;
          let te' = cast_expr ~dst:ret_ty te in
          ( { st_node = SReturn (Some te'); st_loc = loc }
          , { inits; falls_through = false } ))
  | Ast.Break ->
      if not in_loop then Util.error loc "break not in loop";
      ({ st_node = SBreak; st_loc = loc }, { inits; falls_through = false })
  | Ast.Continue ->
      if not in_loop then Util.error loc "continue not in loop";
      ({ st_node = SContinue; st_loc = loc }, { inits; falls_through = false })
  | Ast.Block items ->
      let venv' = push_scope venv in
      let titems, res = tc_block_items env venv' inits ~in_loop ~ret_ty ~locals_rev items in
      ({ st_node = SBlock titems; st_loc = loc }, res)
  | Ast.If (cond, then_s, else_s) ->
      let tcond, inits_cond = tc_expr env venv inits cond in
      if not (is_scalar tcond.e_ty) then Util.error tcond.e_loc "if condition must be scalar";
      let tthen, r_then =
        tc_stmt env venv inits_cond ~in_loop ~ret_ty ~locals_rev then_s
      in
      let telse_opt, r_else =
        match else_s with
        | None -> (None, { inits = inits_cond; falls_through = true })
        | Some s2 ->
            let ts2, r2 = tc_stmt env venv inits_cond ~in_loop ~ret_ty ~locals_rev s2 in
            (Some ts2, r2)
      in
      let inits_after = merge_inits r_then r_else in
      let falls = r_then.falls_through || r_else.falls_through in
      ( { st_node = SIf (tcond, tthen, telse_opt); st_loc = loc }
      , { inits = inits_after; falls_through = falls } )
  | Ast.While (cond, body) ->
      let tcond, inits_cond = tc_expr env venv inits cond in
      if not (is_scalar tcond.e_ty) then Util.error tcond.e_loc "while condition must be scalar";
      let tbody, _ = tc_stmt env venv inits_cond ~in_loop:true ~ret_ty ~locals_rev body in
      ( { st_node = SWhile (tcond, tbody); st_loc = loc }
      , { inits = inits_cond; falls_through = true } )

and tc_block_items (env : env) (venv : venv) (inits : IntSet.t) ~(in_loop : bool) ~(ret_ty : ty)
    ~(locals_rev : (var_id * ty * string) list ref) (items : Ast.decl_or_stmt list) :
    tdecl_or_stmt list * stmt_result =
  let rec loop acc inits falls items =
    match items with
    | [] -> (List.rev acc, { inits; falls_through = falls })
    | _ when not falls -> (List.rev acc, { inits; falls_through = false })
    | item :: tl -> (
        match item with
        | Ast.Stmt s ->
            let ts, r = tc_stmt env venv inits ~in_loop ~ret_ty ~locals_rev s in
            loop (DStmt ts :: acc) r.inits r.falls_through tl
        | Ast.Decl (vty, name, init_opt, dloc) ->
            if equal_ty vty Ast.TVoid then Util.error dloc "cannot declare variable of type void";
            let id = fresh_var env in
            let sym = { vid = id; vty; vname = name; vloc = dloc } in
            add_var venv sym;
            locals_rev := (id, vty, name) :: !locals_rev;
            let tinit_opt, inits_after_init =
              match init_opt with
              | None -> (None, inits)
              | Some e ->
                  let te, inits' = tc_expr env venv inits e in
                  check_convert ~dst:vty ~src:te.e_ty ~loc:te.e_loc;
                  (Some (cast_expr ~dst:vty te), inits')
            in
            let inits' =
              match tinit_opt with
              | None -> inits_after_init
              | Some _ -> IntSet.add id inits_after_init
            in
            loop (DDecl (id, vty, name, tinit_opt, dloc) :: acc) inits' true tl)
  in
  loop [] inits true items

let tc_function (env : env) (f : Ast.func) : tfunc =
  let venv0 = [ Hashtbl.create 32 ] in
  let locals_rev = ref [] in
  let params =
    let seen = Hashtbl.create 16 in
    List.map
      (fun (pty, pname, ploc) ->
        if equal_ty pty Ast.TVoid then Util.error ploc "parameter %s cannot have type void" pname;
        if Hashtbl.mem seen pname then Util.error ploc "duplicate parameter name %s" pname;
        Hashtbl.add seen pname ();
        let pid = fresh_var env in
        let sym = { vid = pid; vty = pty; vname = pname; vloc = ploc } in
        add_var venv0 sym;
        locals_rev := (pid, pty, pname) :: !locals_rev;
        { pid; pty; pname; ploc })
      f.params
  in
  let init0 =
    List.fold_left (fun s p -> IntSet.add p.pid s) IntSet.empty params
  in
  let body, res =
    tc_stmt env venv0 init0 ~in_loop:false ~ret_ty:f.ret_ty ~locals_rev f.body
  in
  if not (equal_ty f.ret_ty Ast.TVoid) && res.falls_through then Util.error f.loc "missing return value";
  let locals = List.rev !locals_rev in
  { ret_ty = f.ret_ty; name = f.name; params; body; locals; loc = f.loc }

let typecheck (prog : Ast.program) : tprogram =
  let env = build_func_env prog in
  List.map (tc_function env) prog

