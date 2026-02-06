open Typecheck

module IntMap = Map.Make (Int)

type var_info =
  { ty : Ast.ty
  ; addr : Llvm_ir.value
  }

type cg_ctx =
  { mutable next_str_id : int
  ; str_tbl : (string, string) Hashtbl.t
  ; mutable str_defs_rev : string list
  }

type env =
  { vars : var_info IntMap.t
  ; func_ret : Ast.ty
  ; mutable loop_stack : (string * string) list
  ; ctx : cg_ctx
  ; tag_layouts : (string, tag_layout) Hashtbl.t
  }

let string_of_tag_kind = function
  | Ast.Struct -> "struct"
  | Ast.Union -> "union"

let tag_key (kind : Ast.tag_kind) (name : string) : string =
  Printf.sprintf "%s:%s" (string_of_tag_kind kind) name

let is_aggregate_ty = function
  | Ast.TStruct _ | Ast.TUnion _ -> true
  | _ -> false

let is_integer_ty = function
  | Ast.TInt | Ast.TChar -> true
  | _ -> false

let create_ctx () : cg_ctx = { next_str_id = 0; str_tbl = Hashtbl.create 32; str_defs_rev = [] }

let escape_llvm_string (s : string) : string =
  let buf = Buffer.create ((String.length s * 2) + 4) in
  String.iter
    (fun c ->
      let code = Char.code c in
      if code >= 32 && code <= 126 && c <> '"' && c <> '\\' then
        Buffer.add_char buf c
      else
        Buffer.add_string buf (Printf.sprintf "\\%02X" code))
    s;
  Buffer.add_string buf "\\00";
  Buffer.contents buf

let intern_string_literal (ctx : cg_ctx) (s : string) : string =
  match Hashtbl.find_opt ctx.str_tbl s with
  | Some sym -> sym
  | None ->
      let id = ctx.next_str_id in
      ctx.next_str_id <- id + 1;
      let sym = Printf.sprintf "@.str.%d" id in
      let len = String.length s + 1 in
      let body = escape_llvm_string s in
      let def =
        Printf.sprintf "%s = private unnamed_addr constant [%d x i8] c\"%s\", align 1" sym len body
      in
      Hashtbl.add ctx.str_tbl s sym;
      ctx.str_defs_rev <- def :: ctx.str_defs_rev;
      sym

let llty_of_mini = function
  | Ast.TInt -> Llvm_ir.I32
  | Ast.TChar -> Llvm_ir.I8
  | Ast.TVoid -> Llvm_ir.Void
  | Ast.TPtr _ -> Llvm_ir.Ptr
  | Ast.TStruct _ | Ast.TUnion _ ->
      Util.error Loc.none "internal: aggregate value type in llty_of_mini"

let aggregate_layout (tag_layouts : (string, tag_layout) Hashtbl.t) (ty : Ast.ty) : tag_layout =
  match ty with
  | Ast.TStruct n -> (
      match Hashtbl.find_opt tag_layouts (tag_key Ast.Struct n) with
      | Some lay -> lay
      | None -> Util.error Loc.none "internal: unknown struct %s in codegen" n)
  | Ast.TUnion n -> (
      match Hashtbl.find_opt tag_layouts (tag_key Ast.Union n) with
      | Some lay -> lay
      | None -> Util.error Loc.none "internal: unknown union %s in codegen" n)
  | _ -> Util.error Loc.none "internal: aggregate_layout expects struct/union type"

let storage_ty_string (tag_layouts : (string, tag_layout) Hashtbl.t) (ty : Ast.ty) : string =
  if is_aggregate_ty ty then
    Printf.sprintf "[%d x i8]" (aggregate_layout tag_layouts ty).size
  else
    Llvm_ir.string_of_llty (llty_of_mini ty)

let sizeof_ty (tag_layouts : (string, tag_layout) Hashtbl.t) (ty : Ast.ty) : int =
  match ty with
  | Ast.TInt -> 4
  | Ast.TChar -> 1
  | Ast.TPtr _ -> 8
  | Ast.TStruct _ | Ast.TUnion _ -> (aggregate_layout tag_layouts ty).size
  | Ast.TVoid -> Util.error Loc.none "internal: void pointee in pointer arithmetic"

let bool_of_scalar (b : Llvm_ir.builder) (v : Llvm_ir.value) : Llvm_ir.value =
  match v.ty with
  | Llvm_ir.I1 -> v
  | Llvm_ir.I32 ->
      Llvm_ir.emit_icmp b "ne" Llvm_ir.I32 v { v = "0"; ty = Llvm_ir.I32 }
  | Llvm_ir.I64 ->
      Llvm_ir.emit_icmp b "ne" Llvm_ir.I64 v { v = "0"; ty = Llvm_ir.I64 }
  | Llvm_ir.I8 ->
      let v32 = Llvm_ir.emit_cast b "sext" Llvm_ir.I8 Llvm_ir.I32 v in
      Llvm_ir.emit_icmp b "ne" Llvm_ir.I32 v32 { v = "0"; ty = Llvm_ir.I32 }
  | Llvm_ir.Ptr -> Llvm_ir.emit_icmp b "ne" Llvm_ir.Ptr v { v = "null"; ty = Llvm_ir.Ptr }
  | Llvm_ir.Void -> Util.error Loc.none "internal: void value in boolean context"

let load_from_addr (b : Llvm_ir.builder) (ty : Ast.ty) (addr : Llvm_ir.value) : Llvm_ir.value =
  if is_aggregate_ty ty then Util.error Loc.none "internal: cannot load aggregate by value";
  let llty = llty_of_mini ty in
  let r = Llvm_ir.fresh_reg b in
  Llvm_ir.emit b (Printf.sprintf "%s = load %s, ptr %s" r (Llvm_ir.string_of_llty llty) addr.v);
  { Llvm_ir.v = r; ty = llty }

let store_to_addr (b : Llvm_ir.builder) (ty : Ast.ty) (v : Llvm_ir.value) (addr : Llvm_ir.value) : unit
    =
  if is_aggregate_ty ty then Util.error Loc.none "internal: cannot store aggregate by value";
  let llty = llty_of_mini ty in
  Llvm_ir.emit b (Printf.sprintf "store %s %s, ptr %s" (Llvm_ir.string_of_llty llty) v.v addr.v)

let rec gen_lvalue (b : Llvm_ir.builder) (env : env) (lv : tlvalue) : Llvm_ir.value * Ast.ty =
  match lv.lv_node with
  | LVar id -> (
      match IntMap.find_opt id env.vars with
      | None -> Util.error lv.lv_loc "unknown var id %d" id
      | Some info -> (info.addr, info.ty))
  | LDeref e ->
      let pv = gen_expr b env e in
      (pv, lv.lv_ty)
  | LMember (base, fld) ->
      let base_addr, _ = gen_lvalue b env base in
      let r = Llvm_ir.fresh_reg b in
      Llvm_ir.emit b (Printf.sprintf "%s = getelementptr i8, ptr %s, i32 %d" r base_addr.v fld.fld_offset);
      ({ Llvm_ir.v = r; ty = Llvm_ir.Ptr }, lv.lv_ty)

and gen_expr (b : Llvm_ir.builder) (env : env) (e : texpr) : Llvm_ir.value =
  match e.e_node with
  | EIntLit n -> { Llvm_ir.v = string_of_int n; ty = Llvm_ir.I32 }
  | ECharLit n -> { Llvm_ir.v = string_of_int n; ty = Llvm_ir.I8 }
  | EStringLit s -> { Llvm_ir.v = intern_string_literal env.ctx s; ty = Llvm_ir.Ptr }
  | EVar id -> (
      match IntMap.find_opt id env.vars with
      | None -> Util.error e.e_loc "unknown var id %d" id
      | Some info -> load_from_addr b info.ty info.addr)
  | ELvalue lv ->
      let addr, ty = gen_lvalue b env lv in
      load_from_addr b ty addr
  | ECall (name, args) ->
      let args_v = List.map (gen_expr b env) args in
      let arg_str =
        args_v
        |> List.map (fun (a : Llvm_ir.value) ->
               Printf.sprintf "%s %s" (Llvm_ir.string_of_llty a.ty) a.v)
        |> String.concat ", "
      in
      let ret_ll = llty_of_mini e.e_ty in
      if ret_ll = Llvm_ir.Void then (
        Llvm_ir.emit b (Printf.sprintf "call void @%s(%s)" name arg_str);
        { v = ""; ty = Llvm_ir.Void })
      else (
        let r = Llvm_ir.fresh_reg b in
        Llvm_ir.emit b
          (Printf.sprintf "%s = call %s @%s(%s)" r (Llvm_ir.string_of_llty ret_ll) name arg_str);
        { v = r; ty = ret_ll })
  | EAssign (lv, rhs) ->
      let addr, dst_ty = gen_lvalue b env lv in
      let rv = gen_expr b env rhs in
      store_to_addr b dst_ty rv addr;
      { rv with ty = llty_of_mini dst_ty }
  | EBinop (op, a, c) -> gen_binop b env e.e_loc op a c
  | EUnop (uop, a) -> gen_unop b env e.e_ty uop a
  | EAddrOf lv ->
      let addr, _ = gen_lvalue b env lv in
      { addr with ty = Llvm_ir.Ptr }
  | ECast (to_ty, inner) ->
      let v = gen_expr b env inner in
      gen_cast b inner.e_ty to_ty v

and gen_cast (b : Llvm_ir.builder) (from_ty : Ast.ty) (to_ty : Ast.ty) (v : Llvm_ir.value) :
    Llvm_ir.value =
  if Typecheck.equal_ty from_ty to_ty then v
  else
    match (from_ty, to_ty) with
    | Ast.TChar, Ast.TInt -> Llvm_ir.emit_cast b "sext" Llvm_ir.I8 Llvm_ir.I32 v
    | Ast.TInt, Ast.TChar -> Llvm_ir.emit_cast b "trunc" Llvm_ir.I32 Llvm_ir.I8 v
    | Ast.TPtr _, Ast.TPtr _ -> v
    | _ -> v

and gen_ptr_plus_int (b : Llvm_ir.builder) (tag_layouts : (string, tag_layout) Hashtbl.t)
    (ptr_ty : Ast.ty) (ptr_v : Llvm_ir.value) (idx_v : Llvm_ir.value) (is_sub : bool) : Llvm_ir.value =
  let pointee =
    match ptr_ty with
    | Ast.TPtr t -> t
    | _ -> Util.error Loc.none "internal: gen_ptr_plus_int requires pointer type"
  in
  let elem_size = sizeof_ty tag_layouts pointee in
  let idx64 =
    match idx_v.ty with
    | Llvm_ir.I64 -> idx_v
    | Llvm_ir.I32 -> Llvm_ir.emit_cast b "sext" Llvm_ir.I32 Llvm_ir.I64 idx_v
    | Llvm_ir.I8 -> Llvm_ir.emit_cast b "sext" Llvm_ir.I8 Llvm_ir.I64 idx_v
    | _ -> Util.error Loc.none "internal: pointer arithmetic index must be integer"
  in
  let scaled =
    if elem_size = 1 then idx64
    else
      Llvm_ir.emit_binop b "mul" Llvm_ir.I64 idx64
        { v = string_of_int elem_size; ty = Llvm_ir.I64 }
  in
  let base = Llvm_ir.emit_cast b "ptrtoint" Llvm_ir.Ptr Llvm_ir.I64 ptr_v in
  let out_i64 =
    Llvm_ir.emit_binop b (if is_sub then "sub" else "add") Llvm_ir.I64 base scaled
  in
  Llvm_ir.emit_cast b "inttoptr" Llvm_ir.I64 Llvm_ir.Ptr out_i64

and gen_ptr_minus_ptr (b : Llvm_ir.builder) (tag_layouts : (string, tag_layout) Hashtbl.t)
    (ptr_ty : Ast.ty) (a : Llvm_ir.value) (c : Llvm_ir.value) : Llvm_ir.value =
  let pointee =
    match ptr_ty with
    | Ast.TPtr t -> t
    | _ -> Util.error Loc.none "internal: gen_ptr_minus_ptr requires pointer type"
  in
  let elem_size = sizeof_ty tag_layouts pointee in
  let ai = Llvm_ir.emit_cast b "ptrtoint" Llvm_ir.Ptr Llvm_ir.I64 a in
  let ci = Llvm_ir.emit_cast b "ptrtoint" Llvm_ir.Ptr Llvm_ir.I64 c in
  let bytes = Llvm_ir.emit_binop b "sub" Llvm_ir.I64 ai ci in
  let elems =
    if elem_size = 1 then bytes
    else
      Llvm_ir.emit_binop b "sdiv" Llvm_ir.I64 bytes
        { v = string_of_int elem_size; ty = Llvm_ir.I64 }
  in
  Llvm_ir.emit_cast b "trunc" Llvm_ir.I64 Llvm_ir.I32 elems

and gen_binop (b : Llvm_ir.builder) (env : env) (_loc : Loc.t) (op : Ast.binop) (a : texpr)
    (c : texpr) : Llvm_ir.value =
  match op with
  | Ast.Add ->
      let va = gen_expr b env a in
      let vc = gen_expr b env c in
      if is_integer_ty a.e_ty && is_integer_ty c.e_ty then
        Llvm_ir.emit_binop b "add" Llvm_ir.I32 va vc
      else if va.ty = Llvm_ir.Ptr && vc.ty <> Llvm_ir.Ptr then
        gen_ptr_plus_int b env.tag_layouts a.e_ty va vc false
      else if va.ty <> Llvm_ir.Ptr && vc.ty = Llvm_ir.Ptr then
        gen_ptr_plus_int b env.tag_layouts c.e_ty vc va false
      else
        Util.error Loc.none "internal: unsupported add operands"
  | Ast.Sub ->
      let va = gen_expr b env a in
      let vc = gen_expr b env c in
      if is_integer_ty a.e_ty && is_integer_ty c.e_ty then
        Llvm_ir.emit_binop b "sub" Llvm_ir.I32 va vc
      else if va.ty = Llvm_ir.Ptr && vc.ty <> Llvm_ir.Ptr then
        gen_ptr_plus_int b env.tag_layouts a.e_ty va vc true
      else if va.ty = Llvm_ir.Ptr && vc.ty = Llvm_ir.Ptr then
        gen_ptr_minus_ptr b env.tag_layouts a.e_ty va vc
      else
        Util.error Loc.none "internal: unsupported sub operands"
  | Ast.Mul -> Llvm_ir.emit_binop b "mul" Llvm_ir.I32 (gen_expr b env a) (gen_expr b env c)
  | Ast.Div -> Llvm_ir.emit_binop b "sdiv" Llvm_ir.I32 (gen_expr b env a) (gen_expr b env c)
  | Ast.Mod -> Llvm_ir.emit_binop b "srem" Llvm_ir.I32 (gen_expr b env a) (gen_expr b env c)
  | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge | Ast.Eq | Ast.Ne ->
      let va = gen_expr b env a in
      let vc = gen_expr b env c in
      let pred =
        match op with
        | Ast.Lt -> "slt"
        | Ast.Le -> "sle"
        | Ast.Gt -> "sgt"
        | Ast.Ge -> "sge"
        | Ast.Eq -> "eq"
        | Ast.Ne -> "ne"
        | _ -> "eq"
      in
      let cmp =
        match (va.ty, vc.ty) with
        | Llvm_ir.Ptr, Llvm_ir.Ptr -> Llvm_ir.emit_icmp b pred Llvm_ir.Ptr va vc
        | _ -> Llvm_ir.emit_icmp b pred Llvm_ir.I32 va vc
      in
      Llvm_ir.emit_cast b "zext" Llvm_ir.I1 Llvm_ir.I32 cmp
  | Ast.LAnd -> gen_short_circuit b env true a c
  | Ast.LOr -> gen_short_circuit b env false a c

and gen_short_circuit (b : Llvm_ir.builder) (env : env) (is_and : bool) (a : texpr) (c : texpr) :
    Llvm_ir.value =
  let lhs_v = gen_expr b env a in
  let lhs_b = bool_of_scalar b lhs_v in
  let rhs_label = Llvm_ir.fresh_label b "sc.rhs" in
  let short_label = Llvm_ir.fresh_label b "sc.short" in
  let merge_label = Llvm_ir.fresh_label b "sc.merge" in
  if is_and then
    Llvm_ir.terminate b
      (Printf.sprintf "br i1 %s, label %%%s, label %%%s" lhs_b.v rhs_label short_label)
  else
    Llvm_ir.terminate b
      (Printf.sprintf "br i1 %s, label %%%s, label %%%s" lhs_b.v short_label rhs_label);
  Llvm_ir.start_block b rhs_label;
  let rhs_v = gen_expr b env c in
  let rhs_b = bool_of_scalar b rhs_v in
  Llvm_ir.terminate b (Printf.sprintf "br label %%%s" merge_label);
  let rhs_end_label = b.current.label in
  Llvm_ir.start_block b short_label;
  Llvm_ir.terminate b (Printf.sprintf "br label %%%s" merge_label);
  let short_end_label = b.current.label in
  Llvm_ir.start_block b merge_label;
  let r = Llvm_ir.fresh_reg b in
  let short_val = if is_and then "false" else "true" in
  Llvm_ir.emit b
    (Printf.sprintf "%s = phi i1 [ %s, %%%s ], [ %s, %%%s ]" r short_val short_end_label rhs_b.v
       rhs_end_label);
  Llvm_ir.emit_cast b "zext" Llvm_ir.I1 Llvm_ir.I32 { v = r; ty = Llvm_ir.I1 }

and gen_unop (b : Llvm_ir.builder) (env : env) (result_ty : Ast.ty) (uop : Ast.unop) (a : texpr) :
    Llvm_ir.value =
  match uop with
  | Ast.Pos -> gen_expr b env a
  | Ast.Neg ->
      let v = gen_expr b env a in
      Llvm_ir.emit_binop b "sub" Llvm_ir.I32 { v = "0"; ty = Llvm_ir.I32 } v
  | Ast.Not ->
      let v = gen_expr b env a in
      let vb = bool_of_scalar b v in
      let inv = Llvm_ir.emit_binop b "xor" Llvm_ir.I1 vb { v = "true"; ty = Llvm_ir.I1 } in
      Llvm_ir.emit_cast b "zext" Llvm_ir.I1 Llvm_ir.I32 inv
  | Ast.Deref ->
      let pv = gen_expr b env a in
      load_from_addr b result_ty pv
  | Ast.AddrOf -> Util.error Loc.none "internal: AddrOf should be EAddrOf"

let rec gen_stmt (b : Llvm_ir.builder) (env : env) (s : tstmt) : unit =
  match s.st_node with
  | SEmpty -> ()
  | SExpr e ->
      ignore (gen_expr b env e);
      ()
  | SReturn eo ->
      let ret_ll = llty_of_mini env.func_ret in
      let instr =
        match eo with
        | None -> "ret void"
        | Some e ->
            let v = gen_expr b env e in
            Printf.sprintf "ret %s %s" (Llvm_ir.string_of_llty ret_ll) v.v
      in
      Llvm_ir.terminate b instr
  | SBreak -> (
      match env.loop_stack with
      | (break_label, _) :: _ -> Llvm_ir.terminate b (Printf.sprintf "br label %%%s" break_label)
      | [] -> Util.error s.st_loc "break outside loop")
  | SContinue -> (
      match env.loop_stack with
      | (_, cont_label) :: _ -> Llvm_ir.terminate b (Printf.sprintf "br label %%%s" cont_label)
      | [] -> Util.error s.st_loc "continue outside loop")
  | SBlock items -> List.iter (gen_item b env) items
  | SIf (cond, then_s, else_s) ->
      let cval = bool_of_scalar b (gen_expr b env cond) in
      let then_label = Llvm_ir.fresh_label b "if.then" in
      let else_label = Llvm_ir.fresh_label b "if.else" in
      let merge_label = Llvm_ir.fresh_label b "if.end" in
      Llvm_ir.terminate b
        (Printf.sprintf "br i1 %s, label %%%s, label %%%s" cval.v then_label else_label);
      Llvm_ir.start_block b then_label;
      gen_stmt b env then_s;
      if not b.current.terminated then
        Llvm_ir.terminate b (Printf.sprintf "br label %%%s" merge_label);
      Llvm_ir.start_block b else_label;
      (match else_s with
      | None -> ()
      | Some s2 -> gen_stmt b env s2);
      if not b.current.terminated then
        Llvm_ir.terminate b (Printf.sprintf "br label %%%s" merge_label);
      Llvm_ir.start_block b merge_label
  | SWhile (cond, body) ->
      let cond_label = Llvm_ir.fresh_label b "while.cond" in
      let body_label = Llvm_ir.fresh_label b "while.body" in
      let end_label = Llvm_ir.fresh_label b "while.end" in
      Llvm_ir.terminate b (Printf.sprintf "br label %%%s" cond_label);
      Llvm_ir.start_block b cond_label;
      let cval = bool_of_scalar b (gen_expr b env cond) in
      Llvm_ir.terminate b
        (Printf.sprintf "br i1 %s, label %%%s, label %%%s" cval.v body_label end_label);
      env.loop_stack <- (end_label, cond_label) :: env.loop_stack;
      Llvm_ir.start_block b body_label;
      gen_stmt b env body;
      if not b.current.terminated then
        Llvm_ir.terminate b (Printf.sprintf "br label %%%s" cond_label);
      env.loop_stack <- List.tl env.loop_stack;
      Llvm_ir.start_block b end_label

and gen_item (b : Llvm_ir.builder) (env : env) (item : tdecl_or_stmt) : unit =
  match item with
  | DStmt s -> gen_stmt b env s
  | DDecl (id, ty, _name, init_opt, _loc) -> (
      match init_opt with
      | None -> ()
      | Some e ->
          let v = gen_expr b env e in
          let addr =
            match IntMap.find_opt id env.vars with
            | None -> Util.error e.e_loc "unknown var id %d" id
            | Some info -> info.addr
          in
          store_to_addr b ty v addr)

let declare_builtins () : string =
  String.concat "\n"
    [ "declare i32 @putchar(i32)"
    ; "declare i32 @getchar()"
    ; "declare ptr @malloc(i32)"
    ; "declare void @free(ptr)"
    ]

let gen_function (tag_layouts : (string, tag_layout) Hashtbl.t) (ctx : cg_ctx) (f : tfunc) : string =
  let b = Llvm_ir.create_builder () in
  let vars =
    List.fold_left
      (fun m (id, ty, _name) ->
        let addr_reg = Llvm_ir.fresh_reg b in
        if is_aggregate_ty ty then (
          let lay = aggregate_layout tag_layouts ty in
          Llvm_ir.emit b
            (Printf.sprintf "%s = alloca %s, align %d" addr_reg
               (storage_ty_string tag_layouts ty) lay.align))
        else
          Llvm_ir.emit b (Printf.sprintf "%s = alloca %s" addr_reg (storage_ty_string tag_layouts ty));
        IntMap.add id { ty; addr = { v = addr_reg; ty = Llvm_ir.Ptr } } m)
      IntMap.empty f.locals
  in
  List.iter
    (fun (p : tparam) ->
      let llty = llty_of_mini p.pty in
      let addr = (IntMap.find p.pid vars).addr in
      Llvm_ir.emit b
        (Printf.sprintf "store %s %%%s, ptr %s" (Llvm_ir.string_of_llty llty) p.pname addr.v))
    f.params;
  let env = { vars; func_ret = f.ret_ty; loop_stack = []; ctx; tag_layouts } in
  gen_stmt b env f.body;
  if not b.current.terminated then (
    match f.ret_ty with
    | Ast.TVoid -> Llvm_ir.terminate b "ret void"
    | Ast.TChar -> Llvm_ir.terminate b "ret i8 0"
    | Ast.TInt -> Llvm_ir.terminate b "ret i32 0"
    | Ast.TPtr _ -> Llvm_ir.terminate b "ret ptr null"
    | Ast.TStruct _ | Ast.TUnion _ ->
        Util.error Loc.none "internal: aggregate return should be rejected in typecheck");
  let params_sig =
    f.params
    |> List.map (fun p ->
           Printf.sprintf "%s %%%s" (Llvm_ir.string_of_llty (llty_of_mini p.pty)) p.pname)
    |> String.concat ", "
  in
  let header =
    Printf.sprintf "define %s @%s(%s) {\n" (Llvm_ir.string_of_llty (llty_of_mini f.ret_ty)) f.name
      params_sig
  in
  header ^ Llvm_ir.pp_blocks b ^ "}\n"

let gen_program (prog : tprogram) : string =
  let tag_layouts = Hashtbl.create 32 in
  List.iter
    (fun (t : tag_layout) -> Hashtbl.replace tag_layouts (tag_key t.tag_kind t.tag_name) t)
    prog.tags;
  let ctx = create_ctx () in
  let funcs = prog.funcs |> List.map (gen_function tag_layouts ctx) |> String.concat "\n" in
  let str_defs = ctx.str_defs_rev |> List.rev |> String.concat "\n" in
  let sections = [ declare_builtins (); str_defs; funcs ] |> List.filter (fun s -> s <> "") in
  String.concat "\n\n" sections
