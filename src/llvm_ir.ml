type llty =
  | I1
  | I8
  | I32
  | I64
  | Void
  | Ptr

let string_of_llty = function
  | I1 -> "i1"
  | I8 -> "i8"
  | I32 -> "i32"
  | I64 -> "i64"
  | Void -> "void"
  | Ptr -> "ptr"

type value =
  { v : string
  ; ty : llty
  }

type block =
  { label : string
  ; mutable insns : string list
  ; mutable terminated : bool
  }

type builder =
  { mutable reg : int
  ; mutable next_label : int
  ; mutable blocks : block list
  ; mutable current : block
  }

let create_builder () : builder =
  let entry = { label = "entry"; insns = []; terminated = false } in
  { reg = 0; next_label = 0; blocks = [ entry ]; current = entry }

let fresh_reg (b : builder) : string =
  let n = b.reg in
  b.reg <- n + 1;
  Printf.sprintf "%%t%d" n

let fresh_label (b : builder) (prefix : string) : string =
  let n = b.next_label in
  b.next_label <- n + 1;
  Printf.sprintf "%s.%d" prefix n

let start_block (b : builder) (label : string) : unit =
  let blk = { label; insns = []; terminated = false } in
  b.blocks <- b.blocks @ [ blk ];
  b.current <- blk

let emit (b : builder) (s : string) : unit =
  if b.current.terminated then () else b.current.insns <- b.current.insns @ [ s ]

let terminate (b : builder) (s : string) : unit =
  if b.current.terminated then ()
  else (
    b.current.insns <- b.current.insns @ [ s ];
    b.current.terminated <- true)

let emit_assign (b : builder) (ty : llty) (rhs : string) : value =
  let r = fresh_reg b in
  emit b (Printf.sprintf "%s = %s %s" r (string_of_llty ty) rhs);
  { v = r; ty }

let emit_binop (b : builder) (op : string) (ty : llty) (a : value) (c : value) : value =
  let r = fresh_reg b in
  emit b (Printf.sprintf "%s = %s %s %s, %s" r op (string_of_llty ty) a.v c.v);
  { v = r; ty }

let emit_icmp (b : builder) (pred : string) (ty : llty) (a : value) (c : value) : value =
  let r = fresh_reg b in
  emit b (Printf.sprintf "%s = icmp %s %s %s, %s" r pred (string_of_llty ty) a.v c.v);
  { v = r; ty = I1 }

let emit_cast (b : builder) (op : string) (from_ty : llty) (to_ty : llty) (v : value) : value =
  let r = fresh_reg b in
  emit b (Printf.sprintf "%s = %s %s %s to %s" r op (string_of_llty from_ty) v.v (string_of_llty to_ty));
  { v = r; ty = to_ty }

let pp_blocks (b : builder) : string =
  let buf = Buffer.create 4096 in
  List.iter
    (fun blk ->
      Buffer.add_string buf (Printf.sprintf "%s:\n" blk.label);
      List.iter (fun ins -> Buffer.add_string buf (Printf.sprintf "  %s\n" ins)) blk.insns)
    b.blocks;
  Buffer.contents buf
