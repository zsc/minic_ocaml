type ty =
  | TInt
  | TChar
  | TVoid
  | TPtr of ty
  | TStruct of string
  | TUnion of string

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | LAnd
  | LOr

type unop =
  | Pos
  | Neg
  | Not
  | AddrOf
  | Deref

type expr =
  { enode : expr_node
  ; eloc : Loc.t
  }

and expr_node =
  | IntLit of int
  | Var of string
  | Call of string * expr list
  | Assign of expr * expr
  | Binop of binop * expr * expr
  | Unop of unop * expr
  | Member of expr * string
  | PtrMember of expr * string

type stmt =
  { snode : stmt_node
  ; sloc : Loc.t
  }

and stmt_node =
  | Block of decl_or_stmt list
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | Return of expr option
  | Break
  | Continue
  | Expr of expr
  | Empty

and decl_or_stmt =
  | Decl of ty * string * expr option * Loc.t
  | Stmt of stmt

type param = ty * string * Loc.t
type field = ty * string * Loc.t

type tag_kind =
  | Struct
  | Union

type func =
  { ret_ty : ty
  ; name : string
  ; params : param list
  ; body : stmt
  ; loc : Loc.t
  }

type type_def =
  { tag_kind : tag_kind
  ; tag_name : string
  ; fields : field list
  ; loc : Loc.t
  }

type top =
  | TopFunc of func
  | TopTypeDef of type_def

type program = top list
