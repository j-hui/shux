
(* everything begins with Freud *)
type id = string list

type mut =
  | Mutable | Immutable

type fn_typ =
  | Kn | Gn

type typ =
  | Int
  | Float
  | String
  | Bool
  | Struct of id (* struct identifier *)
  | Array of typ * int option (* TODO: see if the size is still strictly necessary *)
  | Ptr
  | Void

type bind = Bind of mut * typ * string

type un_op =
  | LogNot | Neg | Pos

type bin_op =
  | Add | Sub | Mul | Div | Mod | Exp
  | Eq | Lt | Gt | Neq | Leq | Geq
  | LogAnd | LogOr
  | Filter | Map
  | For | Do
  | Index

type lambda = {
  lformals  : bind list;
  lbody     : stmt list;
  lret_expr : expr option;
}

and lit =
  | LitInt of int
  | LitFloat of float
  | LitBool of bool
  | LitStr of string
  | LitKn of lambda
  | LitArray of expr list (* include optional type annotation here? *)
  | LitStruct of id * struct_field list (* should this be more sophisticated? *)

and struct_field = StructField of string * expr

and expr =
  | Unit (* can either be the unit generator or a void kernel/nop *)
  | Lit of lit
  | Id of id
  | Uniop of un_op * expr
  | Binop of expr * bin_op * expr
  | Cond of expr * expr * expr (* technically Ternop *)
  | Assign of expr * expr
  | Access of expr * string
  | Call of id * expr list
  | Lookback of id * int
  | LookbackDefault of expr * expr

and stmt =
  | VDecl of bind * expr option
  | Expr of expr

type fn_decl = {
  fname     : string;
  fn_typ    : fn_typ;
  ret_typ   : typ option;
  formals   : bind list;
  body      : stmt list;
  ret_expr  : expr option;
}

type struct_def = {
  sname     : string;
  fields    : bind list;
}

type extern_decl = {
  xalias    : string; (* what we call inside shux *)
  xfname    : string; (* what we link to outside shux *)
  xret_typ  : typ option;
  xformals  : bind list;
}

type glob_decl =
  | ConstDecl of bind * expr
  | StructDef of struct_def
  | ExternDecl of extern_decl

type ns_decl = {
  nname     : string;
  nbody     : program;
}

and program = ns_decl list * glob_decl list * fn_decl list
