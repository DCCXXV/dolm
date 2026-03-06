type verb = Bind | Sever | Invert | Echo | Consume | Veil

type expr =
  | Verb of verb
  | Compose of expr * expr
  | Lam of expr
  | App of expr * expr
  | Var of int

type tile =
  | Empty
