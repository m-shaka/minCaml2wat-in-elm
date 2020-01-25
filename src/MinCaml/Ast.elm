module MinCaml.Ast exposing (..)


type Expr
    = Int Int
    | BinOp BinOp Expr Expr


type BinOp
    = Plus
    | Multi
