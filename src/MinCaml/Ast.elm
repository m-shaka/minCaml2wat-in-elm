module MinCaml.Ast exposing (..)


type Expr
    = Int Int
    | Float Float
    | BinOp BinOp Expr Expr
    | Var String
    | LetIn String Expr Expr


type BinOp
    = Add
    | Sub
    | Mul
    | Div
    | AddDot
    | SubDot
    | MulDot
    | DivDot


isIntOp : BinOp -> Bool
isIntOp op =
    List.member op [ Add, Sub, Mul, Div ]


isFloatOp : BinOp -> Bool
isFloatOp op =
    not <| isIntOp op
