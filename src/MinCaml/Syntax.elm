module MinCaml.Syntax exposing (..)

import MinCaml.Type as Type


type Expr
    = Int Int
    | Float Float
    | BinOp Type.Type BinOp Expr Expr
    | Var Type.Type String
    | LetIn Type.Type String Expr Expr


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


extractType : Expr -> Type.Type
extractType expr =
    case expr of
        Int _ ->
            Type.TInt

        Float _ ->
            Type.TFloat

        BinOp t _ _ _ ->
            t

        Var t _ ->
            t

        LetIn t _ _ _ ->
            t
