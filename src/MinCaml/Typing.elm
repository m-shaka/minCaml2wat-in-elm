module MinCaml.Typing exposing (..)

import MinCaml.Ast as Ast


type TExpr
    = TExpr Type Ast.Expr


type Type
    = TInt
    | TUnit


unify : Type -> Type -> Result String Type
unify t1 t2 =
    if t1 == t2 then
        Ok TUnit

    else
        Err "unification error"


inferExpr : Ast.Expr -> Result String Type
inferExpr expr =
    case expr of
        Ast.Int _ ->
            Ok TInt

        Ast.BinOp _ e1 e2 ->
            let
                unifyOperand e =
                    inferExpr e |> Result.andThen (unify TInt)

                r1 =
                    unifyOperand e1

                r2 =
                    unifyOperand e2
            in
            case ( r1, r2 ) of
                ( Err _, _ ) ->
                    r1

                ( _, Err _ ) ->
                    r2

                _ ->
                    Ok TInt


addType : Ast.Expr -> Result String TExpr
addType expr =
    inferExpr expr |> Result.map (\t -> TExpr t expr)


show : Type -> String
show t =
    case t of
        TInt ->
            "i32"

        TUnit ->
            "()"
