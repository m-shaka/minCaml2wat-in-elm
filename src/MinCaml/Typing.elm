module MinCaml.Typing exposing (..)

import MinCaml.Ast as Ast


type TExpr
    = TExpr Type Ast.Expr


type Type
    = TInt
    | TFloat
    | TUnit


unify : Type -> Type -> Result String Type
unify t1 t2 =
    if t1 == t2 then
        Ok TUnit

    else
        Err "unification error"


inferExpr : Ast.Expr -> Result String Type
inferExpr expr =
    let
        unifyOperand type_ e =
            inferExpr e |> Result.andThen (unify type_)

        inferBinOp type_ e1 e2 =
            case ( unifyOperand type_ e1, unifyOperand type_ e2 ) of
                ( Err e, _ ) ->
                    Err e

                ( _, Err e ) ->
                    Err e

                _ ->
                    Ok type_
    in
    case expr of
        Ast.Int _ ->
            Ok TInt

        Ast.Float _ ->
            Ok TFloat

        Ast.BinOp op e1 e2 ->
            let
                expected =
                    if Ast.isIntOp op then
                        TInt

                    else
                        TFloat
            in
            inferBinOp expected e1 e2

        Ast.Var _ ->
            Ok TUnit

        Ast.LetIn _ _ _ ->
            Ok TUnit


addType : Ast.Expr -> Result String TExpr
addType expr =
    inferExpr expr |> Result.map (\t -> TExpr t expr)


show : Type -> String
show t =
    case t of
        TInt ->
            "i32"

        TFloat ->
            "f32"

        TUnit ->
            "()"
