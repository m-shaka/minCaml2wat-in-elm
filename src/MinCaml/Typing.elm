module MinCaml.Typing exposing (..)

import Dict
import MinCaml.Ast as Ast


type TExpr
    = TExpr Type Ast.Expr


type Type
    = TInt
    | TFloat
    | TUnit


type alias Env =
    Dict.Dict String Type


unify : Type -> Type -> Result String Type
unify t1 t2 =
    if t1 == t2 then
        Ok TUnit

    else
        Err "unification error"


inferExpr : Env -> Ast.Expr -> Result String Type
inferExpr env expr =
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
            inferExpr env e1
                |> Result.andThen (unify expected)
                |> Result.andThen (\_ -> inferExpr env e2)
                |> Result.andThen (unify expected)
                |> Result.map (\_ -> expected)

        Ast.Var name ->
            case Dict.get name env of
                Just t ->
                    Ok t

                Nothing ->
                    Err "undefined variable"

        Ast.LetIn name e1 e2 ->
            inferExpr env e1
                |> Result.andThen
                    (\t ->
                        inferExpr (Dict.insert name t env) e2
                    )


typing : Ast.Expr -> Result String TExpr
typing expr =
    inferExpr Dict.empty expr |> Result.map (\t -> TExpr t expr)


show : Type -> String
show t =
    case t of
        TInt ->
            "i32"

        TFloat ->
            "f32"

        TUnit ->
            "()"
