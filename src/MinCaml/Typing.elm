module MinCaml.Typing exposing (..)

import Dict
import MinCaml.Syntax as Syntax exposing (Expr(..))
import MinCaml.Type exposing (Type(..))


type alias Env =
    Dict.Dict String Type


unify : Type -> Type -> Result String Type
unify t1 t2 =
    if t1 == t2 then
        Ok t1

    else
        Err "unification error"


inferExpr : Env -> Syntax.Expr -> Result String Type
inferExpr env expr =
    case expr of
        Int _ ->
            Ok TInt

        Float _ ->
            Ok TFloat

        BinOp _ op e1 e2 ->
            let
                expected =
                    if Syntax.isIntOp op then
                        TInt

                    else
                        TFloat
            in
            inferExpr env e1
                |> Result.andThen (unify expected)
                |> Result.andThen (\_ -> inferExpr env e2)
                |> Result.andThen (unify expected)
                |> Result.map (\_ -> expected)

        Var _ name ->
            Dict.get name env
                |> Result.fromMaybe "undefined variable"

        LetIn _ name e1 e2 ->
            inferExpr env e1
                |> Result.andThen
                    (\t ->
                        inferExpr (Dict.insert name t env) e2
                    )


typing : Syntax.Expr -> Result String Expr
typing expr =
    let
        addType t =
            case expr of
                Syntax.Int i ->
                    Int i

                Syntax.Float f ->
                    Float f

                BinOp _ op e1 e2 ->
                    BinOp t op e1 e2

                Var _ name ->
                    Var t name

                LetIn _ name e1 e2 ->
                    LetIn t name e1 e2
    in
    inferExpr Dict.empty expr
        |> Result.map addType


show : Type -> String
show t =
    case t of
        TInt ->
            "i32"

        TFloat ->
            "f32"

        TUnit ->
            "()"
