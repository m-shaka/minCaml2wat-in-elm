module MinCaml.Wat exposing (convert, convertExpr)

import MinCaml.Ast as Ast
import MinCaml.Typing as Typing
import String.Format as Format


indent : String
indent =
    String.repeat 8 " "


convertBinOp : Ast.BinOp -> String
convertBinOp op =
    case op of
        Ast.Add ->
            "(i32.add)"

        Ast.Sub ->
            "(i32.sub)"

        Ast.Mul ->
            "(i32.mul)"

        Ast.Div ->
            "(i32.div_s)"

        Ast.AddDot ->
            "(f32.add)"

        Ast.SubDot ->
            "(f32.sub)"

        Ast.MulDot ->
            "(f32.mul)"

        Ast.DivDot ->
            "(f32.div)"


convertExpr : Ast.Expr -> String
convertExpr expr =
    case expr of
        Ast.Int i ->
            "(i32.const " ++ String.fromInt i ++ ")"

        Ast.Float f ->
            "(f32.const " ++ String.fromFloat f ++ ")"

        Ast.BinOp op x y ->
            String.join ("\n" ++ indent)
                [ convertExpr x
                , convertExpr y
                , convertBinOp op
                ]

        _ ->
            ""


convert : Typing.TExpr -> String
convert (Typing.TExpr type_ expr) =
    let
        watExpr =
            convertExpr expr
    in
    """(module
    (export "exported_main" (func $main))
    (func $main (result {{ resultType }})
        {{ body }}
    )
)"""
        |> Format.namedValue "resultType" (Typing.show type_)
        |> Format.namedValue "body" watExpr
