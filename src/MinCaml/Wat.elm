module MinCaml.Wat exposing (convert, convertExpr)

import MinCaml.Ast as Ast
import MinCaml.Typing as Typing
import String.Format as Format


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


convertExpr : Int -> Typing.TExpr -> String
convertExpr indentLevel (Typing.TExpr t expr) =
    let
        genIndent n =
            String.repeat n " "
    in
    case expr of
        Ast.Int i ->
            "(i32.const " ++ String.fromInt i ++ ")"

        Ast.Float f ->
            "(f32.const " ++ String.fromFloat f ++ ")"

        Ast.BinOp op x y ->
            String.join ("\n" ++ genIndent indentLevel)
                [ convertExpr indentLevel x
                , convertExpr indentLevel y
                , convertBinOp op
                ]

        Ast.Var name ->
            Format.value "(get_local ${})" name

        _ ->
            ""


convert : Typing.TExpr -> String
convert (Typing.TExpr type_ expr) =
    let
        watExpr =
            convertExpr 8 expr
    in
    """(module
    (export "exported_main" (func $main))
    (func $main (result {{ resultType }})
        {{ body }}
    )
)"""
        |> Format.namedValue "resultType" (Typing.show type_)
        |> Format.namedValue "body" watExpr
