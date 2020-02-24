module MinCaml.Wat exposing (convert, convertExpr)

import MinCaml.Syntax as Syntax
import MinCaml.Typing as Typing
import String.Format as Format


convertBinOp : Syntax.BinOp -> String
convertBinOp op =
    case op of
        Syntax.Add ->
            "(i32.add)"

        Syntax.Sub ->
            "(i32.sub)"

        Syntax.Mul ->
            "(i32.mul)"

        Syntax.Div ->
            "(i32.div_s)"

        Syntax.AddDot ->
            "(f32.add)"

        Syntax.SubDot ->
            "(f32.sub)"

        Syntax.MulDot ->
            "(f32.mul)"

        Syntax.DivDot ->
            "(f32.div)"


convertExpr : Int -> Syntax.Expr -> String
convertExpr indentLevel expr =
    let
        genIndent n =
            String.repeat n " "
    in
    case expr of
        Syntax.Int i ->
            "(i32.const " ++ String.fromInt i ++ ")"

        Syntax.Float f ->
            "(f32.const " ++ String.fromFloat f ++ ")"

        Syntax.BinOp _ op x y ->
            String.join ("\n" ++ genIndent indentLevel)
                [ convertExpr indentLevel x
                , convertExpr indentLevel y
                , convertBinOp op
                ]

        Syntax.Var _ name ->
            Format.value "(get_local ${})" name

        _ ->
            ""


convert : Syntax.Expr -> String
convert expr =
    let
        watExpr =
            convertExpr 8 expr

        type_ =
            Syntax.extractType expr
    in
    """(module
    (export "exported_main" (func $main))
    (func $main (result {{ resultType }})
        {{ body }}
    )
)"""
        |> Format.namedValue "resultType" (Typing.show type_)
        |> Format.namedValue "body" watExpr
