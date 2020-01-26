module MinCaml.Wat exposing (convert, convertExpr)

import MinCaml.Ast as Ast


indent : String
indent =
    String.repeat 8 " "


convertBinOp : Ast.BinOp -> String
convertBinOp op =
    case op of
        Ast.Add ->
            "(i32.add)"

        Ast.Mul ->
            "(i32.mul)"


convertExpr : Ast.Expr -> String
convertExpr expr =
    case expr of
        Ast.Int i ->
            "(i32.const " ++ String.fromInt i ++ ")"

        Ast.BinOp op x y ->
            String.join ("\n" ++ indent)
                [ convertExpr x
                , convertExpr y
                , convertBinOp op
                ]


convert : Ast.Expr -> String
convert expr =
    let
        base =
            """(module
    (export "exported_main" (func $main))
    (func $main (result i32)
        """

        watExpr =
            convertExpr expr
    in
    base ++ watExpr ++ "))"
