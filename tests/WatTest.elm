module WatTest exposing (suite)

import Ast
import Expect
import Test exposing (..)
import Wat exposing (convertExpr)


suite : Test
suite =
    describe "convertExpr test"
        [ test "int" <|
            \_ -> Expect.equal "(i32.const 1)" <| convertExpr (Ast.Int 1)
        , test "plus" <|
            \_ ->
                let
                    expr =
                        Ast.BinOp Ast.Plus (Ast.Int 1) (Ast.Int 2)

                    expected =
                        String.join "\n"
                            [ "(i32.const 1)"
                            , "(i32.const 2)"
                            , "(i32.add)"
                            ]
                in
                Expect.equal expected <| convertExpr expr
        ]
