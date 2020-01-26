module WatTest exposing (suite)

import Expect
import MinCaml.Ast as Ast
import MinCaml.Wat exposing (convertExpr)
import Test exposing (..)


suite : Test
suite =
    describe "convertExpr test"
        [ test "int" <|
            \_ -> Expect.equal "(i32.const 1)" <| convertExpr (Ast.Int 1)
        , test "add" <|
            \_ ->
                let
                    expr =
                        Ast.BinOp Ast.Add (Ast.Int 1) (Ast.Int 2)

                    expected =
                        String.join ("\n" ++ String.repeat 8 " ")
                            [ "(i32.const 1)"
                            , "(i32.const 2)"
                            , "(i32.add)"
                            ]
                in
                Expect.equal expected <| convertExpr expr
        ]
