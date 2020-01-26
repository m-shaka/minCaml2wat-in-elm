module ParserTest exposing (suite)

import Expect
import MinCaml.Ast as Ast
import MinCaml.Parser exposing (parse)
import Test exposing (..)


suite : Test
suite =
    describe "parser test"
        [ test "add assoc" <|
            \_ ->
                let
                    src =
                        "5 + 3 + 2"

                    expected =
                        Ast.BinOp
                            Ast.Add
                            (Ast.BinOp
                                Ast.Add
                                (Ast.Int 5)
                                (Ast.Int 3)
                            )
                            (Ast.Int 2)
                in
                case parse src of
                    Ok ast ->
                        Expect.equal expected ast

                    Err e ->
                        Expect.fail e
        , test "mul assoc" <|
            \_ ->
                let
                    src =
                        "5 * 3 * 2"

                    expected =
                        Ast.BinOp
                            Ast.Mul
                            (Ast.BinOp
                                Ast.Mul
                                (Ast.Int 5)
                                (Ast.Int 3)
                            )
                            (Ast.Int 2)
                in
                case parse src of
                    Ok ast ->
                        Expect.equal expected ast

                    Err e ->
                        Expect.fail e
        ]
