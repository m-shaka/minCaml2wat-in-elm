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
        , testVar
        ]


testVar : Test
testVar =
    describe "Var"
        [ test "simple alpha" <|
            \_ ->
                Expect.equal (Ok (Ast.Var "a")) (parse "a")
        , test "start from _" <|
            \_ ->
                Expect.equal (Ok (Ast.Var "_a")) (parse "_a")
        , test "contains number" <|
            \_ ->
                Expect.equal (Ok (Ast.Var "_1a3b")) (parse "_1a3b")
        , test "contains '" <|
            \_ ->
                Expect.equal (Ok (Ast.Var "_a'")) (parse "_a'")
        , test "start from capital" <|
            \_ ->
                Expect.err (parse "Abc")
        , test "apply binOp to var" <|
            \_ ->
                let
                    src =
                        "a + b"

                    expected =
                        Ast.BinOp Ast.Add (Ast.Var "a") (Ast.Var "b")
                in
                Expect.equal (Ok expected) (parse src)
        ]
