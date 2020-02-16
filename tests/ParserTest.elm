module ParserTest exposing (suite)

import Expect
import MinCaml.Ast as Ast
import MinCaml.Parser exposing (parse)
import Test exposing (..)


suite : Test
suite =
    describe "parser test"
        [ testBinOp
        , testBinOpDot
        , testVar
        , testLetIn
        ]


testBinOp : Test
testBinOp =
    describe "binOp"
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
                Expect.equal (Ok expected) (parse src)
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
                Expect.equal (Ok expected) (parse src)
        , test "both (additive first)" <|
            \_ ->
                let
                    src =
                        "5 + 3 * 2 / 2"

                    expected =
                        Ast.BinOp
                            Ast.Add
                            (Ast.Int 5)
                            (Ast.BinOp
                                Ast.Div
                                (Ast.BinOp
                                    Ast.Mul
                                    (Ast.Int 3)
                                    (Ast.Int 2)
                                )
                                (Ast.Int 2)
                            )
                in
                Expect.equal (Ok expected) (parse src)
        , test "both (multiplicative first)" <|
            \_ ->
                let
                    src =
                        "5 * 3 + 2"

                    expected =
                        Ast.BinOp
                            Ast.Add
                            (Ast.BinOp
                                Ast.Mul
                                (Ast.Int 5)
                                (Ast.Int 3)
                            )
                            (Ast.Int 2)
                in
                Expect.equal (Ok expected) (parse src)
        ]


testBinOpDot : Test
testBinOpDot =
    describe "binOp dot"
        [ test "both (additive first)" <|
            \_ ->
                let
                    src =
                        "5. +. 3. *. 2. /. 2."

                    expected =
                        Ast.BinOp
                            Ast.AddDot
                            (Ast.Float 5.0)
                            (Ast.BinOp
                                Ast.DivDot
                                (Ast.BinOp
                                    Ast.MulDot
                                    (Ast.Float 3.0)
                                    (Ast.Float 2.0)
                                )
                                (Ast.Float 2.0)
                            )
                in
                Expect.equal (Ok expected) (parse src)
        , test "both (multiplicative first)" <|
            \_ ->
                let
                    src =
                        "5. *. 3. +. 2."

                    expected =
                        Ast.BinOp
                            Ast.AddDot
                            (Ast.BinOp
                                Ast.MulDot
                                (Ast.Float 5.0)
                                (Ast.Float 3.0)
                            )
                            (Ast.Float 2.0)
                in
                Expect.equal (Ok expected) (parse src)
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


testLetIn : Test
testLetIn =
    describe "let ... in"
        [ test "simple" <|
            \_ ->
                let
                    src =
                        "let a = 3 in a"

                    expected =
                        Ast.LetIn "a" (Ast.Int 3) (Ast.Var "a")
                in
                Expect.equal (Ok expected) (parse src)
        , test "nested" <|
            \_ ->
                let
                    src =
                        "let a = 3 in let b = 5 in a + b"

                    expected =
                        Ast.LetIn "a"
                            (Ast.Int 3)
                            (Ast.LetIn "b"
                                (Ast.Int 5)
                                (Ast.BinOp Ast.Add (Ast.Var "a") (Ast.Var "b"))
                            )
                in
                Expect.equal (Ok expected) (parse src)
        , test "complex exp" <|
            \_ ->
                let
                    src =
                        "let a = 3.1 +. b in a"

                    expected =
                        Ast.LetIn "a"
                            (Ast.BinOp Ast.AddDot (Ast.Float 3.1) (Ast.Var "b"))
                            (Ast.Var "a")
                in
                Expect.equal (Ok expected) (parse src)
        ]
