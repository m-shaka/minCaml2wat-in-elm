module ParserTest exposing (suite)

import Expect
import MinCaml.Parser exposing (parse)
import MinCaml.Syntax as Syntax
import MinCaml.Type as Type
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
                        Syntax.BinOp
                            Type.TUnit
                            Syntax.Add
                            (Syntax.BinOp
                                Type.TUnit
                                Syntax.Add
                                (Syntax.Int 5)
                                (Syntax.Int 3)
                            )
                            (Syntax.Int 2)
                in
                Expect.equal (Ok expected) (parse src)
        , test "mul assoc" <|
            \_ ->
                let
                    src =
                        "5 * 3 * 2"

                    expected =
                        Syntax.BinOp
                            Type.TUnit
                            Syntax.Mul
                            (Syntax.BinOp
                                Type.TUnit
                                Syntax.Mul
                                (Syntax.Int 5)
                                (Syntax.Int 3)
                            )
                            (Syntax.Int 2)
                in
                Expect.equal (Ok expected) (parse src)
        , test "both (additive first)" <|
            \_ ->
                let
                    src =
                        "5 + 3 * 2 / 2"

                    expected =
                        Syntax.BinOp
                            Type.TUnit
                            Syntax.Add
                            (Syntax.Int 5)
                            (Syntax.BinOp
                                Type.TUnit
                                Syntax.Div
                                (Syntax.BinOp
                                    Type.TUnit
                                    Syntax.Mul
                                    (Syntax.Int 3)
                                    (Syntax.Int 2)
                                )
                                (Syntax.Int 2)
                            )
                in
                Expect.equal (Ok expected) (parse src)
        , test "both (multiplicative first)" <|
            \_ ->
                let
                    src =
                        "5 * 3 + 2"

                    expected =
                        Syntax.BinOp
                            Type.TUnit
                            Syntax.Add
                            (Syntax.BinOp
                                Type.TUnit
                                Syntax.Mul
                                (Syntax.Int 5)
                                (Syntax.Int 3)
                            )
                            (Syntax.Int 2)
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
                        Syntax.BinOp
                            Type.TUnit
                            Syntax.AddDot
                            (Syntax.Float 5.0)
                            (Syntax.BinOp
                                Type.TUnit
                                Syntax.DivDot
                                (Syntax.BinOp
                                    Type.TUnit
                                    Syntax.MulDot
                                    (Syntax.Float 3.0)
                                    (Syntax.Float 2.0)
                                )
                                (Syntax.Float 2.0)
                            )
                in
                Expect.equal (Ok expected) (parse src)
        , test "both (multiplicative first)" <|
            \_ ->
                let
                    src =
                        "5. *. 3. +. 2."

                    expected =
                        Syntax.BinOp
                            Type.TUnit
                            Syntax.AddDot
                            (Syntax.BinOp
                                Type.TUnit
                                Syntax.MulDot
                                (Syntax.Float 5.0)
                                (Syntax.Float 3.0)
                            )
                            (Syntax.Float 2.0)
                in
                Expect.equal (Ok expected) (parse src)
        ]


testVar : Test
testVar =
    describe "Var"
        [ test "simple alpha" <|
            \_ ->
                Expect.equal (Ok (Syntax.Var Type.TUnit "a")) (parse "a")
        , test "start from _" <|
            \_ ->
                Expect.equal (Ok (Syntax.Var Type.TUnit "_a")) (parse "_a")
        , test "contains number" <|
            \_ ->
                Expect.equal (Ok (Syntax.Var Type.TUnit "_1a3b")) (parse "_1a3b")
        , test "contains '" <|
            \_ ->
                Expect.equal (Ok (Syntax.Var Type.TUnit "_a'")) (parse "_a'")
        , test "start from capital" <|
            \_ ->
                Expect.err (parse "Abc")
        , test "apply binOp to var" <|
            \_ ->
                let
                    src =
                        "a + b"

                    expected =
                        Syntax.BinOp
                            Type.TUnit
                            Syntax.Add
                            (Syntax.Var Type.TUnit "a")
                            (Syntax.Var Type.TUnit "b")
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
                        Syntax.LetIn
                            Type.TUnit
                            "a"
                            (Syntax.Int 3)
                            (Syntax.Var Type.TUnit "a")
                in
                Expect.equal (Ok expected) (parse src)
        , test "nested" <|
            \_ ->
                let
                    src =
                        "let a = 3 in let b = 5 in a + b"

                    expected =
                        Syntax.LetIn
                            Type.TUnit
                            "a"
                            (Syntax.Int 3)
                            (Syntax.LetIn
                                Type.TUnit
                                "b"
                                (Syntax.Int 5)
                                (Syntax.BinOp
                                    Type.TUnit
                                    Syntax.Add
                                    (Syntax.Var Type.TUnit "a")
                                    (Syntax.Var Type.TUnit "b")
                                )
                            )
                in
                Expect.equal (Ok expected) (parse src)
        , test "complex exp" <|
            \_ ->
                let
                    src =
                        "let a = 3.1 +. b in a"

                    expected =
                        Syntax.LetIn
                            Type.TUnit
                            "a"
                            (Syntax.BinOp
                                Type.TUnit
                                Syntax.AddDot
                                (Syntax.Float 3.1)
                                (Syntax.Var Type.TUnit "b")
                            )
                            (Syntax.Var Type.TUnit "a")
                in
                Expect.equal (Ok expected) (parse src)
        ]
