module WatTest exposing (suite)

import Expect
import MinCaml.Syntax as Syntax
import MinCaml.Type as Type
import MinCaml.Wat exposing (convertExpr)
import Test exposing (..)


suite : Test
suite =
    describe "convertExpr test"
        [ test "int" <|
            \_ -> Expect.equal "(i32.const 1)" <| convertExpr 8 (Syntax.Int 1)
        , test "add" <|
            \_ ->
                let
                    expr =
                        Syntax.BinOp Type.TInt Syntax.Add (Syntax.Int 1) (Syntax.Int 2)

                    expected =
                        String.join ("\n" ++ String.repeat 8 " ")
                            [ "(i32.const 1)"
                            , "(i32.const 2)"
                            , "(i32.add)"
                            ]
                in
                Expect.equal expected <| convertExpr 8 expr
        ]
