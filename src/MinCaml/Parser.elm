module MinCaml.Parser exposing (parse)

import Char
import MinCaml.Ast as Ast
import Parser exposing (..)
import Result
import Set


number_ : Parser Ast.Expr
number_ =
    number
        { int = Just Ast.Int
        , binary = Nothing
        , octal = Nothing
        , hex = Nothing
        , float = Just Ast.Float
        }


var : Parser Ast.Expr
var =
    variable
        { start = \c -> Char.isLower c || c == '_'
        , inner = \c -> Char.isAlphaNum c || c == '_' || c == '\''
        , reserved = Set.fromList []
        }
        |> map Ast.Var


base : Parser Ast.Expr
base =
    oneOf
        [ number_, var ]


factor : Parser Ast.Expr
factor =
    oneOf
        [ succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> additive)
            |. spaces
            |. symbol ")"
        , succeed identity
            |= base
        ]


multiplicative : Parser Ast.Expr
multiplicative =
    oneOf
        [ dotOp "*" Ast.Mul Ast.MulDot
        , dotOp "/" Ast.Div Ast.DivDot
        ]
        |> chainl factor


additive : Parser Ast.Expr
additive =
    oneOf
        [ dotOp "+" Ast.Add Ast.AddDot
        , dotOp "-" Ast.Sub Ast.SubDot
        ]
        |> chainl multiplicative


{-| clone of <https://hackage.haskell.org/package/parsec3-1.0.1.8/docs/src/Text-Parsec-Combinator.html#chainl1>
-}
chainl : Parser Ast.Expr -> Parser Ast.BinOp -> Parser Ast.Expr
chainl baseParser opParser =
    let
        rest e =
            oneOf
                [ succeed (\op e_ -> Ast.BinOp op e e_)
                    |= opParser
                    |. spaces
                    |= baseParser
                    |. spaces
                    |> andThen rest
                , succeed e
                ]
    in
    succeed identity
        |= baseParser
        |. spaces
        |> andThen rest


dotOp : String -> Ast.BinOp -> Ast.BinOp -> Parser Ast.BinOp
dotOp s op dotOp_ =
    succeed identity
        |. symbol s
        |> andThen
            (\_ ->
                oneOf
                    [ succeed dotOp_
                        |. symbol "."
                    , succeed op
                    ]
            )


expr : Parser Ast.Expr
expr =
    succeed identity
        |. spaces
        |= additive
        |. spaces
        |. end


parse : String -> Result String Ast.Expr
parse input =
    run expr input |> Result.mapError (String.join "\n" << List.map toString)


toString : DeadEnd -> String
toString { row, col, problem } =
    "row: " ++ String.fromInt row ++ ", column: " ++ String.fromInt col ++ ", " ++ Debug.toString problem
