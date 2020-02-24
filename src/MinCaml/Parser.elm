module MinCaml.Parser exposing (parse)

import Char
import MinCaml.Syntax as Syntax
import MinCaml.Type as Type
import Parser exposing (..)
import Result
import Set


number_ : Parser Syntax.Expr
number_ =
    number
        { int = Just Syntax.Int
        , binary = Nothing
        , octal = Nothing
        , hex = Nothing
        , float = Just Syntax.Float
        }


var : Parser String
var =
    variable
        { start = \c -> Char.isLower c || c == '_'
        , inner = \c -> Char.isAlphaNum c || c == '_' || c == '\''
        , reserved = Set.fromList [ "let", "in" ]
        }


letIn : Parser Syntax.Expr
letIn =
    succeed (Syntax.LetIn Type.TUnit)
        |. keyword "let"
        |. spaces
        |= var
        |. spaces
        |. symbol "="
        |. spaces
        |= lazy (\_ -> additive)
        |. spaces
        |. keyword "in"
        |. spaces
        |= lazy (\_ -> additive)


base : Parser Syntax.Expr
base =
    oneOf
        [ number_, map (Syntax.Var Type.TUnit) var, letIn ]


factor : Parser Syntax.Expr
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


multiplicative : Parser Syntax.Expr
multiplicative =
    oneOf
        [ dotOp "*" Syntax.Mul Syntax.MulDot
        , dotOp "/" Syntax.Div Syntax.DivDot
        ]
        |> chainl factor


additive : Parser Syntax.Expr
additive =
    oneOf
        [ dotOp "+" Syntax.Add Syntax.AddDot
        , dotOp "-" Syntax.Sub Syntax.SubDot
        ]
        |> chainl multiplicative


{-| clone of <https://hackage.haskell.org/package/parsec3-1.0.1.8/docs/src/Text-Parsec-Combinator.html#chainl1>
-}
chainl : Parser Syntax.Expr -> Parser Syntax.BinOp -> Parser Syntax.Expr
chainl baseParser opParser =
    let
        rest e =
            oneOf
                [ succeed (\op e_ -> Syntax.BinOp Type.TUnit op e e_)
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


dotOp : String -> Syntax.BinOp -> Syntax.BinOp -> Parser Syntax.BinOp
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


expr : Parser Syntax.Expr
expr =
    succeed identity
        |. spaces
        |= additive
        |. spaces
        |. end


parse : String -> Result String Syntax.Expr
parse input =
    run expr input |> Result.mapError (String.join "\n" << List.map toString)


toString : DeadEnd -> String
toString { row, col, problem } =
    "row: " ++ String.fromInt row ++ ", column: " ++ String.fromInt col ++ ", " ++ Debug.toString problem
