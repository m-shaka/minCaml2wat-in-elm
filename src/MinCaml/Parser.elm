module MinCaml.Parser exposing (parse)

import MinCaml.Ast as Ast
import Parser exposing (..)
import Result


int_ : Parser Ast.Expr
int_ =
    succeed Ast.Int |= int


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
            |= int_
        ]


multiplicative : Parser Ast.Expr
multiplicative =
    oneOf
        [ map (always Ast.Mul) <| symbol "*"
        , map (always Ast.Div) <| symbol "/"
        ]
        |> chainl factor


additive : Parser Ast.Expr
additive =
    oneOf
        [ map (always Ast.Add) <| symbol "+"
        , map (always Ast.Sub) <| symbol "-"
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
