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
    succeed identity
        |= lazy (\_ -> factor)
        |. spaces
        |> andThen
            (\e ->
                oneOf
                    [ succeed (Ast.BinOp Ast.Multi e)
                        |. symbol "*"
                        |. spaces
                        |= lazy (\_ -> factor)
                    , succeed e
                    ]
            )


additive : Parser Ast.Expr
additive =
    succeed identity
        |= lazy (\_ -> multiplicative)
        |. spaces
        |> andThen
            (\e ->
                oneOf
                    [ succeed (Ast.BinOp Ast.Plus e)
                        |. symbol "+"
                        |. spaces
                        |= lazy (\_ -> multiplicative)
                    , succeed e
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
