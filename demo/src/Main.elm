module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Maybe
import MinCaml.Parser exposing (parse)
import MinCaml.Typing exposing (addType)
import MinCaml.Wat as Wat
import Port


type alias Flags =
    ()


type alias Model =
    { wat : String
    , returnValue : Maybe String
    , srcCode : String
    }


type Msg
    = SendWat
    | RecvReturnValue String
    | InputText String


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Model "" Nothing "", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendWat ->
            case parse model.srcCode |> Result.andThen addType of
                Ok expr ->
                    let
                        wat =
                            Wat.convert expr
                    in
                    ( { model | wat = wat }, Cmd.batch [ Port.sendWat wat ] )

                Err err ->
                    ( { model | wat = err }, Cmd.none )

        RecvReturnValue v ->
            ( { model | returnValue = Just v }, Cmd.none )

        InputText t ->
            ( { model | srcCode = t }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Port.returnValue RecvReturnValue ]


view : Model -> Html.Html Msg
view model =
    div []
        [ form [ onSubmit SendWat ]
            [ input [ type_ "text", onInput InputText, value model.srcCode ] []
            , input [ type_ "submit", value "Compile!" ] []
            ]
        , pre [] [ code [ style "white-space" "pre-wrap" ] [ text model.wat ] ]
        , text <| "returnValue: " ++ Maybe.withDefault "" model.returnValue
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
