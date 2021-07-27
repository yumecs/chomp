module Main exposing (main)

import Browser
import Chomp exposing (Board)
import Css exposing (..)
import Debug
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE

---------------------------------------------------------------------ˀ

main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    { boardSize : ( Int, Int )
    , board : Board
    , score : ( Int, Int )
    , gameOver : Bool
    , currentPlayer : Bool
    }

type Msg
    = Click Int Int
    | Reset
    | Chrow Int
    | Chcol Int

type alias Flags =
    ()

init : Flags -> ( Model, Cmd Msg )
init () =
    ( initModel ( 6, 6 ), Cmd.none )

initModel : ( Int, Int ) -> Model
initModel ( x, y ) =
    { boardSize = ( x, y )
    , board = Chomp.initChomp ( x, y )
    , score = ( 0, 0 )
    , gameOver = False
    , currentPlayer = True
    }

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

viewPlayer : Model -> String
viewPlayer { currentPlayer } =
    if currentPlayer then
        "Player 1"
    else
        "Player 2"

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click r c ->
            if model.gameOver then
                ( model, Cmd.none )
            else
                let
                    _ =
                        Debug.log "clicked" <| "row = " ++ String.fromInt r ++ "; col = " ++ String.fromInt c
                    winScore =
                        if model.currentPlayer then
                            ( Tuple.first model.score + 1, Tuple.second model.score )

                        else
                            ( Tuple.first model.score, Tuple.second model.score + 1 )
                    lossScore =
                        if model.currentPlayer then
                            ( Tuple.first model.score, Tuple.second model.score + 1 )

                        else
                            ( Tuple.first model.score + 1, Tuple.second model.score )
                    newModel =
                        case Chomp.remove ( r, c ) model.board of
                            Just newBoard ->
                                if Chomp.isLoss newBoard then
                                    { model | board = newBoard, score = winScore, gameOver = True }

                                else
                                    { model | board = newBoard, currentPlayer = not model.currentPlayer }

                            Nothing ->
                                { model
                                    | board = Chomp.initEmpty (Chomp.boardDimension model.board)
                                    , currentPlayer = not model.currentPlayer
                                    , score = lossScore
                                    , gameOver = True
                                }
                in
                ( newModel, Cmd.none )

        Reset ->
            ( { model | board = Chomp.initChomp model.boardSize, gameOver = False, currentPlayer = True }, Cmd.none )

        Chrow r ->
            ( { model | boardSize = ( r, Tuple.second model.boardSize ) }, Cmd.none )

        Chcol c ->
            ( { model | boardSize = ( Tuple.first model.boardSize, c ) }, Cmd.none )


view : Model -> Html Msg
view ({ board, score, gameOver } as model) =
    let
        ( s1, s2 ) =
            Tuple.mapBoth String.fromInt String.fromInt score
        space =
            Html.br [] []
    in
    Html.div
        [ HtmlA.style "position" "absolute"
        , HtmlA.style "left" "50%"
        , HtmlA.style "top" "0%"
        , HtmlA.style "transform" "translate(-50%, 20%)"
        , HtmlA.style "text-align" "center"
        ]
        [ Html.text "Chomp"
        , space
        , space
        , Html.hr [] []
        , space
        , Html.div
            []
            (if gameOver then
                [ Html.text <| "Game Over! " ++ viewPlayer model ++ " wins."
                , space
                , space
                ]

             else
                [ Html.text <| viewPlayer model ++ "'s turn" ]
            )
        , space
        , Html.div
            [ HtmlA.style "text-align" "center"
            , HtmlA.style "background" "lightblue"
            , HtmlA.style "padding" "5px"
            , HtmlA.style "border-radius" "5px"
            ]
            [ Chomp.view Click board ]
        , space
        , space
        , Html.text <| "Player 1: " ++ s1 ++ " | Player 2: " ++ s2
        , space
        , space
        , Html.text "Rows"
        , space
        , Html.div
            []
            [ Html.input
                [ HtmlA.type_ "range"
                , HtmlA.min "1"
                , HtmlA.max "15"
                , HtmlA.value <| String.fromInt <| Tuple.first <| model.boardSize
                , HtmlE.onInput (Chrow << Maybe.withDefault 5 << String.toInt)
                ]
                []
            , Html.text <| String.fromInt <| Tuple.first <| model.boardSize
            ]
        , Html.text "Columns:"
        , space
        , Html.div
            []
            [ Html.input
                [ HtmlA.type_ "range"
                , HtmlA.min "1"
                , HtmlA.max "15"
                , HtmlA.value <| String.fromInt <| Tuple.second <| model.boardSize
                , HtmlE.onInput (Chcol << Maybe.withDefault 5 << String.toInt)
                ]
                []
            , Html.text <| String.fromInt <| Tuple.second <| model.boardSize
            ]
        , space
        , space
        , Html.div
            []
            (if gameOver then
                [ space
                , space
                , Html.button [ HtmlE.onClick Reset ] [ Html.text "Play again" ]
                ]

             else
                [ space
                , space
                , Html.button [ HtmlE.onClick Reset ] [ Html.text "Reset" ]
                ]
            )
        ]
