module Chomp exposing (..)

import Array
import Array2D exposing (..)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE


type alias Board =
    Array2D Int

-- Creates a new board

initChomp : ( Int, Int ) -> Board
initChomp ( r, c ) =
    repeat r c 1

initEmpty : ( Int, Int ) -> Board
initEmpty ( r, c ) =
    repeat r c 0

-- Returns rows and columns of a board

boardDimension : Board -> ( Int, Int )
boardDimension b =
    ( Array2D.rows b, Array2D.columns b )

-- Given a played position, modify the 2D array to show that certain pieces have been removed.
-- Returns Nothing if play instantly loses.

remove : ( Int, Int ) -> Board -> Maybe Board
remove (( i, j ) as coords) board =
    if i == rows board - 1 && j == 0 then
        Nothing
    else
        Just (indexedMap (updateCell coords) board)

-- Updates a cell to 0 if it is 'eaten'. Else defaults to cell value.

updateCell : ( Int, Int ) -> Int -> Int -> Int -> Int
updateCell ( i, j ) r c cell =
    if r <= i && c >= j then
        0
    else
        cell

-- Checks if next play is a loss.

isLoss : Board -> Bool
isLoss board =
    let
        r =
            rows board
        c =
            columns board
    in
    board == set (r - 1) 0 1 (repeat r c 0)


viewCell : msg -> Int -> Html msg
viewCell event state =
    let
        ( color, click ) =
            if state >= 1 then
                ( "pink", [ HtmlE.onClick event ] )
                -- Ignore click if on an already empty tile
            else
                ( "lavender", [] )
    in
    Html.div
        ([ HtmlA.style "width" "35px"
         , HtmlA.style "height" "35px"
         , HtmlA.style "background-color" color
         , HtmlA.style "border" "1px solid black"
         ]
            ++ click
        )
        []

arraysToLists : Array2D a -> List (List a)
arraysToLists =
    Array.toList << Array.map Array.toList << .data

view : (Int -> Int -> msg) -> Board -> Html msg
view event board =
    (Html.div
        [ HtmlA.style "display" "grid"
        , HtmlA.style "grid-template-columns" ("repeat(" ++ String.fromInt (Tuple.second (boardDimension board)) ++ ", 35px)")
        , HtmlA.style "padding" "50px"
        ]
        << List.concat
        << arraysToLists
        << indexedMap (\r c -> viewCell <| event r c)
    )
        board
