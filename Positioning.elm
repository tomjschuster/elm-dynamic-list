module Positioning exposing (..)

import Array exposing (Array)
import Types exposing (Dimensions, Position)


type alias Config =
    { fixedWidth : Maybe Int
    , fixedHeight : Maybe Int
    , xMargin : Int
    , yMargin : Int
    , columns : Int
    }


getTranslations : Config -> List Dimensions -> List Position
getTranslations config dimensions =
    ( Array.repeat config.columns 0, [] )
        |> getTranslationHelper config dimensions
        |> Tuple.second


getTranslationHelper :
    Config
    -> List Dimensions
    -> ( Array Int, List Position )
    -> ( Array Int, List Position )
getTranslationHelper config dimensions ( columnHeights, translations ) =
    case dimensions of
        [] ->
            ( columnHeights, translations )

        next :: rest ->
            let
                ( minIndex, minHeight ) =
                    getMinColumn columnHeights

                newColumnHeights =
                    Array.set
                        minIndex
                        (next.height + config.yMargin)
                        columnHeights

                newTranslations =
                    Position (getTranslateX config next.width minIndex) minHeight
                        |> List.singleton
                        |> List.append translations
            in
            getTranslationHelper config rest ( newColumnHeights, newTranslations )


getMinColumn : Array Int -> ( Int, Int )
getMinColumn columnHeights =
    let
        ( _, minIndex, minHeight ) =
            Array.foldl
                compareColumnHeights
                ( 0, 0, Maybe.withDefault 0 (Array.get 0 columnHeights) )
                columnHeights
    in
    ( minIndex, minHeight )


getTranslateX : Config -> Int -> Int -> Int
getTranslateX config width =
    (*) (width + config.xMargin)


compareColumnHeights : Int -> ( Int, Int, Int ) -> ( Int, Int, Int )
compareColumnHeights height ( index, minIndex, minHeight ) =
    if height < minHeight then
        ( index + 1, index, height )
    else
        ( index + 1, minIndex, minHeight )
