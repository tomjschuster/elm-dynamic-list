module DynamicList
    exposing
        ( Config
        , Dimensions
        , DynamicList
        , Item
        , ListType(..)
        , Translation
        , view
        )

import Array exposing (Array)
import Html exposing (Html, div)
import Html.Attributes exposing (style)


type alias DynamicList msg =
    { config : Config, items : List (Item msg) }


type alias Config =
    { listType : ListType
    , xMargin : Int
    , yMargin : Int
    , columns : Int
    }


type alias Item msg =
    { dimensions : Dimensions
    , content : Html msg
    }


type ListType
    = FixedWidth Int
    | FixedHeight Int


type alias Dimensions =
    { width : Int, height : Int }


type alias Translation =
    { x : Int, y : Int }


view : DynamicList msg -> Html msg
view { config, items } =
    let
        translations =
            List.map .dimensions items |> getTranslations config

        a =
            Debug.log "translations" translations
    in
    div
        []
        (List.map2 (itemView config) items translations)


itemView : Config -> Item msg -> Translation -> Html msg
itemView config { content } translation =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "transform", translationStyle translation )
            ]
        ]
        [ content ]


translationStyle : Translation -> String
translationStyle { x, y } =
    "translate(" ++ intToPx x ++ ", " ++ intToPx y ++ ")"


intToPx : Int -> String
intToPx =
    toString >> flip (++) "px"


getTranslations : Config -> List Dimensions -> List Translation
getTranslations config dimensions =
    ( Array.repeat config.columns 0, [] )
        |> getTranslationHelper config dimensions
        |> Tuple.second


getTranslationHelper :
    Config
    -> List Dimensions
    -> ( Array Int, List Translation )
    -> ( Array Int, List Translation )
getTranslationHelper config dimensions ( columnHeights, translations ) =
    case config.listType of
        FixedWidth width ->
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
                            Translation (getTranslateX config width minIndex) minHeight
                                |> List.singleton
                                |> List.append translations
                    in
                    getTranslationHelper
                        config
                        rest
                        ( newColumnHeights, newTranslations )

        _ ->
            ( Array.empty, [] )


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
getTranslateX config width x =
    let
        a =
            Debug.log "config" x
    in
    (*) (width + config.xMargin) x


compareColumnHeights : Int -> ( Int, Int, Int ) -> ( Int, Int, Int )
compareColumnHeights height ( index, minIndex, minHeight ) =
    if height < minHeight then
        ( index + 1, index, height )
    else
        ( index + 1, minIndex, minHeight )
