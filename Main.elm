module Main exposing (main)

import Html exposing (Html, button, code, div, fieldset, footer, h1, h2, header, input, label, main_, p, text)
import Html.Attributes as Attr
import Html.Events as Events
import Random


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
infixr 9 =>



-- MODEL


type alias Model =
    { generatorCount : Maybe Int
    , layout : Layout
    , items : List Item
    }


init : ( Model, Cmd Msg )
init =
    Model (Just 12) defaultLayout []
        => (Random.list 12 (itemGenerator defaultLayout) |> Random.generate SetItems)


type alias Layout =
    { columnWidth : Maybe Int
    , xMargin : Maybe Int
    , yMargin : Maybe Int
    }


defaultLayout : Layout
defaultLayout =
    Layout (Just 240) (Just 12) (Just 12)


type alias Item =
    { dimensions : Dimensions
    }


type alias Dimensions =
    { height : Float
    , width : Float
    }



-- UPDATE


type Msg
    = NoOp
    | UpdateGeneratorCount String
    | GenerateRandomItem
    | SetItems (List Item)
    | ClearItems
    | UpdateColumnWidth String
    | UpdateXMargin String
    | UpdateYMargin String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model => Cmd.none

        UpdateGeneratorCount intString ->
            let
                generatorCount =
                    if intString == "" then
                        Nothing
                    else
                        Result.withDefault 12 (String.toInt intString) |> Just
            in
            { model | generatorCount = generatorCount } => Cmd.none

        UpdateColumnWidth columnWidthStr ->
            let
                { layout } =
                    model

                columnWidth =
                    if columnWidthStr == "" then
                        Nothing
                    else
                        Just (Result.withDefault 240 (String.toInt columnWidthStr))

                updatedLayout =
                    { layout | columnWidth = columnWidth }
            in
            { model | layout = updatedLayout } => Cmd.none

        UpdateXMargin xMarginStr ->
            let
                { layout } =
                    model

                xMargin =
                    if xMarginStr == "" then
                        Nothing
                    else
                        Just
                            (Result.withDefault 0 (String.toInt xMarginStr))

                updatedLayout =
                    { layout | xMargin = xMargin }
            in
            { model | layout = updatedLayout } => Cmd.none

        UpdateYMargin yMarginStr ->
            let
                { layout } =
                    model

                yMargin =
                    if yMarginStr == "" then
                        Nothing
                    else
                        Just
                            (Result.withDefault 0 (String.toInt yMarginStr))

                updatedLayout =
                    { layout | yMargin = yMargin }
            in
            { model | layout = updatedLayout } => Cmd.none

        GenerateRandomItem ->
            model
                => (Random.list
                        (Maybe.withDefault 12 model.generatorCount)
                        (itemGenerator model.layout)
                        |> Random.generate SetItems
                   )

        SetItems items ->
            { model | items = items } => Cmd.none

        ClearItems ->
            { model | items = [] } => Cmd.none


tupleToDimensions : ( Float, Float ) -> Dimensions
tupleToDimensions ( height, width ) =
    Dimensions height width


itemGenerator : Layout -> Random.Generator Item
itemGenerator layout =
    Random.pair
        (Random.float 50 500)
        (Random.float (toFloat (Maybe.withDefault 240 layout.columnWidth)) (toFloat (Maybe.withDefault 240 layout.columnWidth)))
        |> Random.map (tupleToDimensions >> Item)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ header []
            [ h1 [] [ text "Elm Dynamic List" ] ]
        , main_ []
            [ controlPanel model
            , div [] (List.map (itemView model.layout) model.items)
            ]
        , footer []
            [ code
                [ Attr.style [ ( "width", "500px" ), ( "display", "block" ) ] ]
                [ text <| toString model ]
            ]
        ]


controlPanel : Model -> Html Msg
controlPanel model =
    div []
        [ fieldset [ Attr.style [ ( "display", "inline-block" ) ] ]
            [ div []
                [ label [ Attr.style [ ( "display", "block" ) ] ] [ text "Item Generator" ]
                , input
                    [ model.generatorCount
                        |> viewMaybeInt
                        |> Attr.value
                    , Events.onInput UpdateGeneratorCount
                    , Attr.type_ "number"
                    , Attr.style [ ( "width", "50px" ) ]
                    , Attr.placeholder "12"
                    ]
                    []
                , button
                    [ Events.onClick GenerateRandomItem
                    ]
                    [ text "Generate" ]
                , button
                    [ Events.onClick ClearItems
                    , Attr.disabled (List.isEmpty model.items)
                    ]
                    [ text "Clear" ]
                ]
            ]
        , fieldset [ Attr.style [ ( "display", "inline-block" ) ] ]
            [ div [ Attr.style [ ( "display", "inline-block" ) ] ]
                [ label [ Attr.style [ ( "display", "block" ) ] ] [ text "Column Width" ]
                , input
                    [ Attr.style [ ( "width", "50px" ) ]
                    , Attr.type_ "number"
                    , Events.onInput UpdateColumnWidth
                    , Attr.placeholder "240"
                    , model.layout.columnWidth
                        |> viewMaybeInt
                        |> Attr.value
                    ]
                    []
                ]
            , div [ Attr.style [ ( "display", "inline-block" ) ] ]
                [ label [ Attr.style [ ( "display", "block" ) ] ] [ text "X Margin" ]
                , input
                    [ Attr.style [ ( "width", "50px" ) ]
                    , Attr.type_ "number"
                    , Events.onInput UpdateXMargin
                    , Attr.placeholder "12"
                    , model.layout.xMargin
                        |> viewMaybeInt
                        |> Attr.value
                    ]
                    []
                ]
            , div [ Attr.style [ ( "display", "inline-block" ) ] ]
                [ label [ Attr.style [ ( "display", "block" ) ] ] [ text "Y Margin" ]
                , input
                    [ Attr.style [ ( "width", "50px" ) ]
                    , Attr.type_ "number"
                    , Events.onInput UpdateYMargin
                    , Attr.placeholder "12"
                    , model.layout.yMargin
                        |> viewMaybeInt
                        |> Attr.value
                    ]
                    []
                ]
            ]
        ]


itemView : Layout -> Item -> Html Msg
itemView layout { dimensions } =
    div
        [ Attr.style
            [ ( "border", "1px solid black" )
            , ( "display", "inline-block" )
            , ( "margin", maybeIntToPx layout.yMargin 12 ++ " " ++ maybeIntToPx layout.xMargin 12 )
            , ( "width", maybeIntToPx layout.columnWidth 240 )
            , ( "height", toString dimensions.height ++ "px" )
            ]
        ]
        []


dimensionsStyle : Dimensions -> List ( String, String )
dimensionsStyle { height, width } =
    [ ( "height", toString height ++ "px" )
    , ( "width", toString width ++ "px" )
    ]


viewMaybeInt : Maybe Int -> String
viewMaybeInt maybeInt =
    maybeInt
        |> Maybe.map toString
        |> Maybe.withDefault ""


maybeIntToPx : Maybe Int -> Int -> String
maybeIntToPx maybeInt default =
    maybeInt
        |> Maybe.withDefault default
        |> toString
        |> flip (++) "px"
