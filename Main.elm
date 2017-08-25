module Main exposing (main)

import Html exposing (Html, button, code, div, footer, h1, header, input, main_, text)
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
    , items : List Item
    }


init : ( Model, Cmd Msg )
init =
    Model Nothing [] => (Random.list 12 itemGenerator |> Random.generate SetItems)


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model => Cmd.none

        UpdateGeneratorCount intString ->
            case String.toInt intString of
                Ok count ->
                    { model | generatorCount = Just count } => Cmd.none

                Err _ ->
                    if intString == "" then
                        { model | generatorCount = Nothing } => Cmd.none
                    else
                        model => Cmd.none

        GenerateRandomItem ->
            case model.generatorCount of
                Just generatorCount ->
                    { model | generatorCount = Just generatorCount }
                        => (Random.list generatorCount itemGenerator |> Random.generate SetItems)

                Nothing ->
                    { model | generatorCount = Nothing }
                        => Cmd.none

        SetItems items ->
            { model | items = items, generatorCount = Nothing } => Cmd.none

        ClearItems ->
            { model | items = [] } => Cmd.none


tupleToDimensions : ( Float, Float ) -> Dimensions
tupleToDimensions ( height, width ) =
    Dimensions height width


itemGenerator : Random.Generator Item
itemGenerator =
    Random.pair
        (Random.float 50 500)
        (Random.float 240 240)
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
        [ header
            []
            [ h1 [] [ text "Elm Dynamic List" ]
            ]
        , main_
            []
            [ div
                []
                [ input
                    [ model.generatorCount
                        |> displayGeneratorCount
                        |> Attr.value
                    , Events.onInput UpdateGeneratorCount
                    , Attr.type_ "number"
                    ]
                    []
                , button
                    [ Events.onClick GenerateRandomItem
                    , Attr.disabled (model.generatorCount == Nothing)
                    ]
                    [ text "Generate Items" ]
                , button
                    [ Events.onClick ClearItems
                    , Attr.disabled (List.isEmpty model.items)
                    ]
                    [ text "Clear Items" ]
                ]
            , div [] (List.map itemView model.items)
            ]
        , footer
            []
            [ code
                [ Attr.style [ ( "width", "500px" ), ( "display", "block" ) ] ]
                [ text <| toString model ]
            ]
        ]


itemView : Item -> Html Msg
itemView { dimensions } =
    div
        [ ( "border", "1px solid black" )
            :: ( "display", "inline-block" )
            :: dimensionsStyle dimensions
            |> Attr.style
        ]
        []


dimensionsStyle : Dimensions -> List ( String, String )
dimensionsStyle { height, width } =
    [ ( "height", toString height ++ "px" )
    , ( "width", toString width ++ "px" )
    ]


displayGeneratorCount : Maybe Int -> String
displayGeneratorCount generatorCount =
    generatorCount
        |> Maybe.map toString
        |> Maybe.withDefault ""
