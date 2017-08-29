module Main exposing (main)

import Config exposing (Config)
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
    , config : Config
    , items : List Item
    }


init : ( Model, Cmd Msg )
init =
    Model (Just 12) Config.default []
        => (Random.list 12 (itemGenerator Config.default) |> Random.generate SetItems)


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
    | UpdateConfig Config.Msg


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

        UpdateConfig configMsg ->
            { model | config = Config.update configMsg model.config } => Cmd.none

        GenerateRandomItem ->
            model
                => (Random.list
                        (Maybe.withDefault 12 model.generatorCount)
                        (itemGenerator model.config)
                        |> Random.generate SetItems
                   )

        SetItems items ->
            { model | items = items } => Cmd.none

        ClearItems ->
            { model | items = [] } => Cmd.none


tupleToDimensions : ( Float, Float ) -> Dimensions
tupleToDimensions ( height, width ) =
    Dimensions height width


itemGenerator : Config -> Random.Generator Item
itemGenerator config =
    let
        width =
            Config.defaultWidth |> toFloat
    in
    Random.pair
        (Random.float 50 500)
        (Random.float width width)
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
            , div [] (List.map (itemView model.config) model.items)
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
                    , Events.onInput Config.UpdateWidth |> Attr.map UpdateConfig
                    , Attr.placeholder "240"
                    , model.config.width
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
                    , Events.onInput Config.UpdateXMargin |> Attr.map UpdateConfig
                    , Attr.placeholder "12"
                    , model.config.xMargin
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
                    , Events.onInput Config.UpdateYMargin |> Attr.map UpdateConfig
                    , Attr.placeholder "12"
                    , model.config.yMargin
                        |> viewMaybeInt
                        |> Attr.value
                    ]
                    []
                ]
            ]
        ]


itemView : Config -> Item -> Html Msg
itemView config { dimensions } =
    div
        [ Attr.style
            [ ( "border", "1px solid black" )
            , ( "display", "inline-block" )
            , ( "margin"
              , maybeIntToPx config.yMargin Config.defaultYMargin
                    ++ " "
                    ++ maybeIntToPx config.xMargin Config.defaultXMargin
              )
            , ( "width", maybeIntToPx config.width Config.defaultWidth )
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
