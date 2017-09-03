port module Main exposing (main)

import Config exposing (Config)
import Html exposing (Html, button, code, div, fieldset, footer, h1, h2, header, input, label, main_, p, text)
import Html.Attributes as Attr
import Html.Events as Events
import Mouse
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
    { randomItemCount : Maybe Int
    , config : Config
    , items : List Item
    , draggedItemId : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    Model (Just 12) Config.default [] Nothing
        => (Random.list 12 (itemGenerator Config.default) |> Random.generate SetItems)


type alias Item =
    { dimensions : Dimensions
    , id : Int
    }


type alias Dimensions =
    { height : Int
    , width : Int
    }



-- UPDATE


type Msg
    = NoOp
    | UpdateRandomItemCount String
    | GenerateRandomItem
    | SetItems (List (Int -> Item))
    | ClearItems
    | UpdateConfig Config.Msg
    | SetDraggedItem Int
    | ClearDraggedItem Mouse.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model => Cmd.none

        UpdateRandomItemCount intString ->
            let
                randomItemCount =
                    if intString == "" then
                        Nothing
                    else
                        Result.withDefault 12 (String.toInt intString) |> Just
            in
            { model | randomItemCount = randomItemCount } => Cmd.none

        UpdateConfig configMsg ->
            { model | config = Config.update configMsg model.config } => Cmd.none

        GenerateRandomItem ->
            model
                => (Random.list
                        (Maybe.withDefault 12 model.randomItemCount)
                        (itemGenerator model.config)
                        |> Random.generate SetItems
                   )

        SetItems toItems ->
            let
                ( _, items ) =
                    List.foldl (\toItem ( id, items ) -> ( id - 1, toItem id :: items )) ( List.length toItems, [] ) toItems
            in
            { model | items = items } => Cmd.none

        ClearItems ->
            { model | items = [] } => Cmd.none

        SetDraggedItem id ->
            { model | draggedItemId = Just id } => Cmd.none

        ClearDraggedItem _ ->
            { model | draggedItemId = Nothing } => Cmd.none


tupleToDimensions : ( Int, Int ) -> Dimensions
tupleToDimensions ( height, width ) =
    Dimensions height width


itemGenerator : Config -> Random.Generator (Int -> Item)
itemGenerator config =
    let
        width =
            Config.defaultWidth |> toFloat
    in
    Random.pair
        (Random.int (Maybe.withDefault 40 config.height) (Maybe.withDefault 480 config.height))
        (Random.int (Maybe.withDefault 40 config.width) (Maybe.withDefault 480 config.width))
        |> Random.map (tupleToDimensions >> Item)



-- SUBSCRIPTIONS


port mouseLeaves : (Mouse.Position -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions { draggedItemId } =
    if draggedItemId == Nothing then
        Sub.none
    else
        Sub.batch
            [ Mouse.ups ClearDraggedItem
            , mouseLeaves ClearDraggedItem
            ]



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ header [] [ h1 [] [ text "Elm Dynamic List" ] ]
        , main_ []
            [ controlPanel model
            , div [] (List.map (itemView model.config model.draggedItemId) model.items)
            ]
        , footer [] [ code [] [ text <| toString model ] ]
        ]


controlPanel : Model -> Html Msg
controlPanel model =
    div [ Attr.class "control-panel" ]
        [ fieldset [ Attr.class "item-generator" ]
            [ div []
                [ label [] [ text "Item Generator" ]
                , input
                    [ model.randomItemCount |> viewMaybeInt |> Attr.value
                    , Events.onInput UpdateRandomItemCount
                    , Attr.type_ "number"
                    , Attr.placeholder "12"
                    ]
                    []
                , div []
                    [ button
                        [ Events.onClick GenerateRandomItem ]
                        [ text "Generate" ]
                    ]
                , div []
                    [ button
                        [ Events.onClick ClearItems ]
                        [ text "Clear" ]
                    ]
                ]
            ]
        , fieldset []
            [ div []
                [ label [] [ text "Width" ]
                , input
                    [ Attr.type_ "number"
                    , Events.onInput Config.UpdateWidth |> Attr.map UpdateConfig
                    , Attr.placeholder "40 - 480"
                    , Attr.disabled (model.config.width == Nothing)
                    , model.config.width |> viewMaybeInt |> Attr.value
                    ]
                    []
                , input
                    [ Attr.type_ "checkbox"
                    , Attr.checked (model.config.width /= Nothing)
                    , Events.onClick Config.ToggleWidth |> Attr.map UpdateConfig
                    ]
                    []
                ]
            , div []
                [ label [] [ text "Height" ]
                , input
                    [ Attr.type_ "number"
                    , Events.onInput Config.UpdateHeight |> Attr.map UpdateConfig
                    , Attr.placeholder "40-480"
                    , Attr.disabled (model.config.height == Nothing)
                    , model.config.height |> viewMaybeInt |> Attr.value
                    ]
                    []
                , input
                    [ Attr.type_ "checkbox"
                    , Attr.checked (model.config.height /= Nothing)
                    , Events.onClick Config.ToggleHeight |> Attr.map UpdateConfig
                    ]
                    []
                ]
            , div []
                [ label [] [ text "X Margin" ]
                , input
                    [ Attr.type_ "number"
                    , Events.onInput Config.UpdateXMargin |> Attr.map UpdateConfig
                    , Attr.placeholder "12"
                    , model.config.xMargin |> viewMaybeInt |> Attr.value
                    ]
                    []
                ]
            , div []
                [ label [] [ text "Y Margin" ]
                , input
                    [ Attr.type_ "number"
                    , Events.onInput Config.UpdateYMargin |> Attr.map UpdateConfig
                    , Attr.placeholder "12"
                    , model.config.yMargin |> viewMaybeInt |> Attr.value
                    ]
                    []
                ]
            ]
        ]


compareIds : Maybe Int -> Int -> Bool
compareIds maybeId currentId =
    case maybeId of
        Just id ->
            id == currentId

        Nothing ->
            False


itemView : Config -> Maybe Int -> Item -> Html Msg
itemView config draggedItemId { dimensions, id } =
    div
        [ Attr.classList [ ( "item", True ), ( "dragged", compareIds draggedItemId id ) ]
        , itemStyle config dimensions
        , Events.onMouseDown (SetDraggedItem id)
        ]
        []


itemStyle : Config -> Dimensions -> Html.Attribute Msg
itemStyle config { width, height } =
    Attr.style
        [ ( "margin"
          , maybeIntToPx config.yMargin Config.defaultYMargin
                ++ " "
                ++ maybeIntToPx config.xMargin Config.defaultXMargin
          )
        , ( "width", width |> toString |> flip (++) "px" )
        , ( "height", height |> toString |> flip (++) "px" )
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
