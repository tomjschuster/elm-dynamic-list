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
    Model (Just defaultItemCount) Config.default [] Nothing
        => (Random.list
                defaultItemCount
                (itemGenerator Config.default)
                |> Random.generate SetItems
           )


type alias Item =
    { dimensions : Dimensions
    , id : Int
    }


defaultItemCount : Int
defaultItemCount =
    12


type alias Dimensions =
    { width : Int
    , height : Int
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
                        Result.withDefault
                            defaultItemCount
                            (String.toInt intString)
                            |> Just
            in
            { model | randomItemCount = randomItemCount } => Cmd.none

        UpdateConfig configMsg ->
            { model | config = Config.update configMsg model.config } => Cmd.none

        GenerateRandomItem ->
            model
                => (Random.list
                        (Maybe.withDefault
                            defaultItemCount
                            model.randomItemCount
                        )
                        (itemGenerator model.config)
                        |> Random.generate SetItems
                   )

        SetItems toItems ->
            let
                items =
                    List.indexedMap (\idx toItem -> toItem (idx + 1)) toItems
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
itemGenerator { width, minWidth, maxWidth, height, minHeight, maxHeight } =
    Random.pair
        (Random.int
            (dimensionRangeValue Config.defaultMinWidth width minWidth)
            (dimensionRangeValue Config.defaultMaxWidth width maxWidth)
        )
        (Random.int
            (dimensionRangeValue Config.defaultMinHeight height minHeight)
            (dimensionRangeValue Config.defaultMaxHeight height maxHeight)
        )
        |> Random.map (tupleToDimensions >> Item)


dimensionRangeValue : Int -> Maybe Int -> Maybe Int -> Int
dimensionRangeValue default fixedValue rangeValue =
    case ( fixedValue, rangeValue ) of
        ( Nothing, Nothing ) ->
            default

        ( Just int, _ ) ->
            int

        ( _, Just int ) ->
            int



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
            , div
                [ Attr.class "dynamic-list" ]
                (List.map (itemView model.config model.draggedItemId) model.items)
            ]
        , footer [] [ code [] [ text <| toString model ] ]
        ]


controlPanel : Model -> Html Msg
controlPanel model =
    div [ Attr.class "control-panel" ]
        [ fieldset []
            [ h2 [] [ text "Control Panel" ]
            , div []
                [ div [ Attr.class "generate-items" ]
                    [ button
                        [ Events.onClick GenerateRandomItem ]
                        [ text "Create Items" ]
                    ]
                , input
                    [ model.randomItemCount |> viewMaybeInt |> Attr.value
                    , Events.onInput UpdateRandomItemCount
                    , Attr.type_ "number"
                    , Attr.placeholder "12"
                    ]
                    []
                , button
                    [ Events.onClick ClearItems ]
                    [ text "Clear" ]
                ]
            , div []
                [ label [] [ text "Fixed Width" ]
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
                [ label [] [ text "Min Width" ]
                , input
                    [ Attr.type_ "number"
                    , Events.onInput Config.UpdateMinWidth |> Attr.map UpdateConfig
                    , Attr.placeholder "40"
                    , Attr.disabled (model.config.width /= Nothing)
                    , model.config.minWidth |> viewMaybeInt |> Attr.value
                    ]
                    []
                ]
            , div []
                [ label [] [ text "Max Width" ]
                , input
                    [ Attr.type_ "number"
                    , Events.onInput Config.UpdateMaxWidth |> Attr.map UpdateConfig
                    , Attr.placeholder "480"
                    , Attr.disabled (model.config.width /= Nothing)
                    , model.config.maxWidth |> viewMaybeInt |> Attr.value
                    ]
                    []
                ]
            , div []
                [ label [] [ text "Fixed Height" ]
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
                , div []
                    [ label [] [ text "Min Height" ]
                    , input
                        [ Attr.type_ "number"
                        , Events.onInput Config.UpdateMinHeight |> Attr.map UpdateConfig
                        , Attr.placeholder "40"
                        , Attr.disabled (model.config.height /= Nothing)
                        , model.config.minHeight |> viewMaybeInt |> Attr.value
                        ]
                        []
                    ]
                , div []
                    [ label [] [ text "Max Height" ]
                    , input
                        [ Attr.type_ "number"
                        , Events.onInput Config.UpdateMaxHeight |> Attr.map UpdateConfig
                        , Attr.placeholder "480"
                        , Attr.disabled (model.config.height /= Nothing)
                        , model.config.maxHeight |> viewMaybeInt |> Attr.value
                        ]
                        []
                    ]
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
