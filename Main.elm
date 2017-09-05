port module Main exposing (main)

import Config exposing (Config)
import Html exposing (Html, button, code, div, fieldset, footer, h1, h2, h3, header, input, label, main_, p, text)
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


initialModel : Model
initialModel =
    { randomItemCount = Just defaultItemCount
    , config = Config.default
    , items = []
    , draggedItemId = Nothing
    }


init : ( Model, Cmd Msg )
init =
    initialModel
        => (Random.list
                defaultItemCount
                (randomItemGenerator initialModel.config)
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

        GenerateRandomItem ->
            model
                => (Random.list
                        (Maybe.withDefault
                            defaultItemCount
                            model.randomItemCount
                        )
                        (randomItemGenerator model.config)
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

        UpdateConfig configMsg ->
            { model | config = Config.update configMsg model.config } => Cmd.none

        SetDraggedItem id ->
            { model | draggedItemId = Just id } => Cmd.none

        ClearDraggedItem _ ->
            { model | draggedItemId = Nothing } => Cmd.none


tupleToDimensions : ( Int, Int ) -> Dimensions
tupleToDimensions ( height, width ) =
    Dimensions height width


randomItemGenerator : Config -> Random.Generator (Int -> Item)
randomItemGenerator config =
    let
        itemFixedWidth =
            Maybe.withDefault Config.defaultFixedWidth config.fixedWidth

        itemMinWidth =
            Maybe.withDefault Config.defaultMinWidth config.minWidth

        itemMaxWidth =
            Maybe.withDefault Config.defaultMaxWidth config.maxWidth

        itemFixedHeight =
            Maybe.withDefault Config.defaultFixedHeight config.fixedHeight

        itemMinHeight =
            Maybe.withDefault Config.defaultMinHeight config.minHeight

        itemMaxHeight =
            Maybe.withDefault Config.defaultMaxHeight config.maxHeight

        ( widthMin, widthMax ) =
            case config.widthMode of
                Config.FixedWidth ->
                    ( itemFixedWidth, itemFixedHeight )

                Config.UnknownWidth ->
                    ( itemMinWidth, itemMaxWidth )

        ( heightMax, heightMin ) =
            case config.heightMode of
                Config.FixedHeight ->
                    ( itemFixedHeight, itemFixedHeight )

                Config.UnknownHeight ->
                    ( itemMinHeight, itemMaxHeight )
    in
    Random.pair
        (Random.int widthMin widthMax)
        (Random.int heightMin heightMax)
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
            , items model
            ]
        , footer [] [ code [] [ text <| toString model ] ]
        ]


controlPanel : Model -> Html Msg
controlPanel model =
    div [ Attr.class "control-panel" ]
        [ fieldset []
            [ div [ Attr.class "item-generator" ]
                [ h2 [] [ text "Generate Items" ]
                , itemGenerator model.randomItemCount
                ]
            , div [ Attr.class "item-options" ]
                [ h2 [] [ text "Options" ]
                , div [ Attr.class "item-width" ]
                    [ h3 [] [ text "Width" ]
                    , widthModeControl model.config.widthMode
                    , widthFields model.config
                    ]
                , div [ Attr.class "item-height" ]
                    [ h3 [] [ text "Height" ]
                    , heightModeControl model.config.heightMode
                    , heightFields model.config
                    ]
                , div [ Attr.class "item-margins" ]
                    [ h3 [] [ text "Margins" ]
                    , marginFields model.config
                    ]
                ]
            ]
        ]


itemGenerator : Maybe Int -> Html Msg
itemGenerator randomItemCount =
    div []
        [ input
            [ randomItemCount |> viewMaybeInt |> Attr.value
            , Events.onInput UpdateRandomItemCount
            , Attr.type_ "number"
            , Attr.placeholder "12"
            ]
            []
        , button
            [ Events.onClick GenerateRandomItem ]
            [ text "Generate" ]
        , button
            [ Events.onClick ClearItems ]
            [ text "Clear" ]
        ]


widthModeControl : Config.WidthMode -> Html Msg
widthModeControl widthMode =
    div []
        [ label []
            [ input
                [ Attr.type_ "radio"
                , Events.onClick
                    (Config.UpdateWidthMode Config.FixedWidth)
                    |> Attr.map UpdateConfig
                , Attr.checked (widthMode == Config.FixedWidth)
                ]
                []
            , text "Fixed"
            ]
        , label []
            [ input
                [ Attr.type_ "radio"
                , Events.onClick
                    (Config.UpdateWidthMode Config.UnknownWidth)
                    |> Attr.map UpdateConfig
                , Attr.checked (widthMode == Config.UnknownWidth)
                ]
                []
            , text "Unknown"
            ]
        ]


widthFields : Config -> Html Msg
widthFields config =
    case config.widthMode of
        Config.FixedWidth ->
            fixedWidthField config.fixedWidth

        Config.UnknownWidth ->
            randomWidthFields config


fixedWidthField : Maybe Int -> Html Msg
fixedWidthField width =
    div [ Attr.class "fixed-field" ]
        [ input
            [ Attr.type_ "number"
            , Events.onInput Config.UpdateFixedWidth |> Attr.map UpdateConfig
            , Attr.placeholder "240"
            , width |> viewMaybeInt |> Attr.value
            ]
            []
        ]


randomWidthFields : Config -> Html Msg
randomWidthFields config =
    div []
        [ div [ Attr.class "min-max-field" ]
            [ label [] [ text "Min" ]
            , input
                [ Attr.type_ "number"
                , Events.onInput Config.UpdateMinWidth |> Attr.map UpdateConfig
                , Attr.placeholder "40"
                , config.minWidth |> viewMaybeInt |> Attr.value
                ]
                []
            ]
        , div [ Attr.class "min-max-field" ]
            [ label [] [ text "Max" ]
            , input
                [ Attr.type_ "number"
                , Events.onInput Config.UpdateMaxWidth |> Attr.map UpdateConfig
                , Attr.placeholder "480"
                , config.maxWidth |> viewMaybeInt |> Attr.value
                ]
                []
            ]
        ]


heightModeControl : Config.HeightMode -> Html Msg
heightModeControl heightMode =
    div []
        [ label []
            [ input
                [ Attr.type_ "radio"
                , Events.onClick
                    (Config.UpdateHeightMode Config.FixedHeight)
                    |> Attr.map UpdateConfig
                , Attr.checked (heightMode == Config.FixedHeight)
                ]
                []
            , text "Fixed"
            ]
        , label []
            [ input
                [ Attr.type_ "radio"
                , Events.onClick
                    (Config.UpdateHeightMode Config.UnknownHeight)
                    |> Attr.map UpdateConfig
                , Attr.checked (heightMode == Config.UnknownHeight)
                ]
                []
            , text "Unknown"
            ]
        ]


heightFields : Config -> Html Msg
heightFields config =
    case config.heightMode of
        Config.FixedHeight ->
            fixedHeightField config.fixedHeight

        Config.UnknownHeight ->
            randomHeightFields config


fixedHeightField : Maybe Int -> Html Msg
fixedHeightField height =
    div [ Attr.class "fixed-field" ]
        [ input
            [ Attr.type_ "number"
            , Events.onInput Config.UpdateFixedHeight |> Attr.map UpdateConfig
            , Attr.placeholder "240"
            , height |> viewMaybeInt |> Attr.value
            ]
            []
        ]


randomHeightFields : Config -> Html Msg
randomHeightFields config =
    div []
        [ div [ Attr.class "min-max-field" ]
            [ label [] [ text "Min" ]
            , input
                [ Attr.type_ "number"
                , Events.onInput Config.UpdateMinHeight |> Attr.map UpdateConfig
                , Attr.placeholder "40"
                , config.minHeight |> viewMaybeInt |> Attr.value
                ]
                []
            ]
        , div [ Attr.class "min-max-field" ]
            [ label [] [ text "Max" ]
            , input
                [ Attr.type_ "number"
                , Events.onInput Config.UpdateMaxHeight |> Attr.map UpdateConfig
                , Attr.placeholder "480"
                , config.maxHeight |> viewMaybeInt |> Attr.value
                ]
                []
            ]
        ]


marginFields : Config -> Html Msg
marginFields { xMargin, yMargin } =
    div []
        [ label [] [ text "X" ]
        , input
            [ Attr.type_ "number"
            , Events.onInput Config.UpdateXMargin |> Attr.map UpdateConfig
            , Attr.placeholder "12"
            , xMargin |> viewMaybeInt |> Attr.value
            ]
            []
        , label [] [ text "Y" ]
        , input
            [ Attr.type_ "number"
            , Events.onInput Config.UpdateYMargin |> Attr.map UpdateConfig
            , Attr.placeholder "12"
            , yMargin |> viewMaybeInt |> Attr.value
            ]
            []
        ]


items : Model -> Html Msg
items { config, draggedItemId, items } =
    div
        [ Attr.class "dynamic-list" ]
        (List.map (itemView config draggedItemId) items)


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
