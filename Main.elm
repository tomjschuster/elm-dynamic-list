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
    , widthMode : WidthMode
    , heightMode : HeightMode
    , items : List Item
    , draggedItemId : Maybe Int
    }


initialModel : Model
initialModel =
    { randomItemCount = Just defaultItemCount
    , config = Config.default
    , widthMode = FixedWidth
    , heightMode = UnknownHeight
    , items = []
    , draggedItemId = Nothing
    }


init : ( Model, Cmd Msg )
init =
    initialModel
        => (Random.list
                defaultItemCount
                (randomItemGenerator initialModel)
                |> Random.generate SetItems
           )


type WidthMode
    = FixedWidth
    | UnknownWidth


type HeightMode
    = FixedHeight
    | UnknownHeight


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
    | UpdateWidthMode WidthMode
    | UpdateHeightMode HeightMode
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
                        (randomItemGenerator model)
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

        UpdateHeightMode heightMode ->
            { model | heightMode = heightMode } => Cmd.none

        UpdateWidthMode widthMode ->
            { model | widthMode = widthMode } => Cmd.none

        UpdateConfig configMsg ->
            { model | config = Config.update configMsg model.config } => Cmd.none

        SetDraggedItem id ->
            { model | draggedItemId = Just id } => Cmd.none

        ClearDraggedItem _ ->
            { model | draggedItemId = Nothing } => Cmd.none


tupleToDimensions : ( Int, Int ) -> Dimensions
tupleToDimensions ( height, width ) =
    Dimensions height width


randomItemGenerator : Model -> Random.Generator (Int -> Item)
randomItemGenerator { widthMode, heightMode, config } =
    let
        itemWidth =
            Maybe.withDefault Config.defaultWidth config.width

        itemMinWidth =
            Maybe.withDefault Config.defaultMinWidth config.minWidth

        itemMaxWidth =
            Maybe.withDefault Config.defaultMaxWidth config.maxWidth

        itemHeight =
            Maybe.withDefault Config.defaultHeight config.height

        itemMinHeight =
            Maybe.withDefault Config.defaultMinHeight config.minHeight

        itemMaxHeight =
            Maybe.withDefault Config.defaultMaxHeight config.maxHeight

        ( widthMin, widthMax ) =
            case widthMode of
                FixedWidth ->
                    ( itemWidth, itemHeight )

                UnknownWidth ->
                    ( itemMinWidth, itemMaxWidth )

        ( heightMax, heightMin ) =
            case heightMode of
                FixedHeight ->
                    ( itemHeight, itemHeight )

                UnknownHeight ->
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
                    , widthModeControl model.widthMode
                    , widthFields model.widthMode model.config
                    ]
                , div [ Attr.class "item-height" ]
                    [ h3 [] [ text "Height" ]
                    , heightModeControl model.heightMode
                    , heightFields model.heightMode model.config
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


widthModeControl : WidthMode -> Html Msg
widthModeControl widthMode =
    div []
        [ label []
            [ input
                [ Attr.type_ "radio"
                , Events.onClick (UpdateWidthMode FixedWidth)
                , Attr.checked (widthMode == FixedWidth)
                ]
                []
            , text "Fixed"
            ]
        , label []
            [ input
                [ Attr.type_ "radio"
                , Events.onClick (UpdateWidthMode UnknownWidth)
                , Attr.checked (widthMode == UnknownWidth)
                ]
                []
            , text "Unknown"
            ]
        ]


widthFields : WidthMode -> Config -> Html Msg
widthFields widthMode config =
    case widthMode of
        FixedWidth ->
            fixedWidthField config.width

        UnknownWidth ->
            randomWidthFields config


fixedWidthField : Maybe Int -> Html Msg
fixedWidthField width =
    div [ Attr.class "fixed-field" ]
        [ input
            [ Attr.type_ "number"
            , Events.onInput Config.UpdateWidth |> Attr.map UpdateConfig
            , Attr.placeholder "240"
            , width |> viewMaybeInt |> Attr.value
            ]
            []
        ]


randomWidthFields : Config -> Html Msg
randomWidthFields { width, minWidth, maxWidth } =
    div []
        [ div [ Attr.class "min-max-field" ]
            [ label [] [ text "Min" ]
            , input
                [ Attr.type_ "number"
                , Events.onInput Config.UpdateMinWidth |> Attr.map UpdateConfig
                , Attr.placeholder "40"
                , minWidth |> viewMaybeInt |> Attr.value
                ]
                []
            ]
        , div [ Attr.class "min-max-field" ]
            [ label [] [ text "Max" ]
            , input
                [ Attr.type_ "number"
                , Events.onInput Config.UpdateMaxWidth |> Attr.map UpdateConfig
                , Attr.placeholder "480"
                , maxWidth |> viewMaybeInt |> Attr.value
                ]
                []
            ]
        ]


heightModeControl : HeightMode -> Html Msg
heightModeControl heightMode =
    div []
        [ label []
            [ input
                [ Attr.type_ "radio"
                , Events.onClick (UpdateHeightMode FixedHeight)
                , Attr.checked (heightMode == FixedHeight)
                ]
                []
            , text "Fixed"
            ]
        , label []
            [ input
                [ Attr.type_ "radio"
                , Events.onClick (UpdateHeightMode UnknownHeight)
                , Attr.checked (heightMode == UnknownHeight)
                ]
                []
            , text "Unknown"
            ]
        ]


heightFields : HeightMode -> Config -> Html Msg
heightFields heightMode config =
    case heightMode of
        FixedHeight ->
            fixedHeightField config.height

        UnknownHeight ->
            randomHeightFields config


fixedHeightField : Maybe Int -> Html Msg
fixedHeightField height =
    div [ Attr.class "fixed-field" ]
        [ input
            [ Attr.type_ "number"
            , Events.onInput Config.UpdateHeight |> Attr.map UpdateConfig
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
