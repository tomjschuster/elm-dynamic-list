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
    Random.pair
        (Config.getWidthRange config |> uncurry Random.int)
        (Config.getHeightRange config |> uncurry Random.int)
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
        [ header [ Attr.class "title" ] [ h1 [] [ text "Elm Dynamic List" ] ]
        , main_ []
            [ controlPanel model
            , items model
            ]
        , footer [] [ code [] [ text <| toString model ] ]
        ]


controlPanel : Model -> Html Msg
controlPanel model =
    div [ Attr.class "control-panel" ]
        [ div [ Attr.class "generate-items" ]
            [ h2 [] [ text "Generate Items" ]
            , itemGenerator model.randomItemCount
            ]
        , div [ Attr.class "item-options" ]
            [ h2 [ Attr.class "options-title" ] [ text "Options" ]
            , div [ Attr.class "options-categories" ]
                [ div [ Attr.class "options-category item-width" ]
                    [ h3 [] [ text "Width" ]
                    , widthModeControl model.config.widthMode
                    , widthFields model.config
                    ]
                , div [ Attr.class "options-category item-height" ]
                    [ h3 [] [ text "Height" ]
                    , heightModeControl model.config.heightMode
                    , heightFields model.config
                    ]
                , div [ Attr.class "options-category item-margins" ]
                    [ h3 [] [ text "Margins" ]
                    , marginFields model.config
                    ]
                ]
            ]
        ]


itemGenerator : Maybe Int -> Html Msg
itemGenerator randomItemCount =
    div [ Attr.class "item-generator" ]
        [ button
            [ Events.onClick GenerateRandomItem ]
            [ text "Generate" ]
        , input
            [ randomItemCount |> viewMaybeInt |> Attr.value
            , Events.onInput UpdateRandomItemCount
            , Attr.type_ "number"
            , Attr.placeholder "12"
            ]
            []
        , button
            [ Events.onClick ClearItems ]
            [ text "Clear" ]
        ]


widthModeControl : Config.WidthMode -> Html Msg
widthModeControl widthMode =
    div [ Attr.class "mode-control" ]
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


heightModeControl : Config.HeightMode -> Html Msg
heightModeControl heightMode =
    div [ Attr.class "mode-control" ]
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


widthFields : Config -> Html Msg
widthFields config =
    case config.widthMode of
        Config.FixedWidth ->
            fixedWidthField config.fixedWidth

        Config.UnknownWidth ->
            randomWidthFields config


heightFields : Config -> Html Msg
heightFields config =
    case config.heightMode of
        Config.FixedHeight ->
            fixedHeightField config.fixedHeight

        Config.UnknownHeight ->
            randomHeightFields config


stringToMaybeInt : Maybe Int -> String -> Maybe Int
stringToMaybeInt default stringInt =
    if stringInt == "" then
        Nothing
    else
        String.toInt stringInt
            |> Result.map Just
            |> Result.withDefault default


updateIntField : (Maybe Int -> msg) -> Maybe Int -> String -> msg
updateIntField toMsg default stringInt =
    stringToMaybeInt default stringInt |> toMsg


updateConfigField : (Config -> Maybe Int) -> (Maybe Int -> msg) -> String -> msg
updateConfigField field toMsg =
    updateIntField toMsg (Config.default |> field)


fixedWidthField : Maybe Int -> Html Msg
fixedWidthField width =
    div [ Attr.class "fixed-field" ]
        [ input
            [ Attr.type_ "number"
            , Events.onInput
                (updateConfigField .fixedWidth Config.UpdateFixedWidth)
                |> Attr.map UpdateConfig
            , Attr.placeholder "240"
            , width |> viewMaybeInt |> Attr.value
            ]
            []
        ]


randomWidthFields : Config -> Html Msg
randomWidthFields config =
    div [ Attr.class "random-fields" ]
        [ div [ Attr.class "min-max-field" ]
            [ input
                [ Attr.type_ "number"
                , Events.onInput
                    (updateConfigField .minWidth Config.UpdateMinWidth)
                    |> Attr.map UpdateConfig
                , Attr.placeholder "40"
                , config.minWidth |> viewMaybeInt |> Attr.value
                ]
                []
            , label [] [ text "Min" ]
            ]
        , div [ Attr.class "min-max-field" ]
            [ input
                [ Attr.type_ "number"
                , Events.onInput
                    (updateConfigField .maxWidth Config.UpdateMaxWidth)
                    |> Attr.map UpdateConfig
                , Attr.placeholder "480"
                , config.maxWidth |> viewMaybeInt |> Attr.value
                ]
                []
            , label [] [ text "Max" ]
            ]
        ]


randomHeightFields : Config -> Html Msg
randomHeightFields config =
    div [ Attr.class "random-fields" ]
        [ div [ Attr.class "min-max-field" ]
            [ input
                [ Attr.type_ "number"
                , Events.onInput
                    (updateConfigField .minHeight Config.UpdateMinHeight)
                    |> Attr.map UpdateConfig
                , Attr.placeholder "40"
                , config.minHeight |> viewMaybeInt |> Attr.value
                ]
                []
            , label [] [ text "Min" ]
            ]
        , div [ Attr.class "min-max-field" ]
            [ input
                [ Attr.type_ "number"
                , Events.onInput
                    (updateConfigField .maxHeight Config.UpdateMaxHeight)
                    |> Attr.map UpdateConfig
                , Attr.placeholder "480"
                , config.maxHeight |> viewMaybeInt |> Attr.value
                ]
                []
            , label [] [ text "Max" ]
            ]
        ]


fixedHeightField : Maybe Int -> Html Msg
fixedHeightField height =
    div [ Attr.class "fixed-field" ]
        [ input
            [ Attr.type_ "number"
            , Events.onInput
                (updateConfigField .fixedHeight Config.UpdateFixedHeight)
                |> Attr.map UpdateConfig
            , Attr.placeholder "240"
            , height |> viewMaybeInt |> Attr.value
            ]
            []
        ]


marginFields : Config -> Html Msg
marginFields { xMargin, yMargin } =
    div [ Attr.class "margin-fields" ]
        [ div [ Attr.class "margin-field" ]
            [ input
                [ Attr.type_ "number"
                , Events.onInput
                    (updateConfigField .xMargin Config.UpdateXMargin)
                    |> Attr.map UpdateConfig
                , Attr.placeholder "12"
                , xMargin |> viewMaybeInt |> Attr.value
                ]
                []
            , label [] [ text "X" ]
            ]
        , div [ Attr.class "margin-field" ]
            [ input
                [ Attr.type_ "number"
                , Events.onInput
                    (updateConfigField .yMargin Config.UpdateXMargin)
                    |> Attr.map UpdateConfig
                , Attr.placeholder "12"
                , yMargin |> viewMaybeInt |> Attr.value
                ]
                []
            , label [] [ text "Y" ]
            ]
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
