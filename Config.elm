module Config
    exposing
        ( Config
        , ExternalMsg(..)
        , HeightMode(..)
        , Msg
        , WidthMode(..)
        , controlPanel
        , default
        , defaultItemCount
        , defaultXMargin
        , defaultYMargin
        , getHeightRange
        , getWidthRange
        , update
        )

import Html exposing (Html, button, code, div, fieldset, footer, h1, h2, h3, header, input, label, main_, p, text)
import Html.Attributes as Attr
import Html.Events as Events
import Utils exposing ((=>))


type alias Config =
    { itemCount : Maybe Int
    , widthMode : WidthMode
    , fixedWidth : Maybe Int
    , minWidth : Maybe Int
    , maxWidth : Maybe Int
    , heightMode : HeightMode
    , fixedHeight : Maybe Int
    , minHeight : Maybe Int
    , maxHeight : Maybe Int
    , xMargin : Maybe Int
    , yMargin : Maybe Int
    }


type WidthMode
    = FixedWidth
    | UnknownWidth


type HeightMode
    = FixedHeight
    | UnknownHeight


default : Config
default =
    { itemCount = Nothing
    , widthMode = FixedWidth
    , fixedWidth = Nothing
    , minWidth = Nothing
    , maxWidth = Nothing
    , heightMode = UnknownHeight
    , fixedHeight = Nothing
    , minHeight = Nothing
    , maxHeight = Nothing
    , xMargin = Nothing
    , yMargin = Nothing
    }


defaultItemCount : Int
defaultItemCount =
    12


defaultFixedWidth : Int
defaultFixedWidth =
    240


defaultMinWidth : Int
defaultMinWidth =
    40


defaultMaxWidth : Int
defaultMaxWidth =
    480


defaultFixedHeight : Int
defaultFixedHeight =
    240


defaultMinHeight : Int
defaultMinHeight =
    40


defaultMaxHeight : Int
defaultMaxHeight =
    480


defaultXMargin : Int
defaultXMargin =
    12


defaultYMargin : Int
defaultYMargin =
    12


type ExternalMsg
    = NoOp
    | Close
    | GenerateItems (Maybe Int)
    | ClearItems


type Msg
    = NoOpMsg
    | CloseMsg
    | GenerateItemsMsg (Maybe Int)
    | ClearItemsMsg
    | UpdateItemCount (Maybe Int)
    | UpdateWidthMode WidthMode
    | UpdateFixedWidth (Maybe Int)
    | UpdateMinWidth (Maybe Int)
    | UpdateMaxWidth (Maybe Int)
    | UpdateHeightMode HeightMode
    | UpdateFixedHeight (Maybe Int)
    | UpdateMinHeight (Maybe Int)
    | UpdateMaxHeight (Maybe Int)
    | UpdateXMargin (Maybe Int)
    | UpdateYMargin (Maybe Int)


update : Msg -> Config -> ( Config, ExternalMsg )
update msg config =
    case msg of
        NoOpMsg ->
            config => NoOp

        CloseMsg ->
            config => Close

        GenerateItemsMsg maybeCount ->
            config => GenerateItems maybeCount

        ClearItemsMsg ->
            config => ClearItems

        UpdateItemCount itemCount ->
            { config | itemCount = itemCount } => NoOp

        UpdateWidthMode widthMode ->
            { config | widthMode = widthMode } => NoOp

        UpdateFixedWidth fixedWidth ->
            { config | fixedWidth = fixedWidth } => NoOp

        UpdateMinWidth minWidth ->
            { config | minWidth = minWidth } => NoOp

        UpdateMaxWidth maxWidth ->
            { config | maxWidth = maxWidth } => NoOp

        UpdateHeightMode heightMode ->
            { config | heightMode = heightMode } => NoOp

        UpdateFixedHeight fixedHeight ->
            { config | fixedHeight = fixedHeight } => NoOp

        UpdateMinHeight minHeight ->
            { config | minHeight = minHeight } => NoOp

        UpdateMaxHeight maxHeight ->
            { config | maxHeight = maxHeight } => NoOp

        UpdateXMargin xMargin ->
            { config | xMargin = xMargin } => NoOp

        UpdateYMargin yMargin ->
            { config | yMargin = yMargin } => NoOp


getWidthRange : Config -> ( Int, Int )
getWidthRange config =
    case config.widthMode of
        FixedWidth ->
            ( Maybe.withDefault defaultFixedWidth config.fixedWidth
            , Maybe.withDefault defaultFixedWidth config.fixedWidth
            )

        UnknownWidth ->
            ( Maybe.withDefault defaultMinWidth config.minWidth
            , Maybe.withDefault defaultMaxWidth config.maxWidth
            )


getHeightRange : Config -> ( Int, Int )
getHeightRange config =
    case config.heightMode of
        FixedHeight ->
            ( Maybe.withDefault defaultFixedHeight config.fixedHeight
            , Maybe.withDefault defaultFixedHeight config.fixedHeight
            )

        UnknownHeight ->
            ( Maybe.withDefault defaultMinHeight config.minHeight
            , Maybe.withDefault defaultMaxHeight config.maxHeight
            )


controlPanel : Config -> Html Msg
controlPanel config =
    div [ Attr.class "control-panel" ]
        [ button
            [ Attr.class "hide-control-panel"
            , Events.onClick CloseMsg
            ]
            [ text "X" ]
        , div [ Attr.class "generate-items" ]
            [ h2 [] [ text "Generate Items" ]
            , itemGenerator config.itemCount
            ]
        , div [ Attr.class "item-options" ]
            [ h2 [ Attr.class "options-title" ] [ text "Options" ]
            , div [ Attr.class "options-categories" ]
                [ div [ Attr.class "options-category item-width" ]
                    [ h3 [] [ text "Width" ]
                    , widthModeControl config.widthMode
                    , widthFields config
                    ]
                , div [ Attr.class "options-category item-height" ]
                    [ h3 [] [ text "Height" ]
                    , heightModeControl config.heightMode
                    , heightFields config
                    ]
                , div [ Attr.class "options-category item-margins" ]
                    [ h3 [] [ text "Margins" ]
                    , marginFields config
                    ]
                ]
            ]
        ]


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
    updateIntField toMsg (default |> field)


itemGenerator : Maybe Int -> Html Msg
itemGenerator itemCount =
    div [ Attr.class "item-generator" ]
        [ button
            [ GenerateItemsMsg itemCount |> Events.onClick
            ]
            [ text "Generate" ]
        , input
            [ itemCount |> viewMaybeInt |> Attr.value
            , updateConfigField .itemCount UpdateItemCount |> Events.onInput
            , Attr.type_ "number"
            , Attr.placeholder "12"
            ]
            []
        , button
            [ Events.onClick ClearItemsMsg ]
            [ text "Clear" ]
        ]


widthModeControl : WidthMode -> Html Msg
widthModeControl widthMode =
    div [ Attr.class "mode-control" ]
        [ label []
            [ input
                [ Attr.type_ "radio"
                , UpdateWidthMode FixedWidth |> Events.onClick
                , Attr.checked (widthMode == FixedWidth)
                ]
                []
            , text "Fixed"
            ]
        , label []
            [ input
                [ Attr.type_ "radio"
                , UpdateWidthMode UnknownWidth |> Events.onClick
                , Attr.checked (widthMode == UnknownWidth)
                ]
                []
            , text "Unknown"
            ]
        ]


heightModeControl : HeightMode -> Html Msg
heightModeControl heightMode =
    div [ Attr.class "mode-control" ]
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


widthFields : Config -> Html Msg
widthFields config =
    case config.widthMode of
        FixedWidth ->
            fixedWidthField config.fixedWidth

        UnknownWidth ->
            randomWidthFields config


heightFields : Config -> Html Msg
heightFields config =
    case config.heightMode of
        FixedHeight ->
            fixedHeightField config.fixedHeight

        UnknownHeight ->
            randomHeightFields config


fixedWidthField : Maybe Int -> Html Msg
fixedWidthField width =
    div [ Attr.class "fixed-field" ]
        [ input
            [ Attr.type_ "number"
            , updateConfigField .fixedWidth UpdateFixedWidth |> Events.onInput
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
                , updateConfigField .minWidth UpdateMinWidth |> Events.onInput
                , Attr.placeholder "40"
                , config.minWidth |> viewMaybeInt |> Attr.value
                ]
                []
            , label [] [ text "Min" ]
            ]
        , div [ Attr.class "min-max-field" ]
            [ input
                [ Attr.type_ "number"
                , updateConfigField .maxWidth UpdateMaxWidth |> Events.onInput
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
                , updateConfigField .minHeight UpdateMinHeight |> Events.onInput
                , Attr.placeholder "40"
                , config.minHeight |> viewMaybeInt |> Attr.value
                ]
                []
            , label [] [ text "Min" ]
            ]
        , div [ Attr.class "min-max-field" ]
            [ input
                [ Attr.type_ "number"
                , updateConfigField .maxHeight UpdateMaxHeight |> Events.onInput
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
            , updateConfigField .fixedHeight UpdateFixedHeight |> Events.onInput
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
                , updateConfigField .xMargin UpdateXMargin |> Events.onInput
                , Attr.placeholder "12"
                , xMargin |> viewMaybeInt |> Attr.value
                ]
                []
            , label [] [ text "X" ]
            ]
        , div [ Attr.class "margin-field" ]
            [ input
                [ Attr.type_ "number"
                , updateConfigField .yMargin UpdateYMargin |> Events.onInput
                , Attr.placeholder "12"
                , yMargin |> viewMaybeInt |> Attr.value
                ]
                []
            , label [] [ text "Y" ]
            ]
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
