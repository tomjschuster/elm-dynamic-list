module ItemGenerator
    exposing
        ( ExternalMsg(..)
        , HeightMode(..)
        , Model
        , Msg
        , WidthMode(..)
        , controlPanel
        , default
        , defaultXMargin
        , defaultYMargin
        , fixedWidth
        , init
        , update
        , view
        , xMargin
        , yMargin
        )

import Html exposing (Html, button, div, fieldset, h2, h3, input, label, text)
import Html.Attributes as Attr
import Html.Events as Events
import Random
import Types exposing (Dimensions)


-- MODEL


type alias Model =
    { showControlPanel : Bool
    , itemCount : Maybe Int
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


init : Cmd Msg
init =
    Random.list
        defaultItemCount
        (randomItemGenerator default)
        |> Random.generate SetItemsMsg


type WidthMode
    = FixedWidth
    | UnknownWidth


type HeightMode
    = FixedHeight
    | UnknownHeight


default : Model
default =
    { showControlPanel = True
    , itemCount = Nothing
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


fixedWidth : Model -> Int
fixedWidth =
    .fixedWidth >> Maybe.withDefault defaultFixedWidth


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


xMargin : Model -> Int
xMargin =
    .xMargin >> Maybe.withDefault defaultXMargin


defaultYMargin : Int
defaultYMargin =
    12


yMargin : Model -> Int
yMargin =
    .yMargin >> Maybe.withDefault defaultYMargin



-- UPDATE


type ExternalMsg
    = NoOp
    | SetItems (List Dimensions)
    | ClearItems


type Msg
    = NoOpMsg
    | CloseControlPanel
    | OpenControlPanel
    | GenerateItems (Maybe Int)
    | SetItemsMsg (List Dimensions)
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


update : Msg -> Model -> ( Model, Cmd Msg, ExternalMsg )
update msg model =
    case msg of
        NoOpMsg ->
            ( model, Cmd.none, NoOp )

        CloseControlPanel ->
            ( { model | showControlPanel = False }, Cmd.none, NoOp )

        OpenControlPanel ->
            ( { model | showControlPanel = True }, Cmd.none, NoOp )

        GenerateItems maybeCount ->
            ( model
            , Random.list
                (Maybe.withDefault defaultItemCount maybeCount)
                (randomItemGenerator model)
                |> Random.generate SetItemsMsg
            , NoOp
            )

        SetItemsMsg dimensions ->
            ( model, Cmd.none, SetItems dimensions )

        ClearItemsMsg ->
            ( model, Cmd.none, ClearItems )

        UpdateItemCount itemCount ->
            ( { model | itemCount = itemCount }, Cmd.none, NoOp )

        UpdateWidthMode widthMode ->
            ( { model | widthMode = widthMode }, Cmd.none, NoOp )

        UpdateFixedWidth fixedWidth ->
            ( { model | fixedWidth = fixedWidth }, Cmd.none, NoOp )

        UpdateMinWidth minWidth ->
            ( { model | minWidth = minWidth }, Cmd.none, NoOp )

        UpdateMaxWidth maxWidth ->
            ( { model | maxWidth = maxWidth }, Cmd.none, NoOp )

        UpdateHeightMode heightMode ->
            ( { model | heightMode = heightMode }, Cmd.none, NoOp )

        UpdateFixedHeight fixedHeight ->
            ( { model | fixedHeight = fixedHeight }, Cmd.none, NoOp )

        UpdateMinHeight minHeight ->
            ( { model | minHeight = minHeight }, Cmd.none, NoOp )

        UpdateMaxHeight maxHeight ->
            ( { model | maxHeight = maxHeight }, Cmd.none, NoOp )

        UpdateXMargin xMargin ->
            ( { model | xMargin = xMargin }, Cmd.none, NoOp )

        UpdateYMargin yMargin ->
            ( { model | yMargin = yMargin }, Cmd.none, NoOp )


randomItemGenerator : Model -> Random.Generator Dimensions
randomItemGenerator model =
    Random.pair
        (getWidthRange model |> uncurry Random.int)
        (getHeightRange model |> uncurry Random.int)
        |> Random.map (uncurry Dimensions)


getWidthRange : Model -> ( Int, Int )
getWidthRange model =
    case model.widthMode of
        FixedWidth ->
            ( Maybe.withDefault defaultFixedWidth model.fixedWidth
            , Maybe.withDefault defaultFixedWidth model.fixedWidth
            )

        UnknownWidth ->
            ( Maybe.withDefault defaultMinWidth model.minWidth
            , Maybe.withDefault defaultMaxWidth model.maxWidth
            )


getHeightRange : Model -> ( Int, Int )
getHeightRange model =
    case model.heightMode of
        FixedHeight ->
            ( Maybe.withDefault defaultFixedHeight model.fixedHeight
            , Maybe.withDefault defaultFixedHeight model.fixedHeight
            )

        UnknownHeight ->
            ( Maybe.withDefault defaultMinHeight model.minHeight
            , Maybe.withDefault defaultMaxHeight model.maxHeight
            )


view : Model -> Html Msg
view model =
    if model.showControlPanel then
        controlPanel model
    else
        div [ Attr.class "show-control-panel" ]
            [ button
                [ Events.onClick OpenControlPanel ]
                [ text "Control Panel" ]
            ]


controlPanel : Model -> Html Msg
controlPanel model =
    div [ Attr.class "control-panel" ]
        [ button
            [ Attr.class "hide-control-panel"
            , Events.onClick CloseControlPanel
            ]
            [ text "X" ]
        , div [ Attr.class "generate-items" ]
            [ h2 [] [ text "Generate Items" ]
            , itemGenerator model.itemCount
            ]
        , div [ Attr.class "item-options" ]
            [ h2 [ Attr.class "options-title" ] [ text "Options" ]
            , div [ Attr.class "options-categories" ]
                [ div [ Attr.class "options-category item-width" ]
                    [ h3 [] [ text "Width" ]
                    , widthModeControl model.widthMode
                    , widthFields model
                    ]
                , div [ Attr.class "options-category item-height" ]
                    [ h3 [] [ text "Height" ]
                    , heightModeControl model.heightMode
                    , heightFields model
                    ]
                , div [ Attr.class "options-category item-margins" ]
                    [ h3 [] [ text "Margins" ]
                    , marginFields model
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


updateModelField : (Model -> Maybe Int) -> (Maybe Int -> msg) -> String -> msg
updateModelField field toMsg =
    updateIntField toMsg (default |> field)


itemGenerator : Maybe Int -> Html Msg
itemGenerator itemCount =
    div [ Attr.class "item-generator" ]
        [ button
            [ GenerateItems itemCount |> Events.onClick
            ]
            [ text "Generate" ]
        , input
            [ itemCount |> viewMaybeInt |> Attr.value
            , updateModelField .itemCount UpdateItemCount |> Events.onInput
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


widthFields : Model -> Html Msg
widthFields model =
    case model.widthMode of
        FixedWidth ->
            fixedWidthField model.fixedWidth

        UnknownWidth ->
            randomWidthFields model


heightFields : Model -> Html Msg
heightFields model =
    case model.heightMode of
        FixedHeight ->
            fixedHeightField model.fixedHeight

        UnknownHeight ->
            randomHeightFields model


fixedWidthField : Maybe Int -> Html Msg
fixedWidthField width =
    div [ Attr.class "fixed-field" ]
        [ input
            [ Attr.type_ "number"
            , updateModelField .fixedWidth UpdateFixedWidth |> Events.onInput
            , Attr.placeholder "240"
            , width |> viewMaybeInt |> Attr.value
            ]
            []
        ]


randomWidthFields : Model -> Html Msg
randomWidthFields model =
    div [ Attr.class "random-fields" ]
        [ div [ Attr.class "min-max-field" ]
            [ input
                [ Attr.type_ "number"
                , updateModelField .minWidth UpdateMinWidth |> Events.onInput
                , Attr.placeholder "40"
                , model.minWidth |> viewMaybeInt |> Attr.value
                ]
                []
            , label [] [ text "Min" ]
            ]
        , div [ Attr.class "min-max-field" ]
            [ input
                [ Attr.type_ "number"
                , updateModelField .maxWidth UpdateMaxWidth |> Events.onInput
                , Attr.placeholder "480"
                , model.maxWidth |> viewMaybeInt |> Attr.value
                ]
                []
            , label [] [ text "Max" ]
            ]
        ]


randomHeightFields : Model -> Html Msg
randomHeightFields model =
    div [ Attr.class "random-fields" ]
        [ div [ Attr.class "min-max-field" ]
            [ input
                [ Attr.type_ "number"
                , updateModelField .minHeight UpdateMinHeight |> Events.onInput
                , Attr.placeholder "40"
                , model.minHeight |> viewMaybeInt |> Attr.value
                ]
                []
            , label [] [ text "Min" ]
            ]
        , div [ Attr.class "min-max-field" ]
            [ input
                [ Attr.type_ "number"
                , updateModelField .maxHeight UpdateMaxHeight |> Events.onInput
                , Attr.placeholder "480"
                , model.maxHeight |> viewMaybeInt |> Attr.value
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
            , updateModelField .fixedHeight UpdateFixedHeight |> Events.onInput
            , Attr.placeholder "240"
            , height |> viewMaybeInt |> Attr.value
            ]
            []
        ]


marginFields : Model -> Html Msg
marginFields { xMargin, yMargin } =
    div [ Attr.class "margin-fields" ]
        [ div [ Attr.class "margin-field" ]
            [ input
                [ Attr.type_ "number"
                , updateModelField .xMargin UpdateXMargin |> Events.onInput
                , Attr.placeholder "12"
                , xMargin |> viewMaybeInt |> Attr.value
                ]
                []
            , label [] [ text "X" ]
            ]
        , div [ Attr.class "margin-field" ]
            [ input
                [ Attr.type_ "number"
                , updateModelField .yMargin UpdateYMargin |> Events.onInput
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
