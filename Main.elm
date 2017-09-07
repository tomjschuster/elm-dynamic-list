port module Main exposing (main)

import Config exposing (Config, ExternalMsg)
import Html exposing (Html, button, code, div, fieldset, footer, h1, h2, h3, header, input, label, main_, p, text)
import Html.Attributes as Attr
import Html.Events as Events
import Mouse
import Random
import Utils exposing ((=>))


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { showControlPanel : Bool
    , config : Config
    , items : List Item
    , draggedItemId : Maybe Int
    }


initialModel : Model
initialModel =
    { showControlPanel = True
    , config = Config.default
    , items = []
    , draggedItemId = Nothing
    }


init : ( Model, Cmd Msg )
init =
    initialModel
        => (Random.list
                Config.defaultItemCount
                (randomItemGenerator initialModel.config)
                |> Random.generate SetItems
           )


type alias Item =
    { dimensions : Dimensions
    , id : Int
    }


type alias Dimensions =
    { width : Int
    , height : Int
    }



-- UPDATE


type Msg
    = NoOp
    | ShowControlPanel
    | HideControlPanel
    | ConfigMsg Config.Msg
    | SetItems (List (Int -> Item))
    | SetDraggedItem Int
    | ClearDraggedItem Mouse.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model => Cmd.none

        ShowControlPanel ->
            { model | showControlPanel = True } => Cmd.none

        HideControlPanel ->
            { model | showControlPanel = False } => Cmd.none

        SetItems toItems ->
            let
                items =
                    List.indexedMap (\idx toItem -> toItem (idx + 1)) toItems
            in
            { model | items = items } => Cmd.none

        ConfigMsg configMsg ->
            let
                ( config, externalMsg ) =
                    Config.update configMsg model.config

                ( updatedModel, cmd ) =
                    case externalMsg of
                        Config.NoOp ->
                            model => Cmd.none

                        Config.Close ->
                            { model | showControlPanel = False } => Cmd.none

                        Config.GenerateItems maybeCount ->
                            model
                                => (Random.list
                                        (Maybe.withDefault
                                            Config.defaultItemCount
                                            maybeCount
                                        )
                                        (randomItemGenerator config)
                                        |> Random.generate SetItems
                                   )

                        Config.ClearItems ->
                            { model | items = [] } => Cmd.none
            in
            { updatedModel | config = config } => cmd

        SetDraggedItem id ->
            { model | draggedItemId = Just id } => Cmd.none

        ClearDraggedItem _ ->
            { model | draggedItemId = Nothing } => Cmd.none


randomItemGenerator : Config -> Random.Generator (Int -> Item)
randomItemGenerator config =
    Random.pair
        (Config.getWidthRange config |> uncurry Random.int)
        (Config.getHeightRange config |> uncurry Random.int)
        |> Random.map (uncurry Dimensions >> Item)



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
controlPanel { showControlPanel, config } =
    if showControlPanel then
        Config.controlPanel config |> Html.map ConfigMsg
    else
        div [ Attr.class "close-control-panel" ]
            [ button
                [ Events.onClick ShowControlPanel
                ]
                [ text "Control Panel" ]
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


maybeIntToPx : Maybe Int -> Int -> String
maybeIntToPx maybeInt default =
    maybeInt
        |> Maybe.withDefault default
        |> toString
        |> flip (++) "px"
