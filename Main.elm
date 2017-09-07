port module Main exposing (main)

import Html exposing (Html, button, code, div, fieldset, footer, h1, h2, h3, header, input, label, main_, p, text)
import Html.Attributes as Attr
import Html.Events as Events
import ItemGenerator
import Mouse
import Positioning
import Types exposing (Dimensions)
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
    { itemGenerator : ItemGenerator.Model
    , items : List Item
    , draggedItemId : Maybe Int
    }


initialModel : Model
initialModel =
    { itemGenerator = ItemGenerator.default
    , items = []
    , draggedItemId = Nothing
    }


init : ( Model, Cmd Msg )
init =
    initialModel => (ItemGenerator.init |> Cmd.map ItemGeneratorMsg)


type alias Item =
    { dimensions : Dimensions
    , id : Int
    }



-- UPDATE


type Msg
    = NoOp
    | ItemGeneratorMsg ItemGenerator.Msg
    | SetDraggedItem Int
    | ClearDraggedItem Mouse.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model => Cmd.none

        ItemGeneratorMsg itemGeneratorMsg ->
            let
                ( itemGenerator, cmd1, externalMsg ) =
                    ItemGenerator.update itemGeneratorMsg model.itemGenerator

                ( updatedModel, cmd2 ) =
                    case externalMsg of
                        ItemGenerator.NoOp ->
                            model => Cmd.none

                        ItemGenerator.SetItems dimensions ->
                            let
                                items =
                                    List.indexedMap
                                        (\idx dim -> Item dim (idx + 1))
                                        dimensions
                            in
                            { model | items = items } => Cmd.none

                        ItemGenerator.ClearItems ->
                            { model | items = [] } => Cmd.none
            in
            { updatedModel | itemGenerator = itemGenerator }
                => Cmd.batch [ cmd1 |> Cmd.map ItemGeneratorMsg, cmd2 ]

        SetDraggedItem id ->
            { model | draggedItemId = Just id } => Cmd.none

        ClearDraggedItem _ ->
            { model | draggedItemId = Nothing } => Cmd.none



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
            [ ItemGenerator.view model.itemGenerator |> Html.map ItemGeneratorMsg
            , items model
            ]
        , footer [] [ code [] [ text <| toString model ] ]
        ]


items : Model -> Html Msg
items { itemGenerator, draggedItemId, items } =
    div
        [ Attr.class "dynamic-list" ]
        (List.map (itemView itemGenerator draggedItemId) items)


compareIds : Maybe Int -> Int -> Bool
compareIds maybeId currentId =
    case maybeId of
        Just id ->
            id == currentId

        Nothing ->
            False


itemView : ItemGenerator.Model -> Maybe Int -> Item -> Html Msg
itemView itemGenerator draggedItemId { dimensions, id } =
    div
        [ Attr.classList [ ( "item", True ), ( "dragged", compareIds draggedItemId id ) ]
        , itemStyle itemGenerator dimensions
        , Events.onMouseDown (SetDraggedItem id)
        ]
        []


itemStyle : ItemGenerator.Model -> Dimensions -> Html.Attribute Msg
itemStyle itemGenerator { width, height } =
    Attr.style
        [ ( "margin"
          , maybeIntToPx itemGenerator.yMargin ItemGenerator.defaultYMargin
                ++ " "
                ++ maybeIntToPx itemGenerator.xMargin ItemGenerator.defaultXMargin
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
