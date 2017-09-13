port module Main exposing (main)

import DynamicList as DL exposing (DynamicList)
import Html exposing (Html, button, code, div, fieldset, footer, h1, h2, h3, header, input, label, main_, p, text)
import Html.Attributes as Attr
import Html.Events as Events
import ItemGenerator as IG
import Mouse
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
    { itemGenerator : IG.Model
    , randomItems : List RandomItem
    , columns : Int
    , draggedItemId : Maybe Int
    }


initialModel : Model
initialModel =
    { itemGenerator = IG.default
    , randomItems = []
    , columns = 3
    , draggedItemId = Nothing
    }


getDLConfig : IG.Model -> Int -> DL.Config
getDLConfig itemGenerator columns =
    DL.Config
        (IG.fixedWidth itemGenerator |> DL.FixedWidth)
        (IG.xMargin itemGenerator)
        (IG.yMargin itemGenerator)
        columns


init : ( Model, Cmd Msg )
init =
    initialModel => (IG.init |> Cmd.map ItemGeneratorMsg)


type alias RandomItem =
    { dimensions : Dimensions
    , id : Int
    }



-- UPDATE


type Msg
    = NoOp
    | ItemGeneratorMsg IG.Msg
    | SetDraggedItem Int
    | ClearDraggedItem Mouse.Position
    | CalculateColumns Dimensions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model => Cmd.none

        ItemGeneratorMsg itemGeneratorMsg ->
            let
                ( itemGenerator, cmd1, externalMsg ) =
                    IG.update itemGeneratorMsg model.itemGenerator

                ( updatedModel, cmd2 ) =
                    case externalMsg of
                        IG.NoOp ->
                            model => Cmd.none

                        IG.SetItems dimensions ->
                            let
                                randomItems =
                                    List.indexedMap
                                        (\idx dim -> RandomItem dim (idx + 1))
                                        dimensions
                            in
                            { model | randomItems = randomItems } => Cmd.none

                        IG.ClearItems ->
                            { model | randomItems = [] } => Cmd.none
            in
            { updatedModel | itemGenerator = itemGenerator }
                => Cmd.batch [ cmd1 |> Cmd.map ItemGeneratorMsg, cmd2 ]

        SetDraggedItem id ->
            { model | draggedItemId = Just id } => Cmd.none

        ClearDraggedItem _ ->
            { model | draggedItemId = Nothing } => Cmd.none

        CalculateColumns { width, height } ->
            model => Cmd.none



-- SUBSCRIPTIONS


port mouseLeaves : (Mouse.Position -> msg) -> Sub msg


port windowResize : (Dimensions -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions { draggedItemId } =
    let
        clearDraggedItemCmd =
            if draggedItemId == Nothing then
                Sub.none
            else
                Sub.batch
                    [ Mouse.ups ClearDraggedItem
                    , mouseLeaves ClearDraggedItem
                    ]
    in
    Sub.batch [ clearDraggedItemCmd, windowResize CalculateColumns ]



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ header [ Attr.class "title" ] [ h1 [] [ text "Elm Dynamic List" ] ]
        , main_ []
            [ IG.view model.itemGenerator |> Html.map ItemGeneratorMsg
            , items model |> DL.view
            ]

        --        , footer [] [ code [] [ text <| toString model ] ]
        ]


items : Model -> DynamicList Msg
items { itemGenerator, draggedItemId, randomItems, columns } =
    DynamicList
        (getDLConfig itemGenerator columns)
        (List.map (randomItemView itemGenerator draggedItemId >> uncurry DL.Item) randomItems)


compareIds : Maybe Int -> Int -> Bool
compareIds maybeId currentId =
    case maybeId of
        Just id ->
            id == currentId

        Nothing ->
            False


randomItemView : IG.Model -> Maybe Int -> RandomItem -> ( DL.Dimensions, Html Msg )
randomItemView itemGenerator draggedItemId { dimensions, id } =
    ( dimensions
    , div
        [ Attr.classList [ ( "item", True ), ( "dragged", compareIds draggedItemId id ) ]
        , randomItemStyle itemGenerator dimensions
        , Events.onMouseDown (SetDraggedItem id)
        ]
        []
    )


randomItemStyle : IG.Model -> Dimensions -> Html.Attribute Msg
randomItemStyle itemGenerator { width, height } =
    Attr.style
        [ ( "margin"
          , maybeIntToPx itemGenerator.yMargin IG.defaultYMargin
                ++ " "
                ++ maybeIntToPx itemGenerator.xMargin IG.defaultXMargin
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
