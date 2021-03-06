port module Main exposing (main)

import DynamicList as DL exposing (DynamicList)
import Html exposing (Attribute, Html, button, code, div, fieldset, footer, h1, h2, h3, header, input, label, main_, p, text)
import Html.Attributes as Attr
import Html.Events as Events
import ItemGenerator as IG
import Json.Decode as JD
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
    , dynamicList : DynamicList Msg
    , mousePosition : Mouse.Position
    }


initialModel : Model
initialModel =
    { itemGenerator = IG.default
    , dynamicList = DL.empty |> DL.setContainerId "dynamic-list-1"
    , mousePosition = { x = 0, y = 0 }
    }


init : ( Model, Cmd Msg )
init =
    initialModel
        => Cmd.batch
            [ IG.init |> Cmd.map ItemGeneratorMsg
            , getContainerWidth ()
            ]


type alias RandomItem =
    { dimensions : Dimensions
    , id : Int
    }



-- UPDATE


type Msg
    = NoOp
    | ItemGeneratorMsg IG.Msg
    | DragItem String
    | ReleaseItem Mouse.Position
    | GetContainerWidth
    | SetContainerWidth Int
    | SetMousePosition Mouse.Position
    | ResortList String


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
                                items =
                                    List.indexedMap
                                        (\idx dim ->
                                            DL.Item dim
                                                (idx + 1 |> toString)
                                                (itemView model.itemGenerator dim (idx + 1 |> toString))
                                                { x = 0, y = 0 }
                                        )
                                        dimensions
                            in
                            { model | dynamicList = buildList model items }
                                => getContainerWidth ()

                        IG.ClearItems ->
                            let
                                { dynamicList } =
                                    model

                                updatedDynamicList =
                                    { dynamicList | items = [] }
                            in
                            { model | dynamicList = updatedDynamicList } => Cmd.none
            in
            { updatedModel | itemGenerator = itemGenerator }
                => Cmd.batch [ cmd1 |> Cmd.map ItemGeneratorMsg, cmd2 ]

        DragItem id ->
            { model | dynamicList = DL.dragItem id model.mousePosition model.dynamicList } => Cmd.none

        ReleaseItem _ ->
            { model | dynamicList = DL.releaseItem model.dynamicList } => Cmd.none

        SetMousePosition mousePosition ->
            { model
                | dynamicList = DL.setMousePosition mousePosition model.dynamicList
                , mousePosition = mousePosition
            }
                => Cmd.none

        GetContainerWidth ->
            model => getContainerWidth ()

        SetContainerWidth width ->
            { model
                | dynamicList =
                    model.dynamicList
                        |> DL.setContainerWidth width
                        |> DL.repositionItems
            }
                => Cmd.none

        ResortList id ->
            { model
                | dynamicList =
                    model.dynamicList
                        |> DL.resortList id
                        |> DL.repositionItems
            }
                => Cmd.none


buildList : Model -> List (DL.Item Msg) -> DynamicList Msg
buildList { dynamicList, itemGenerator } items =
    dynamicList
        |> DL.setXMargin (IG.xMargin itemGenerator)
        |> DL.setYMargin (IG.yMargin itemGenerator)
        |> DL.setItems items


port getContainerWidth : () -> Cmd msg



-- SUBSCRIPTIONS


port mouseLeaves : (Mouse.Position -> msg) -> Sub msg


port windowResize : (Int -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ windowResize SetContainerWidth
        , Mouse.moves SetMousePosition
        , Mouse.ups ReleaseItem
        , mouseLeaves ReleaseItem
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ header [ Attr.class "title" ] [ h1 [] [ text "Elm Dynamic List" ] ]
        , main_ []
            [ IG.view model.itemGenerator |> Html.map ItemGeneratorMsg
            , DL.view model.dynamicList
            ]

        --        , footer [] [ code [] [ text <| toString model ] ]
        ]


onItemSelect : String -> Attribute Msg
onItemSelect id =
    Events.onWithOptions "mousedown"
        { stopPropagation = True, preventDefault = True }
        (JD.succeed (DragItem id))


itemView : IG.Model -> Dimensions -> String -> Html Msg
itemView itemGenerator dimensions id =
    div
        [ itemStyle itemGenerator dimensions
        , onItemSelect id
        , Events.onMouseOver (ResortList id)
        ]
        [ id |> toString |> text ]


itemStyle : IG.Model -> Dimensions -> Html.Attribute Msg
itemStyle itemGenerator { width, height } =
    Attr.style
        [ ( "width", width |> toString |> flip (++) "px" )
        , ( "height", height |> toString |> flip (++) "px" )
        ]


maybeIntToPx : Maybe Int -> Int -> String
maybeIntToPx maybeInt default =
    maybeInt
        |> Maybe.withDefault default
        |> toString
        |> flip (++) "px"
