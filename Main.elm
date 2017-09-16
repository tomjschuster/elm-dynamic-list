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
    , columns : Int
    , dynamicList : DynamicList Msg
    }


initialModel : Model
initialModel =
    { itemGenerator = IG.default
    , columns = 3
    , dynamicList = DL.empty
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
    | SetDraggedItem String
    | ClearDraggedItem Mouse.Position
    | CalculateColumns Dimensions
    | SetMousePosition Mouse.Position


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
                                        )
                                        dimensions
                            in
                            { model | dynamicList = buildList model items }
                                => Cmd.none

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

        SetDraggedItem id ->
            { model | dynamicList = DL.setDraggedItem (Just id) model.dynamicList } => Cmd.none

        ClearDraggedItem _ ->
            { model | dynamicList = DL.setDraggedItem Nothing model.dynamicList } => Cmd.none

        SetMousePosition mousePosition ->
            { model | dynamicList = DL.setMousePosition mousePosition model.dynamicList } => Cmd.none

        CalculateColumns { width, height } ->
            model => Cmd.none


buildList : Model -> List (DL.Item Msg) -> DynamicList Msg
buildList { itemGenerator, columns } items =
    DynamicList
        (getDLConfig itemGenerator columns)
        items
        Nothing
        { x = 0, y = 0 }



-- SUBSCRIPTIONS


port mouseLeaves : (Mouse.Position -> msg) -> Sub msg


port windowResize : (Dimensions -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ windowResize CalculateColumns
        , Mouse.moves SetMousePosition
        , Mouse.ups ClearDraggedItem
        , mouseLeaves ClearDraggedItem
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
        (JD.succeed (SetDraggedItem id))


itemView : IG.Model -> Dimensions -> String -> Html Msg
itemView itemGenerator dimensions id =
    div
        [ itemStyle itemGenerator dimensions
        , onItemSelect id
        ]
        []


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
