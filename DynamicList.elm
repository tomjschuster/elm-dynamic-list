module DynamicList
    exposing
        ( Config
        , Dimensions
        , DynamicList
        , Item
        , ListType(..)
        , Position
        , empty
        , setContainerId
        , setContainerWidth
        , setDraggedItem
        , setItems
        , setListType
        , setMousePosition
        , setXMargin
        , setYMargin
        , view
        )

import Array exposing (Array)
import Html exposing (Html, div)
import Html.Attributes as Attr


-- MODEL


type alias DynamicList msg =
    { config : Config
    , items : List (Item msg)
    , draggedId : Maybe String
    , mousePosition : Position
    }


empty : DynamicList msg
empty =
    { config = defaultConfig
    , items = []
    , draggedId = Nothing
    , mousePosition = { x = 0, y = 0 }
    }


type alias Config =
    { listType : ListType
    , xMargin : Int
    , yMargin : Int
    , containerWidth : Int
    , containerId : String
    }


defaultItemWidth : Int
defaultItemWidth =
    240


defaultMargin : Int
defaultMargin =
    12


defaultColumns : Int
defaultColumns =
    3


defaultConfig : Config
defaultConfig =
    { listType = FixedWidth defaultItemWidth
    , xMargin = defaultMargin
    , yMargin = defaultMargin
    , containerWidth = defaultColumns * (defaultItemWidth + defaultMargin) + defaultMargin
    , containerId = "dynamic-list-1"
    }


type alias Item msg =
    { dimensions : Dimensions
    , id : String
    , content : Html msg
    }


type ListType
    = FixedWidth Int


type alias Dimensions =
    { width : Int, height : Int }


type alias Position =
    { x : Int, y : Int }


setItems : List (Item msg) -> DynamicList msg -> DynamicList msg
setItems items dynamicList =
    { dynamicList | items = items }


setDraggedItem : Maybe String -> DynamicList msg -> DynamicList msg
setDraggedItem draggedId dynamicList =
    { dynamicList | draggedId = draggedId }


setMousePosition : Position -> DynamicList msg -> DynamicList msg
setMousePosition mousePosition dynamicList =
    { dynamicList | mousePosition = mousePosition }


setContainerWidth : Int -> DynamicList msg -> DynamicList msg
setContainerWidth containerWidth dynamicList =
    let
        { config } =
            dynamicList

        updatedConfig =
            { config | containerWidth = containerWidth }
    in
    { dynamicList | config = updatedConfig }


setXMargin : Int -> DynamicList msg -> DynamicList msg
setXMargin xMargin dynamicList =
    let
        { config } =
            dynamicList

        updatedConfig =
            { config | xMargin = xMargin }
    in
    { dynamicList | config = updatedConfig }


setYMargin : Int -> DynamicList msg -> DynamicList msg
setYMargin yMargin dynamicList =
    let
        { config } =
            dynamicList

        updatedConfig =
            { config | yMargin = yMargin }
    in
    { dynamicList | config = updatedConfig }


setListType : ListType -> DynamicList msg -> DynamicList msg
setListType listType dynamicList =
    let
        { config } =
            dynamicList

        updatedConfig =
            { config | listType = listType }
    in
    { dynamicList | config = updatedConfig }


setContainerId : String -> DynamicList msg -> DynamicList msg
setContainerId containerId dynamicList =
    let
        { config } =
            dynamicList

        updatedConfig =
            { config | containerId = containerId }
    in
    { dynamicList | config = updatedConfig }



-- VIEW


view : DynamicList msg -> Html msg
view { config, items, draggedId } =
    let
        positions =
            List.map .dimensions items |> getPositions config
    in
    div
        [ Attr.id config.containerId
        , Attr.class "dynamic-list"
        ]
        (List.map2 (itemView config draggedId) items positions)


itemView : Config -> Maybe String -> Item msg -> Position -> Html msg
itemView config draggedId { content, id } position =
    div
        [ Attr.style
            [ ( "position", "absolute" )
            , ( "transform", translationStyle position )
            ]
        , Attr.classList
            [ ( "dynamic-list-item", True )
            , ( "dragged", isDragged id draggedId )
            ]
        ]
        [ content ]


isDragged : String -> Maybe String -> Bool
isDragged id =
    Maybe.map ((==) id) >> Maybe.withDefault False


translationStyle : Position -> String
translationStyle { x, y } =
    "translate(" ++ intToPx x ++ ", " ++ intToPx y ++ ")"


intToPx : Int -> String
intToPx =
    toString >> flip (++) "px"


getPositions : Config -> List Dimensions -> List Position
getPositions config =
    case config.listType of
        FixedWidth width ->
            let
                columns =
                    (config.containerWidth - config.xMargin) // (width + config.xMargin)
            in
            List.foldl
                (getPosition config)
                ( Array.repeat columns 0, [] )
                >> Tuple.second


getPosition :
    Config
    -> Dimensions
    -> ( Array Int, List Position )
    -> ( Array Int, List Position )
getPosition config { height } ( columnHeights, positions ) =
    case config.listType of
        FixedWidth width ->
            let
                ( _, index, translateY ) =
                    getMinColumn columnHeights

                translateX =
                    index * (width + config.xMargin) + config.xMargin

                newHeight =
                    height + translateY + config.yMargin
            in
            ( updateColumnHeights index newHeight columnHeights
            , Position translateX translateY
                |> List.singleton
                |> List.append positions
            )


updateColumnHeights : Int -> Int -> Array Int -> Array Int
updateColumnHeights minIndex newHeight heights =
    Array.set minIndex newHeight heights


updatePositions : Int -> Int -> List Position -> List Position
updatePositions x y positions =
    Position x y
        |> List.singleton
        |> List.append positions


getMinColumn : Array Int -> ( Int, Int, Int )
getMinColumn columnHeights =
    Array.foldl
        compareColumnHeights
        ( 0, 0, Maybe.withDefault 0 (Array.get 0 columnHeights) )
        columnHeights


compareColumnHeights : Int -> ( Int, Int, Int ) -> ( Int, Int, Int )
compareColumnHeights height ( index, minIndex, minHeight ) =
    if height < minHeight then
        ( index + 1, index, height )
    else
        ( index + 1, minIndex, minHeight )
