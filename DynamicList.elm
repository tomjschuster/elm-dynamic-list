module DynamicList
    exposing
        ( Config
        , Dimensions
        , DynamicList
        , Item
        , ListType(..)
        , Position
        , dragItem
        , empty
        , releaseItem
        , repositionItems
        , setContainerId
        , setContainerWidth
        , setItems
        , setListType
        , setMousePosition
        , setXMargin
        , setYMargin
        , view
        )

import Array exposing (Array)
import Html exposing (Html, div, text)
import Html.Attributes as Attr


-- MODEL


type alias DynamicList msg =
    { config : Config
    , items : List (Item msg)
    , draggedItem : DraggedItem
    , mousePosition : Position
    }


type DraggedItem
    = DraggedItem String Position
    | None


empty : DynamicList msg
empty =
    { config = defaultConfig
    , items = []
    , draggedItem = None
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
    , position : Position
    }


type ListType
    = FixedWidth Int


type alias Dimensions =
    { width : Int, height : Int }


type alias Position =
    { x : Int, y : Int }


setItems : List (Item msg) -> DynamicList msg -> DynamicList msg
setItems items dynamicList =
    { dynamicList | items = getPositions dynamicList.config items }


repositionItems : DynamicList msg -> DynamicList msg
repositionItems dynamicList =
    { dynamicList | items = getPositions dynamicList.config dynamicList.items }


dragItem : String -> Position -> DynamicList msg -> DynamicList msg
dragItem draggedId position dynamicList =
    { dynamicList | draggedItem = DraggedItem draggedId position }


releaseItem : DynamicList msg -> DynamicList msg
releaseItem dynamicList =
    { dynamicList | draggedItem = None }


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


moveItem : Position -> Item msg -> Item msg
moveItem position item =
    { item | position = position }



-- VIEW


view : DynamicList msg -> Html msg
view { config, items, draggedItem, mousePosition } =
    let
        fixedItems =
            List.map (itemView config draggedItem) items

        allItems =
            case draggedItem of
                DraggedItem id clickPosition ->
                    items
                        |> List.filter (.id >> (==) id)
                        |> List.head
                        |> Maybe.map
                            (draggedItemView config clickPosition mousePosition >> flip (::) fixedItems)
                        |> Maybe.withDefault fixedItems

                None ->
                    fixedItems
    in
    div
        [ Attr.id config.containerId
        , Attr.class "dynamic-list"
        ]
        allItems


draggedItemView : Config -> Position -> Position -> Item msg -> Html msg
draggedItemView { listType } clickPosition { x, y } { dimensions, content } =
    let
        (FixedWidth width) =
            listType
    in
    div
        [ Attr.style
            [ ( "background-color", "red" )
            , ( "height", dimensions.height |> toString |> flip (++) "px" )
            , ( "width", width |> toString |> flip (++) "px" )
            , ( "position", "absolute" )
            , ( "left", x |> toString |> flip (++) "px" )
            , ( "top", y |> toString |> flip (++) "px" )
            ]
        ]
        [ content ]


itemView : Config -> DraggedItem -> Item msg -> Html msg
itemView config draggedItem { content, id, position } =
    div
        [ Attr.style
            [ ( "position", "absolute" )
            , ( "transform", translationStyle position )
            ]
        , Attr.classList
            [ ( "dynamic-list-item", True )
            , ( "dragged", isDragged id draggedItem )
            ]
        ]
        [ content ]


isDragged : String -> DraggedItem -> Bool
isDragged id draggedItem =
    case draggedItem of
        DraggedItem draggedId clickPosition ->
            id == draggedId

        None ->
            False


translationStyle : Position -> String
translationStyle { x, y } =
    "translate(" ++ intToPx x ++ ", " ++ intToPx y ++ ")"


intToPx : Int -> String
intToPx =
    toString >> flip (++) "px"


getPositions : Config -> List (Item msg) -> List (Item msg)
getPositions config =
    case config.listType of
        FixedWidth width ->
            let
                columns =
                    (config.containerWidth - config.xMargin) // (width + config.xMargin)

                leftShift =
                    rem (config.containerWidth - config.xMargin) (width + config.xMargin) // 2
            in
            List.foldl
                (getPosition config leftShift)
                ( Array.repeat columns 0, [] )
                >> Tuple.second


getPosition :
    Config
    -> Int
    -> Item msg
    -> ( Array Int, List (Item msg) )
    -> ( Array Int, List (Item msg) )
getPosition config leftShift item ( columnHeights, items ) =
    case config.listType of
        FixedWidth width ->
            let
                ( _, index, translateY ) =
                    getMinColumn columnHeights

                translateX =
                    index * (width + config.xMargin) + config.xMargin + leftShift

                newHeight =
                    item.dimensions.height + translateY + config.yMargin
            in
            ( updateColumnHeights index newHeight columnHeights
            , item
                |> moveItem (Position translateX translateY)
                |> List.singleton
                |> List.append items
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
