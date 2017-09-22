module DynamicList
    exposing
        ( Config
        , Dimensions
        , DynamicList
        , Item
        , ListType
        , Position
        , dragItem
        , empty
        , emptyContainer
        , releaseItem
        , repositionItems
        , resortList
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
    , container : Container
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
    , container = emptyContainer
    , items = []
    , draggedItem = None
    , mousePosition = { x = 0, y = 0 }
    }


type alias Config =
    { listType : ListType
    , xMargin : Int
    , yMargin : Int
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
    }


type alias Container =
    { id : String
    , width : Int
    }


emptyContainer : Container
emptyContainer =
    { id = ""
    , width = 0
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
    { dynamicList
        | items = getPositions dynamicList.config dynamicList.container items
    }


repositionItems : DynamicList msg -> DynamicList msg
repositionItems dynamicList =
    { dynamicList
        | items = getPositions dynamicList.config dynamicList.container dynamicList.items
    }


dragItem : String -> Position -> DynamicList msg -> DynamicList msg
dragItem draggedId position dynamicList =
    { dynamicList | draggedItem = DraggedItem draggedId position }


releaseItem : DynamicList msg -> DynamicList msg
releaseItem dynamicList =
    { dynamicList | draggedItem = None }


setMousePosition : Position -> DynamicList msg -> DynamicList msg
setMousePosition mousePosition dynamicList =
    { dynamicList | mousePosition = mousePosition }


setContainerId : String -> DynamicList msg -> DynamicList msg
setContainerId id dynamicList =
    let
        { container } =
            dynamicList

        updatedContainer =
            { container | id = id }
    in
    { dynamicList | container = updatedContainer }


setContainerWidth : Int -> DynamicList msg -> DynamicList msg
setContainerWidth width dynamicList =
    let
        { container } =
            dynamicList

        updatedContainer =
            { container | width = width }
    in
    { dynamicList | container = updatedContainer }


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


moveItem : Position -> Item msg -> Item msg
moveItem position item =
    { item | position = position }


resortList : String -> DynamicList msg -> DynamicList msg
resortList overId dynamicList =
    case dynamicList.draggedItem of
        None ->
            dynamicList

        DraggedItem draggedId _ ->
            if draggedId == overId then
                dynamicList
            else
                let
                    maybeDraggedItem =
                        List.filter (.id >> (==) draggedId) dynamicList.items |> List.head
                in
                dynamicList.items
                    |> List.foldr (compareDraggedItem overId draggedId) ( maybeDraggedItem, [] )
                    |> Tuple.second
                    |> flip setItems dynamicList


compareDraggedItem : String -> String -> Item msg -> ( Maybe (Item msg), List (Item msg) ) -> ( Maybe (Item msg), List (Item msg) )
compareDraggedItem overId draggedId item ( maybeDraggedItem, acc ) =
    maybeDraggedItem
        |> Maybe.map
            (\draggedItem ->
                case ( draggedId == item.id, overId == item.id ) of
                    ( True, False ) ->
                        ( Just draggedItem, acc )

                    ( False, True ) ->
                        ( Nothing, draggedItem :: item :: acc )

                    ( _, _ ) ->
                        ( Just draggedItem, item :: acc )
            )
        |> Maybe.withDefault
            (if draggedId == item.id then
                ( Nothing, acc )
             else
                ( Nothing, item :: acc )
            )



-- VIEW


view : DynamicList msg -> Html msg
view { config, container, items, draggedItem, mousePosition } =
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
        [ Attr.id container.id
        , Attr.class "dynamic-list"
        , Attr.style [ ( "position", "relative" ) ]
        ]
        allItems


draggedItemView : Config -> Position -> Position -> Item msg -> Html msg
draggedItemView { listType } clickPosition mousePosition { position, dimensions, content } =
    let
        (FixedWidth width) =
            listType

        offset =
            { x = clickPosition.x - position.x, y = clickPosition.y - position.y }
    in
    div
        [ Attr.style
            [ ( "background-color", "red" )
            , ( "height", dimensions.height |> toString |> flip (++) "px" )
            , ( "width", width |> toString |> flip (++) "px" )
            , ( "position", "absolute" )
            , ( "left", mousePosition.x - offset.x |> toString |> flip (++) "px" )
            , ( "top", mousePosition.y - offset.y |> toString |> flip (++) "px" )
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


getPositions : Config -> Container -> List (Item msg) -> List (Item msg)
getPositions config container =
    case config.listType of
        FixedWidth width ->
            let
                columns =
                    (container.width - config.xMargin) // (width + config.xMargin)

                leftShift =
                    rem (container.width - config.xMargin) (width + config.xMargin) // 2
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
