module Config
    exposing
        ( Config
        , HeightMode(..)
        , Msg(..)
        , WidthMode(..)
        , default
        , defaultFixedHeight
        , defaultFixedWidth
        , defaultMaxHeight
        , defaultMaxWidth
        , defaultMinHeight
        , defaultMinWidth
        , defaultXMargin
        , defaultYMargin
        , getHeightRange
        , getWidthRange
        , update
        )


type alias Config =
    { widthMode : WidthMode
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
    { widthMode = FixedWidth
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


type Msg
    = NoOp
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


update : Msg -> Config -> Config
update msg config =
    case msg of
        NoOp ->
            config

        UpdateWidthMode widthMode ->
            { config | widthMode = widthMode }

        UpdateFixedWidth fixedWidth ->
            { config | fixedWidth = fixedWidth }

        UpdateMinWidth minWidth ->
            { config | minWidth = minWidth }

        UpdateMaxWidth maxWidth ->
            { config | maxWidth = maxWidth }

        UpdateHeightMode heightMode ->
            { config | heightMode = heightMode }

        UpdateFixedHeight fixedHeight ->
            { config | fixedHeight = fixedHeight }

        UpdateMinHeight minHeight ->
            { config | minHeight = minHeight }

        UpdateMaxHeight maxHeight ->
            { config | maxHeight = maxHeight }

        UpdateXMargin xMargin ->
            { config | xMargin = xMargin }

        UpdateYMargin yMargin ->
            { config | yMargin = yMargin }


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
