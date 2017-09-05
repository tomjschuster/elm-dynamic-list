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
    | UpdateFixedWidth String
    | UpdateMinWidth String
    | UpdateMaxWidth String
    | UpdateHeightMode HeightMode
    | UpdateFixedHeight String
    | UpdateMinHeight String
    | UpdateMaxHeight String
    | UpdateXMargin String
    | UpdateYMargin String


update : Msg -> Config -> Config
update msg config =
    case msg of
        NoOp ->
            config

        UpdateWidthMode widthMode ->
            { config | widthMode = widthMode }

        UpdateFixedWidth intStr ->
            { config | fixedWidth = stringToMaybeInt default.fixedWidth intStr }

        UpdateMinWidth intStr ->
            { config | minWidth = stringToMaybeInt default.minWidth intStr }

        UpdateMaxWidth intStr ->
            { config | maxWidth = stringToMaybeInt default.maxWidth intStr }

        UpdateHeightMode heightMode ->
            { config | heightMode = heightMode }

        UpdateFixedHeight intStr ->
            { config | fixedHeight = stringToMaybeInt default.fixedHeight intStr }

        UpdateMinHeight intStr ->
            { config | minHeight = stringToMaybeInt default.minHeight intStr }

        UpdateMaxHeight intStr ->
            { config | maxHeight = stringToMaybeInt default.maxHeight intStr }

        UpdateXMargin intStr ->
            { config | xMargin = stringToMaybeInt default.xMargin intStr }

        UpdateYMargin intStr ->
            { config | yMargin = stringToMaybeInt default.yMargin intStr }


stringToMaybeInt : Maybe Int -> String -> Maybe Int
stringToMaybeInt default stringInt =
    if stringInt == "" then
        Nothing
    else
        String.toInt stringInt
            |> Result.map Just
            |> Result.withDefault default
