module Config
    exposing
        ( Config
        , Msg(..)
        , default
        , defaultHeight
        , defaultMaxHeight
        , defaultMaxWidth
        , defaultMinHeight
        , defaultMinWidth
        , defaultWidth
        , defaultXMargin
        , defaultYMargin
        , update
        )


type alias Config =
    { width : Maybe Int
    , minWidth : Maybe Int
    , maxWidth : Maybe Int
    , height : Maybe Int
    , minHeight : Maybe Int
    , maxHeight : Maybe Int
    , xMargin : Maybe Int
    , yMargin : Maybe Int
    }


default : Config
default =
    { width = Nothing
    , minWidth = Nothing
    , maxWidth = Nothing
    , height = Nothing
    , minHeight = Nothing
    , maxHeight = Nothing
    , xMargin = Nothing
    , yMargin = Nothing
    }


defaultWidth : Int
defaultWidth =
    240


defaultMinWidth : Int
defaultMinWidth =
    40


defaultMaxWidth : Int
defaultMaxWidth =
    480


defaultHeight : Int
defaultHeight =
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
    | UpdateWidth String
    | UpdateMinWidth String
    | UpdateMaxWidth String
    | UpdateHeight String
    | UpdateMinHeight String
    | UpdateMaxHeight String
    | UpdateXMargin String
    | UpdateYMargin String
    | ToggleWidth
    | ToggleHeight


update : Msg -> Config -> Config
update msg config =
    case msg of
        NoOp ->
            config

        UpdateWidth intStr ->
            { config | width = stringToMaybeInt default.width intStr }

        UpdateMinWidth intStr ->
            { config | minWidth = stringToMaybeInt default.minWidth intStr }

        UpdateMaxWidth intStr ->
            { config | maxWidth = stringToMaybeInt default.maxWidth intStr }

        UpdateHeight intStr ->
            { config | height = stringToMaybeInt default.height intStr }

        UpdateMinHeight intStr ->
            { config | minHeight = stringToMaybeInt default.minHeight intStr }

        UpdateMaxHeight intStr ->
            { config | maxHeight = stringToMaybeInt default.maxHeight intStr }

        UpdateXMargin intStr ->
            { config | xMargin = stringToMaybeInt default.xMargin intStr }

        UpdateYMargin intStr ->
            { config | yMargin = stringToMaybeInt default.yMargin intStr }

        ToggleWidth ->
            let
                width =
                    if config.width == Nothing then
                        Just defaultWidth
                    else
                        Nothing
            in
            { config | width = width }

        ToggleHeight ->
            let
                height =
                    if config.height == Nothing then
                        Just defaultHeight
                    else
                        Nothing
            in
            { config | height = height }


stringToMaybeInt : Maybe Int -> String -> Maybe Int
stringToMaybeInt default stringInt =
    if stringInt == "" then
        Nothing
    else
        String.toInt stringInt
            |> Result.map Just
            |> Result.withDefault default
