module Config
    exposing
        ( Config
        , Msg(..)
        , default
        , defaultHeight
        , defaultWidth
        , defaultXMargin
        , defaultYMargin
        , update
        )


type alias Config =
    { width : Maybe Int
    , height : Maybe Int
    , xMargin : Maybe Int
    , yMargin : Maybe Int
    }


default : Config
default =
    { width = Nothing
    , height = Nothing
    , xMargin = Nothing
    , yMargin = Nothing
    }


defaultWidth : Int
defaultWidth =
    240


defaultHeight : Int
defaultHeight =
    240


defaultXMargin : Int
defaultXMargin =
    12


defaultYMargin : Int
defaultYMargin =
    12


type Msg
    = NoOp
    | UpdateWidth String
    | UpdateHeight String
    | UpdateXMargin String
    | UpdateYMargin String
    | ToggleWidth
    | ToggleHeight


update : Msg -> Config -> Config
update msg config =
    case msg of
        NoOp ->
            config

        UpdateWidth widthStr ->
            { config | width = stringToMaybeInt default.width widthStr }

        UpdateHeight heightStr ->
            { config | height = stringToMaybeInt default.height heightStr }

        UpdateXMargin xMarginStr ->
            { config | xMargin = stringToMaybeInt default.xMargin xMarginStr }

        UpdateYMargin yMarginStr ->
            { config | yMargin = stringToMaybeInt default.yMargin yMarginStr }

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
