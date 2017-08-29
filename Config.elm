module Config
    exposing
        ( Config
        , Msg(..)
        , default
        , defaultWidth
        , defaultXMargin
        , defaultYMargin
        , update
        )


type alias Config =
    { width : Maybe Int
    , xMargin : Maybe Int
    , yMargin : Maybe Int
    }


default : Config
default =
    Config (Just defaultWidth) (Just defaultXMargin) (Just defaultYMargin)


defaultWidth : Int
defaultWidth =
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
    | UpdateXMargin String
    | UpdateYMargin String


update : Msg -> Config -> Config
update msg config =
    case msg of
        NoOp ->
            config

        UpdateWidth widthStr ->
            { config | width = stringToMaybeInt default.width widthStr }

        UpdateXMargin xMarginStr ->
            { config | xMargin = stringToMaybeInt default.xMargin xMarginStr }

        UpdateYMargin yMarginStr ->
            { config | yMargin = stringToMaybeInt default.yMargin yMarginStr }


stringToMaybeInt : Maybe Int -> String -> Maybe Int
stringToMaybeInt default stringInt =
    if stringInt == "" then
        Nothing
    else
        String.toInt stringInt
            |> Result.map Just
            |> Result.withDefault default
