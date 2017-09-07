module Utils exposing ((=>))


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
infixr 9 =>
