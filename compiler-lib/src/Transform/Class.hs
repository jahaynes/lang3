module Transform.Class ( App (..) ) where

class App a where
    app :: t -> a t s -> [a t s] -> a t s
