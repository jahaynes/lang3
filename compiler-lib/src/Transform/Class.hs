module Transform.Class ( App (..)
                       , CTerm (..)
                       , Let (..)
                       ) where

import Core.Expression (Term)

class App a where
    app :: t -> a t s -> [a t s] -> a t s

class CTerm a where
    term :: Term t s -> a t s

class Let a where
    lett :: t -> s -> a t s -> a t s -> a t s
