module Transform.Class ( App (..)
                       , Closure (..)
                       , CTerm (..)
                       , Let (..)
                       , MkClo (..)
                       ) where

import Core.Expression (Term)

import Data.Set (Set)

class App a where
    app :: t -> a t s -> [a t s] -> a t s

class Closure a where
    closure :: t -> Set s -> [s] -> a t s -> a t s

class CTerm a where
    term :: Term t s -> a t s

class Let a where
    lett :: [(t, s, a t s)] -> a t s -> a t s

class MkClo a where
    mkClo :: t -> Set s -> s -> a t s
