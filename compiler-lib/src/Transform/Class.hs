module Transform.Class ( App (..)
                       , Case (..)
                       , Closure (..)
                       , CTerm (..)
                       , DataCon (..)
                       , Let (..)
                       , MkClo (..)
                       ) where

import Core.Expression (Pattern, Term)

import Data.Set (Set)

class App a where
    app :: t -> a t s -> [a t s] -> a t s

class Case a where
    caseOf :: t -> a t s -> [(Pattern s, a t s)] -> a t s

class Closure a where
    closure :: t -> Set s -> [s] -> a t s -> a t s

class CTerm a where
    term :: Term t s -> a t s

class DataCon a where
    dataCon :: t -> s -> [a t s] -> a t s

class Let a where
    lett :: [(t, s, a t s)] -> a t s -> a t s

class MkClo a where
    mkClo :: t -> Set s -> s -> a t s
