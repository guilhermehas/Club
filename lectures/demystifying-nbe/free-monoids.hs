{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module FreeMonoids where

  import Prelude hiding ((*))

  -- Terms
  data Tm x where
    E     :: Tm x
    (:*:) :: Tm x -> Tm x -> Tm x
    K     :: x -> Tm x

  -- Normal forms
  data Nf x where
    Ne' :: Ne' x -> Nf x
    E'  :: Nf x

  data Ne' x where
    Ne     :: Ne x -> Ne' x
    (:**:) :: Ne x -> Ne' x -> Ne' x

  data Ne x where
    K' :: x -> Ne x

  deriving instance Eq x => Eq (Ne x)
  deriving instance Eq x => Eq (Ne' x)
  deriving instance Eq x => Eq (Nf x)
  deriving instance Eq x => Eq (Tm x)
  deriving instance Show x => Show (Ne x)
  deriving instance Show x => Show (Ne' x)
  deriving instance Show x => Show (Nf x)
  deriving instance Show x => Show (Tm x)

  -- Embed normal forms into terms
  embNf :: Nf x -> Tm x
  embNf E'      = E
  embNf (Ne' n) = embNe' n

  embNe' :: Ne' x -> Tm x
  embNe' (Ne n)       = embNe n
  embNe' (n1 :**: n2) = embNe n1 :*: embNe' n2

  embNe :: Ne x -> Tm x
  embNe (K' x) = K x

  -- Monoid operations
  class MonoidOps t x where
    e   :: t x
    (*) :: t x -> t x -> t x
    k   :: x -> t x

  -- Tm supports the monoid ops.
  instance MonoidOps Tm x where
    e   = E
    (*) = (:*:)
    k   = K

  -- Nf supports the monoid ops.
  instance MonoidOps Nf x where
    e  = E'

    E' * n              = n
    n * E'              = n
    (Ne' n1) * (Ne' n2) = Ne' (n1 ** n2)
      where (**) :: Ne' x -> Ne' x -> Ne' x
            (Ne x)       ** n = x  :**: n
            (n1 :**: n2) ** n = n1 :**: (n2 ** n)

    k x = Ne' (Ne (K' x))

  -- Interpretation of terms in type that supports the monoid ops
  eval :: MonoidOps t x => Tm x -> t x
  eval E           = e
  eval (e1 :*: e2) = eval e1 * eval e2
  eval (K x)       = k x

  -- Normalization
  norm :: Tm x -> Tm x
  norm = embNf . eval

  -- Examples
  t1 :: Tm Int
  t1 = (E :*: K 1) :*: (K 2 :*: (K 4 :*: (E :*: K 6)))

  t2 :: Tm Int
  t2 = (E :*: K 1) :*: (K 2 :*: ((E :*: K 4) :*: K 6))

  t3 :: Tm Int
  t3 = (E :*: K 1) :*: ((K 2 :*: K 5) :*: K 6)

  ex1 :: Bool
  ex1 = t1 /= t2 && norm t1 == norm t2

  ex2 :: Bool
  ex2 = t1 /= t3 && norm t1 /= norm t3
