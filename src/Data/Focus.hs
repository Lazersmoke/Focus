{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Data.Focus where

class Focus w a where
  type Focused w a :: *
  focus :: a -> w -> Focused w a
  tinker :: a -> (Focused w a -> Focused w a) -> w -> w

data ExplicitFocus w p = ExplicitFocus (w -> p) ((p -> p) -> w -> w)
instance Focus w (ExplicitFocus w p) where
  type Focused w (ExplicitFocus w p) = p
  focus (ExplicitFocus f _) = f
  tinker (ExplicitFocus _ t) = t
