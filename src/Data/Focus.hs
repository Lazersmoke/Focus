{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Data.Focus where

class Focus whole part spec where
  focus :: spec -> whole -> part
  tinker :: spec -> (part -> part) -> whole -> whole

data ExplicitFocus w p = ExplicitFocus (w -> p) ((p -> p) -> w -> w)
instance Focus w p (ExplicitFocus w p) where
  focus (ExplicitFocus f _) = f
  tinker (ExplicitFocus _ t) = t
