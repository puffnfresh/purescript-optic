module Optic.Plated
  ( Plated
  , plate
  , children
  , universe
  , universeOf
  , transform
  , transformOf
  , descend
  ) where

  import Data.List (List(..), (:))
  import Optic.Extended (TraversalP())
  import Optic.Fold (foldMapOf, toListOf)
  import Optic.Setter (over)
  import Optic.Types (Getting(), SettingP())
  import Prelude ((<$>), (<<<), pure, unit)

  class Plated a where
    plate :: TraversalP a a

  instance listPlated :: Plated (List a) where
    plate f (Cons x xs) = (x:) <$> f xs
    plate _ Nil = pure Nil

  children :: forall a. (Plated a) => a -> List a
  children = toListOf plate

  universe :: forall a. (Plated a) => a -> List a
  universe = universeOf plate

  universeOf :: forall a. Getting (List a) a a -> a -> List a
  universeOf l = go
    where go a = a : foldMapOf l go a

  transform :: forall a. (Plated a) => (a -> a) -> a -> a
  transform = transformOf plate

  -- TODO: Can fail due to strictness.
  transformOf :: forall p a. SettingP (->) a a -> (a -> a) -> a -> a
  transformOf l f = go unit
    where go _ = f <<< over l (go unit)

  descend :: forall a. (Plated a) => (a -> a) -> a -> a
  descend = over plate
