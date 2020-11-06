module Indexable (class Indexable, (!!), index) where

import Data.Array as A
import Data.ArrayView as AV
import Data.Maybe (Maybe)

class Indexable i where
  index :: forall a. i a -> Int -> Maybe a

infixl 8 index as !!

instance indexableArray :: Indexable Array where
  index = A.index

instance indexableArrayView :: Indexable AV.ArrayView where
  index = AV.index
