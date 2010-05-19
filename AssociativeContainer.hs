{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

module AssociativeContainer
(
  AssociativeContainer
, toAList
, fromAList
, insert
, empty
, null
, lookup
, remove
, merge
, mergeAList
)
where

import Prelude hiding (lookup, null)
import qualified Prelude
import Control.Monad
import qualified Data.Map as M

class Eq k => AssociativeContainer a k v | a -> k v where
    toAList :: a -> [(k, v)]
    fromAList :: [(k, v)] -> a
    insert :: (k, v) -> a -> a
    empty :: a
    null :: a -> Bool
    lookup :: (MonadPlus t) => k -> a -> t v
    remove :: k -> a -> a
    merge :: a -> a -> a
    mergeAList :: [(k, v)] -> a -> a
    mergeAList = merge . fromAList

instance Eq a => AssociativeContainer [(a, b)] a b where
    fromAList = id
    toAList = id
    empty = []
    null = Prelude.null
    insert = (:)
    lookup k = msum . map check
        where check (k', v) = guard (k == k') >> return v
    remove k = filter (keyNotEquals k)
    merge = (++)

instance Ord a => AssociativeContainer (M.Map a b) a b where
    fromAList = M.fromList
    toAList = M.toList
    insert = uncurry M.insert
    empty = M.empty
    null = M.null
    lookup k m = maybe mzero return (M.lookup k m)
    remove = M.delete
    merge = M.union

keyEquals k (k', _) = k == k'
keyNotEquals k = not . (keyEquals k)