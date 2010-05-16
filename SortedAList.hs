{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

module SortedAList
(
 SortedAList
)
where

import AssociativeContainer
import Data.Function (on)
import Control.Monad (msum)
import qualified Data.List as L

newtype SortedAList k v = SortedAList [(k, v)]
    deriving (Show)

fromSortedAList :: SortedAList k v -> [(k, v)]
fromSortedAList (SortedAList xs) = xs

instance Ord a => AssociativeContainer (SortedAList a b) a b where
    toAList = fromSortedAList
    fromAList = SortedAList . L.sortBy (compare `on` fst)
    empty = SortedAList []
    insert x (SortedAList m) = SortedAList (insert' x m)
        where insert' x@(k, v) m@(p@(k', v') : xs) | k < k' = x : m
                                                   | otherwise = insert' x m
    null = Prelude.null . fromSortedAList
    lookup k = unlist . range . fromSortedAList
        where 
          unlist = msum . map (return . snd)
          range = takeWhile equals . dropWhile (not . equals)
          equals (k', _) = k == k'
    merge (SortedAList x) (SortedAList y) = SortedAList (merge' x y)
        where
          merge' xs@(x@(k1, _) : xs') ys@(y@(k2, _) : ys') | k1 <= k2 = x : merge' xs' ys
                                                           | otherwise = y : merge' xs ys'
          merge' _ ys = ys

