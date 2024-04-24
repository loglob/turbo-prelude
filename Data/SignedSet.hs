module Data.SignedSet where

import Data.Set qualified as Set
import Turbo.Prelude

-- | A set that can either be include-by-default or exclude-by-default.
data SignedSet a
    = -- | Only the elements of the given set are included
      Positive (Set a)
    | -- | Every element but those of the given set are included
      Negative (Set a)
    deriving (Show)

-- | Alias for difference
(\\) :: (Ord a) => SignedSet a -> SignedSet a -> SignedSet a
(\\) = difference

-- | Removes an element from the set
delete :: (Ord a) => a -> SignedSet a -> SignedSet a
delete k (Positive s) = Positive (Set.delete k s)
delete k (Negative s) = Negative (Set.insert k s)

-- | Every item from the first set that isn't in the second
difference :: (Ord a) => SignedSet a -> SignedSet a -> SignedSet a
difference s t = s `union` invert t

-- | The set containing no elements
empty :: SignedSet a
empty = Positive Set.empty

-- | The set containing every element
full :: SignedSet a
full = Negative Set.empty

-- | Puts an element into the set
insert :: (Ord a) => a -> SignedSet a -> SignedSet a
insert k (Positive s) = Positive (Set.insert k s)
insert k (Negative s) = Negative (Set.delete k s)

-- | A set that contains every element from both of the given sets
intersection :: (Ord a) => SignedSet a -> SignedSet a -> SignedSet a
intersection (Positive s) (Positive t) = Positive (s `Set.intersection` t)
intersection (Positive s) (Negative t) = Positive (s Set.\\ t)
intersection (Negative s) (Positive t) = Positive (t Set.\\ s)
intersection (Negative s) (Negative t) = Negative (s `Set.union` t)

-- | A set that contains every element but those of the given set
invert :: SignedSet a -> SignedSet a
invert (Positive s) = Negative s
invert (Negative s) = Positive s

-- | Whether a signed set contains a key
member :: (Ord a) => a -> SignedSet a -> Bool
member k (Positive s) = Set.member k s
member k (Negative s) = Set.notMember k s

union :: (Ord a) => SignedSet a -> SignedSet a -> SignedSet a
union (Positive s) (Positive t) = Positive (s `Set.union` t)
union (Positive s) (Negative t) = Negative (t Set.\\ s)
union (Negative s) (Positive t) = Negative (s Set.\\ t)
union (Negative s) (Negative t) = Negative (s `Set.intersection` t)
