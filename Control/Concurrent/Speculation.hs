{-# LANGUAGE BangPatterns, FlexibleContexts, DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Control.Concurrent.Speculation
    ( spec
    , spec'
    , evaluated
    , specFoldr
    , specFoldl
--    , specFoldl'
--    , specFoldr'
    , Speculative(..)
    , WrappedFoldable(..)
    , WithoutSpeculation(..)
    ) where

import Prelude hiding (foldl, foldl1, foldr, foldr1)
import Data.Array
import Data.Ix ()
import Data.Foldable
import Data.Traversable
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Set (Set)
import Data.Sequence (Seq)
import Data.Data
import Control.Parallel (par)

import Data.Bits
import Foreign
import Unsafe.Coerce

data Box a = Box a

-- | Inspect the dynamic pointer tagging bits of a closure. This is an impure function that
-- relies on GHC internals and will falsely return 0, but (hopefully) never give the wrong tag number if it returns 
-- a non-0 value.
tag :: a -> Int
tag a = unsafeCoerce (Box a) .&. (sizeOf (undefined :: Int) - 1)
{-# INLINE tag #-}

-- | Returns a guess as to whether or not a value has been evaluated. This is an impure function
-- that relies on GHC internals and will return false negatives, but (hopefully) no false positives.
evaluated :: a -> Bool
evaluated a = tag a /= 0
{-# INLINE evaluated #-}

-- | @'spec' g f a@ evaluates @f g@ while forcing @a@, if @g == a@ then @f g@ is returned. Otherwise @f a@ is evaluated.
--
-- Furthermore, if the argument has already been evaluated, we avoid sparking the parallel computation at all.
--
-- If a good guess at the value of @a@ is available, this is one way to induce parallelism in an otherwise sequential task. 
--
-- However, if the guess isn\'t available more cheaply than the actual answer, then this saves no work and if the guess is
-- wrong, you risk evaluating the function twice.
--
-- > spec a f a = a `seq` f a
--
-- The best-case timeline looks like:
--
-- > [---- f g ----]
-- >    [----- a -----]
-- > [-- spec g f a --]
-- 
-- The worst-case timeline looks like:
--
-- > [---- f g ----]
-- >    [----- a -----]
-- >                  [---- f a ----]
-- > [------- spec g f a -----------]
--
-- Compared to the unspeculated timeline of
--
-- > [---- a -----]
-- >              [---- f a ----]

spec :: Eq a => a -> (a -> b) -> a -> b
spec g f a 
    | evaluated a = f a 
    | otherwise = spec' g f a

-- | @'spec'' g f a@ evaluates a function @f @ using a cheap guess @g@ at the argument in parallel with forcing the argument.
--
-- This is one way to induce parallelism in an otherwise sequential task. 
-- Unlike `spec` this version
-- does not check to see if the argument has already been evaluated before evaluating the speculated
-- version. This is useful when you know 'evaluated' will always return False.
--
-- The following identity holds:
--
-- > spec' a f a = a `seq` f a
spec' :: Eq a => a -> (a -> b) -> a -> b
spec' guess f a = 
    speculation `par` 
        if guess == a
        then speculation
        else f a
    where 
        speculation = f guess
{-# INLINE spec #-}

-- | Compute a right biased fold. The estimator function provides a guess at the value of the suffix
specFoldr :: (Speculative f, Eq b) => (Int -> b) -> (a -> b -> b) -> b -> f a -> b
specFoldr = specFoldrN 0
{-# INLINE specFoldr #-}

-- | Compute a left-biased fold. The estimator function provides a guess at the value of the prefix
specFoldl  :: (Speculative f, Eq b) => (Int -> b) -> (b -> a -> b) -> b -> f a -> b
specFoldl = specFoldlN 0
{-# INLINE specFoldl #-}

class Foldable f => Speculative f where
    -- | Compute a right-biased fold. The estimator function is a guess at the value of the prefix
    specFoldr1 :: Eq a => (Int -> a) -> (a -> a -> a) -> f a -> a
    specFoldrN :: Eq b => Int -> (Int -> b) -> (a -> b -> b) -> b -> f a -> b

    -- | Compute a left biased fold. the estimator function is a guess at the value of the prefix
    specFoldl1 :: Eq a => (Int -> a) -> (a -> a -> a) -> f a -> a
    specFoldlN :: Eq b => Int -> (Int -> b) -> (b -> a -> b) -> b -> f a -> b

    specFoldr1 g f = specFoldr1 g f . toList
    specFoldrN n g f z = specFoldrN n g f z . toList

    specFoldl1 g f = specFoldl1 g f . toList
    specFoldlN n g f z = specFoldlN n g f z . toList

errorEmptyStructure :: String -> a
errorEmptyStructure f = error $ f ++ ": error empty structure"

instance Speculative [] where
    specFoldr1 g f = go 0  
      where
        go _ []  = errorEmptyStructure "specFoldr1"
        go _ [x] = x
        go !n (x:xs) = n' `seq` spec' (g n') (f x) (go n' xs)
          where 
            n' = n + 1

    specFoldrN _ _ _ z [] = z
    specFoldrN !n g f z (x:xs) = n' `seq` spec' (g n') (f x) (specFoldrN n' g f z xs)
      where 
        n' = n + 1
        
    specFoldl1 _ _ []     = errorEmptyStructure "specFoldl1"
    specFoldl1 g f (x:xs) = specFoldlN 1 g f x xs
    specFoldlN  _ _ _ z [] = z
    specFoldlN !n g f z (x:xs) = n' `seq` spec' (g n') (\z' -> specFoldlN n' g f z' xs) (f z x)
      where 
        n' = n + 1

-- speculation never helps with at most one element
instance Speculative Maybe where
    specFoldr1 _   = foldr1
    specFoldrN _ _ = foldr
    specFoldl1 _   = foldl1
    specFoldlN _ _ = foldl

instance Ix i => Speculative (Array i)
instance Speculative Set
instance Speculative (Map a)
instance Speculative IntMap
instance Speculative Seq

-- specFoldl' :: (Speculative f, Eq a) => (Int -> b) -> (b -> a -> b) -> b -> f a -> b
-- specFoldl' g f z0 xs = specFoldr g' f' id xs z0 where
--     f' x k z = k $! f z x
--    g' = undefined -- n z = f (g n) z

-- specFoldr' :: (Speculative f, Eq a) => (Int -> b) -> (a -> b -> b) -> b -> f a -> b
-- specFoldr' g f z0 xs = specFoldl g' f' id xs z0 where
--    f' x k z = k $! f x z
--    g' = undefined -- n z = f (g n) z

newtype WrappedFoldable f a = WrappedFoldable { getWrappedFoldable :: f a } 
    deriving (Functor, Foldable, Traversable)

instance Foldable f => Speculative (WrappedFoldable f)

instance Typeable1 f => Typeable1 (WrappedFoldable f) where
    typeOf1 tfa = mkTyConApp wrappedTyCon [typeOf1 (undefined `asArgsType` tfa)]
        where asArgsType :: f a -> t f a -> f a
              asArgsType = const
    
wrappedTyCon :: TyCon
wrappedTyCon = mkTyCon "Control.Concurrent.Speculation.WrappedFoldable"
{-# NOINLINE wrappedTyCon #-}

wrappedConstr :: Constr
wrappedConstr = mkConstr wrappedDataType "WrappedFoldable" [] Prefix
{-# NOINLINE wrappedConstr #-}

wrappedDataType :: DataType
wrappedDataType = mkDataType "Control.Concurrent.Speculation.WrappedFoldable" [wrappedConstr]
{-# NOINLINE wrappedDataType #-}

instance (Typeable1 f, Data (f a), Data a) => Data (WrappedFoldable f a) where
    gfoldl f z (WrappedFoldable a) = z WrappedFoldable `f` a
    toConstr _ = wrappedConstr
    gunfold k z c = case constrIndex c of
        1 -> k (z WrappedFoldable)
        _ -> error "gunfold"
    dataTypeOf _ = wrappedDataType
    dataCast1 f = gcast1 f

newtype WithoutSpeculation f a = WithoutSpeculation { getWithoutSpeculation :: f a } 
    deriving (Functor, Foldable, Traversable)

instance Typeable1 f => Typeable1 (WithoutSpeculation f) where
    typeOf1 tfa = mkTyConApp withoutTyCon [typeOf1 (undefined `asArgsType` tfa)]
        where asArgsType :: f a -> t f a -> f a
              asArgsType = const

instance Foldable f => Speculative (WithoutSpeculation f) where
    specFoldr1 _   = foldr1
    specFoldrN _ _ = foldr
    specFoldl1 _   = foldl1
    specFoldlN _ _ = foldl

withoutTyCon :: TyCon
withoutTyCon = mkTyCon "Control.Concurrent.Speculation.WithoutSpeculation"
{-# NOINLINE withoutTyCon #-}

withoutConstr :: Constr
withoutConstr = mkConstr withoutDataType "WithoutSpeculation" [] Prefix
{-# NOINLINE withoutConstr #-}

withoutDataType :: DataType
withoutDataType = mkDataType "Control.Concurrent.Speculation.WithoutSpeculation" [withoutConstr]
{-# NOINLINE withoutDataType #-}

instance (Typeable1 f, Data (f a), Data a) => Data (WithoutSpeculation f a) where
    gfoldl f z (WithoutSpeculation a) = z WithoutSpeculation `f` a
    toConstr _ = withoutConstr
    gunfold k z c = case constrIndex c of
        1 -> k (z WithoutSpeculation)
        _ -> error "gunfold"
    dataTypeOf _ = withoutDataType
    dataCast1 f = gcast1 f
