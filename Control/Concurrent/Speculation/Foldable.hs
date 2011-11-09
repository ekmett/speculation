{-# LANGUAGE BangPatterns #-}
module Control.Concurrent.Speculation.Foldable
    ( 
    -- * Speculative folds
      fold, foldBy
    , foldMap, foldMapBy
    , foldr, foldrBy
    , foldl, foldlBy
    , foldr1, foldr1By
    , foldl1, foldl1By
    -- ** Speculative monadic folds
    , foldrM, foldrByM
    , foldlM, foldlByM
    -- * Speculative transactional monadic folds
    , foldrSTM, foldrBySTM
    , foldlSTM, foldlBySTM
    -- * Folding actions
    -- ** Applicative actions
    , traverse_, traverseBy_
    , for_, forBy_
    , sequenceA_, sequenceByA_
    , asum, asumBy
    -- ** Monadic actions
    , mapM_, mapByM_
    , forM_, forByM_
    , sequence_, sequenceBy_
    , msum, msumBy
    -- ** Speculative transactional monadic actions
    , mapSTM_, forSTM_, sequenceSTM_
    -- * Specialized folds
    , toList, toListBy
    , concat, concatBy
    , concatMap, concatMapBy
    , all, any, and, or
    , sum, sumBy
    , product, productBy
    , maximum, maximumBy
    , minimum, minimumBy
    -- * Searches
    , elem, elemBy
    , notElem, notElemBy
    , find, findBy
    ) where

import Prelude hiding 
    (foldl, foldl1, foldr, foldr1
    , any, all, and, or, mapM_, sequence_
    , elem, notElem, sum, product
    , minimum, maximum, concat, concatMap
    )

import Data.Monoid
import Data.Ix ()
import Data.Function (on)
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Control.Concurrent.STM
import Control.Concurrent.Speculation
import Control.Concurrent.Speculation.Internal
import Control.Applicative
import Control.Monad hiding (mapM_, msum, forM_, sequence_)

-- | Given a valid estimator @g@, @'fold' g f xs@ yields the same answer as @'fold' f xs@.
-- 
-- @g n@ should supply an estimate of the value of the monoidal summation over the last @n@ elements of the container.
-- 
-- If @g n@ is accurate a reasonable percentage of the time and faster to compute than the fold, then this can
-- provide increased opportunities for parallelism.

fold :: (Foldable f, Monoid m, Eq m) => (Int -> m) -> f m -> m
fold = foldBy (==)
{-# INLINE fold #-}

-- | 'fold' using 'specBy'
foldBy :: (Foldable f, Monoid m) => (m -> m -> Bool) -> (Int -> m) -> f m -> m
foldBy cmp g = foldrBy cmp g mappend mempty
{-# INLINE foldBy #-}

-- | Given a valid estimator @g@, @'foldMap' g f xs@ yields the same answer as @'foldMap' f xs@.
-- 
-- @g n@ should supply an estimate of the value of the monoidal summation over the last @n@ elements of the container.
-- 
-- If @g n@ is accurate a reasonable percentage of the time and faster to compute than the fold, then this can
-- provide increased opportunities for parallelism.

foldMap :: (Foldable f, Monoid m, Eq m) => (Int -> m) -> (a -> m) -> f a -> m
foldMap = foldMapBy (==)
{-# INLINE foldMap #-}

-- | 'foldMap' using 'specBy'
foldMapBy :: (Foldable f, Monoid m) => (m -> m -> Bool) -> (Int -> m) -> (a -> m) -> f a -> m
foldMapBy cmp g f = foldrBy cmp g (mappend . f) mempty
{-# INLINE foldMapBy #-}


foldr :: (Foldable f, Eq b) => (Int -> b) -> (a -> b -> b) -> b -> f a -> b
foldr = foldrBy (==)
{-# INLINE foldr #-}

-- | Given a valid estimator @g@, @'foldr' g f z xs@ yields the same answer as @'foldr'' f z xs@.
--
-- @g n@ should supply an estimate of the value returned from folding over the last @n@ elements of the container.
--
-- If @g n@ is accurate a reasonable percentage of the time and faster to compute than the fold, then this can
-- provide increased opportunities for parallelism.
foldrBy :: Foldable f => (b -> b -> Bool) -> (Int -> b) -> (a -> b -> b) -> b -> f a -> b
foldrBy cmp g f z = extractAcc . Foldable.foldr mf (Acc 0 z)
  where 
    mf a (Acc n b) = Acc (n + 1) (specBy' cmp (g n) (f a) b)
{-# INLINE foldrBy #-}


{-
-- Variations:
-- These variations are not used because the values ot the left shouldn't affect the intermediate state of a right fold.
--
-- this version receiveds both the number of values remaining and the number so far

foldrBy :: Foldable f => (b -> b -> Bool) -> (Int -> Int -> b) -> (a -> b -> b) -> b -> f a -> b
foldrBy cmp g f z xs = Foldable.foldr mf (Acc 0 (const z)) xs 0
  where 
    mf a (Acc r b) !l = let l' = l + 1 in Acc (r + 1) (specBy' cmp (g l') (f a) (b l'))
{-# INLINE foldrBy #-}

-- this estimator receives the number of values to the left of the summation. 
foldrBy :: Foldable f => (b -> b -> Bool) -> (Int -> b) -> (a -> b -> b) -> b -> f a -> b
foldrBy cmp g f z xs = Foldable.foldr mf (const z) xs 0
  where 
    mf a b !i = let i' = i + 1 in specBy' cmp (g i') (f a) (b i')
{-# INLINE foldrBy #-}
-}

foldlM :: (Foldable f, Monad m, Eq (m b)) => (Int -> m b) -> (b -> a -> m b) -> m b -> f a -> m b
foldlM = foldlByM (==)
{-# INLINE foldlM #-}

foldlByM ::  (Foldable f, Monad m) => (m b -> m b -> Bool) -> (Int -> m b) -> (b -> a -> m b) -> m b -> f a -> m b
foldlByM  cmp g f mz = liftM extractAcc . Foldable.foldl go (liftM (Acc 0) mz) 
  where
    go mia b = do
      Acc n a <- mia
      a' <- specBy' cmp (g n) (>>= (`f` b)) (return a)
      return (Acc (n + 1) a')
{-# INLINE foldlByM #-}

foldrM :: (Foldable f, Monad m, Eq (m b)) => (Int -> m b) -> (a -> b -> m b) -> m b -> f a -> m b
foldrM = foldrByM (==)
{-# INLINE foldrM #-}

foldrByM :: (Foldable f, Monad m) => (m b -> m b -> Bool) -> (Int -> m b) -> (a -> b -> m b) -> m b -> f a -> m b
foldrByM cmp g f mz = liftM extractAcc . Foldable.foldr go (liftM (Acc 0) mz) 
  where
    go a mib = do
      Acc n b <- mib
      b' <- specBy' cmp (g n) (>>= f a) (return b)
      return (Acc (n + 1) b')
{-# INLINE foldrByM #-}

foldlSTM :: (Foldable f, Eq a) => (Int -> STM a) -> (a -> b -> STM a) -> STM a -> f b -> STM a
foldlSTM = foldlBySTM (returning (==))
{-# INLINE foldlSTM #-}

foldlBySTM :: Foldable f => (a -> a -> STM Bool) -> (Int -> STM a) -> (a -> b -> STM a) -> STM a -> f b -> STM a
foldlBySTM cmp g f mz = liftM extractAcc . Foldable.foldl go (liftM (Acc 0) mz)
  where
    go mia b = do
      Acc n a <- mia
      a' <- specBySTM' cmp (g n) (`f` b) a
      return (Acc (n + 1) a')
{-# INLINE foldlBySTM #-}

foldrSTM :: (Foldable f, Eq b) => (Int -> STM b) -> (a -> b -> STM b) -> STM b -> f a -> STM b
foldrSTM = foldrBySTM (returning (==))
{-# INLINE foldrSTM #-}

foldrBySTM :: Foldable f => (b -> b -> STM Bool) -> (Int -> STM b) -> (a -> b -> STM b) -> STM b -> f a -> STM b
foldrBySTM cmp g f mz = liftM extractAcc . Foldable.foldr go (liftM (Acc 0) mz)
  where
    go a mib = do
      Acc n b <- mib
      b' <- specBySTM' cmp (g n) (f a) b
      return (Acc (n + 1) b')
{-# INLINE foldrBySTM #-}

-- | Given a valid estimator @g@, @'foldl' g f z xs@ yields the same answer as @'foldl'' f z xs@.
--
-- @g n@ should supply an estimate of the value returned from folding over the first @n@ elements of the container.
--
-- If @g n@ is accurate a reasonable percentage of the time and faster to compute than the fold, then this can
-- provide increased opportunities for parallelism.

foldl  :: (Foldable f, Eq b) => (Int -> b) -> (b -> a -> b) -> b -> f a -> b
foldl = foldlBy (==) 
{-# INLINE foldl #-}

foldlBy  :: Foldable f => (b -> b -> Bool) -> (Int -> b) -> (b -> a -> b) -> b -> f a -> b
foldlBy cmp g f z = extractAcc . Foldable.foldl mf (Acc 0 z)
  where
    mf (Acc n a) b = Acc (n + 1) (specBy' cmp (g n) (`f` b) a)
{-# INLINE foldlBy #-}

foldr1 :: (Foldable f, Eq a) => (Int -> a) -> (a -> a -> a) -> f a -> a
foldr1 = foldr1By (==) 
{-# INLINE foldr1 #-}

foldr1By :: Foldable f => (a -> a -> Bool) -> (Int -> a) -> (a -> a -> a) -> f a -> a
foldr1By cmp g f xs = fromMaybeAcc (errorEmptyStructure "foldr1")
                                   (Foldable.foldr mf NothingAcc xs)
  where
    mf a (JustAcc n b) = JustAcc (n + 1) (specBy' cmp (g n) (f a) b)
    mf a NothingAcc = JustAcc 1 a
{-# INLINE foldr1By #-}

foldl1 :: (Foldable f, Eq a) => (Int -> a) -> (a -> a -> a) -> f a -> a
foldl1 = foldl1By (==)
{-# INLINE foldl1 #-}

foldl1By :: Foldable f => (a -> a -> Bool) -> (Int -> a) -> (a -> a -> a) -> f a -> a
foldl1By cmp g f xs = fromMaybeAcc (errorEmptyStructure "foldl1")
                               (Foldable.foldl mf NothingAcc xs)
  where
    mf (JustAcc n a) b = JustAcc (n + 1) (specBy' cmp (g n) (`f` b) a)
    mf NothingAcc b    = JustAcc 1 b
{-# INLINE foldl1By #-}

-- | Map each element of a structure to an action, evaluate these actions
-- from left to right and ignore the results.
traverse_ :: (Foldable t, Applicative f, Eq (f ())) => (Int -> f c) -> (a -> f b) -> t a -> f ()
traverse_ = traverseBy_ (==)
{-# INLINE traverse_ #-}

traverseBy_ :: (Foldable t, Applicative f) => (f () -> f () -> Bool) -> (Int -> f c) -> (a -> f b) -> t a -> f ()
traverseBy_ cmp g f = foldrBy cmp ((() <$) . g) ((*>) . f) (pure ())
{-# INLINE traverseBy_ #-}

-- | 'for_' is 'traverse_' with its arguments flipped.
for_ :: (Foldable t, Applicative f, Eq (f ())) => (Int -> f c) -> t a -> (a -> f b) -> f ()
for_ g = flip (traverse_ g)
{-# INLINE for_ #-}

forBy_ :: (Foldable t, Applicative f) => (f () -> f () -> Bool) -> (Int -> f c) -> t a -> (a -> f b) -> f ()
forBy_ cmp g = flip (traverseBy_ cmp g)
{-# INLINE forBy_ #-}

-- | Map each element of the structure to a monadic action, evaluating these actions
-- from left to right and ignoring the results.
mapM_ :: (Foldable t, Monad m, Eq (m ())) => (Int -> m c) -> (a -> m b) -> t a -> m ()
mapM_ = mapByM_ (==)
{-# INLINE mapM_ #-}

-- | Map each element of the structure to a monadic action, evaluating these actions
-- from left to right and ignoring the results, while transactional side-effects from 
-- mis-speculated actions are rolled back.
mapSTM_ :: Foldable t => STM Bool -> (Int -> STM c) -> (a -> STM b) -> t a -> STM ()
mapSTM_ chk g f = foldrBySTM (\_ _ -> chk) (\n -> () <$ g n) (\a _ -> () <$ f a) (return ())
{-# INLINE mapSTM_ #-}

mapByM_ :: (Foldable t, Monad m) => (m () -> m () -> Bool) -> (Int -> m c) -> (a -> m b) -> t a -> m ()
mapByM_ cmp g f = foldrBy cmp (\n -> g n >> return ()) ((>>) . f) (return ())
{-# INLINE mapByM_ #-}

-- | 'for_' is 'mapM_' with its arguments flipped.
forM_ :: (Foldable t, Monad m, Eq (m ())) => (Int -> m c) -> t a -> (a -> m b) -> m ()
forM_ g = flip (mapM_ g)
{-# INLINE forM_ #-}

-- | 'for_' is 'mapM_' with its arguments flipped.
forSTM_ :: Foldable t => STM Bool -> (Int -> STM c) -> t a -> (a -> STM b) -> STM ()
forSTM_ chk g = flip (mapSTM_ chk g)
{-# INLINE forSTM_ #-}

forByM_ :: (Foldable t, Monad m) => (m () -> m () -> Bool) -> (Int -> m c) -> t a -> (a -> m b) -> m ()
forByM_ cmp g = flip (mapByM_ cmp g)
{-# INLINE forByM_ #-}

sequenceA_ :: (Foldable t, Applicative f, Eq (f ())) => (Int -> f b) -> t (f a) -> f ()
sequenceA_ = sequenceByA_ (==)
{-# INLINE sequenceA_ #-}

sequenceByA_ :: (Foldable t, Applicative f, Eq (f ())) => (f () -> f () -> Bool) -> (Int -> f b) -> t (f a) -> f ()
sequenceByA_ cmp g = foldrBy cmp ((()<$) . g) (*>) (pure ())
{-# INLINE sequenceByA_ #-}

sequence_ :: (Foldable t, Monad m, Eq (m ())) => (Int -> m b) -> t (m a) -> m ()
sequence_ = sequenceBy_ (==) 
{-# INLINE sequence_ #-}

sequenceSTM_:: Foldable t => STM Bool -> (Int -> STM a) -> t (STM b) -> STM ()
sequenceSTM_ chk g = foldrBySTM (\_ _ -> chk) (\n -> () <$ g n) (\a _ -> () <$ a) (return ())
{-# INLINE sequenceSTM_ #-}

sequenceBy_ :: (Foldable t, Monad m) => (m () -> m () -> Bool) -> (Int -> m b) -> t (m a) -> m ()
sequenceBy_ cmp g = foldrBy cmp (\n -> g n >> return ()) (>>) (return ())
{-# INLINE sequenceBy_ #-}

asum :: (Foldable t, Alternative f, Eq (f a)) => (Int -> f a) -> t (f a) -> f a
asum = asumBy (==)
{-# INLINE asum #-}

asumBy :: (Foldable t, Alternative f) => (f a -> f a -> Bool) -> (Int -> f a) -> t (f a) -> f a
asumBy cmp g = foldrBy cmp g (<|>) empty
{-# INLINE asumBy #-}

msum  :: (Foldable t, MonadPlus m, Eq (m a)) => (Int -> m a) -> t (m a) -> m a
msum = msumBy (==) 
{-# INLINE msum #-}

msumBy  :: (Foldable t, MonadPlus m) => (m a -> m a -> Bool) -> (Int -> m a) -> t (m a) -> m a
msumBy cmp g = foldrBy cmp g mplus mzero 
{-# INLINE msumBy #-}

toList :: (Foldable t, Eq a) => (Int -> [a]) -> t a -> [a]
toList = toListBy (==)
{-# INLINE toList #-}

toListBy :: Foldable t => ([a] -> [a] -> Bool) -> (Int -> [a]) -> t a -> [a]
toListBy cmp g = foldrBy cmp g (:) []
{-# INLINE toListBy #-}

concat :: (Foldable t, Eq a) => (Int -> [a]) -> t [a] -> [a]
concat = fold
{-# INLINE concat #-}

concatBy :: Foldable t => ([a] -> [a] -> Bool) -> (Int -> [a]) -> t [a] -> [a]
concatBy = foldBy
{-# INLINE concatBy #-}

concatMap :: (Foldable t, Eq b) => (Int -> [b]) -> (a -> [b]) -> t a -> [b]
concatMap = foldMap
{-# INLINE concatMap #-}

concatMapBy :: (Foldable t) => ([b] -> [b] -> Bool) -> (Int -> [b]) -> (a -> [b]) -> t a -> [b]
concatMapBy = foldMapBy
{-# INLINE concatMapBy #-}

and :: Foldable t => (Int -> Bool) -> t Bool -> Bool
and g = getAll . foldMap (All . g) All
{-# INLINE and #-}

or :: Foldable t => (Int -> Bool) -> t Bool -> Bool
or g = getAny . foldMap (Any . g) Any
{-# INLINE or #-}

all :: Foldable t => (Int -> Bool) -> (a -> Bool) -> t a -> Bool
all g p = getAll . foldMap (All . g) (All . p)
{-# INLINE all #-}

any :: Foldable t => (Int -> Bool) -> (a -> Bool) -> t a -> Bool
any g p = getAny . foldMap (Any . g) (Any . p)
{-# INLINE any #-}

sum :: (Foldable t, Eq a, Num a) => (Int -> a) -> t a -> a
sum = sumBy (==)
{-# INLINE sum #-}

sumBy :: (Foldable t, Num a) => (a -> a -> Bool) -> (Int -> a) -> t a -> a
sumBy cmp g = getSum . foldMapBy (on cmp getSum) (Sum . g) Sum
{-# INLINE sumBy #-}

product :: (Foldable t, Eq a, Num a) => (Int -> a) -> t a -> a
product = productBy (==)
{-# INLINE product #-}

productBy :: (Foldable t, Num a) => (a -> a -> Bool) -> (Int -> a) -> t a -> a
productBy cmp g = getProduct . foldMapBy (on cmp getProduct) (Product . g) Product
{-# INLINE productBy #-}

maximum :: (Foldable t, Ord a) => (Int -> a) -> t a -> a
maximum g = foldr1 g max
{-# INLINE maximum #-}

-- TODO: allow for patching?
maximumBy :: Foldable t => (a -> a -> Ordering) -> (Int -> a) -> t a -> a
maximumBy cmp g = foldr1By cmp' g max'
  where 
    max' x y = case cmp x y of 
        GT -> x 
        _  -> y
    cmp' x y = cmp x y == EQ
{-# INLINE maximumBy #-}
        
minimum :: (Foldable t, Ord a) => (Int -> a) -> t a -> a
minimum g = foldr1 g min
{-# INLINE minimum #-}

minimumBy :: Foldable t => (a -> a -> Ordering) -> (Int -> a) -> t a -> a
minimumBy cmp g = foldr1By cmp' g min'
  where 
    min' x y = case cmp x y of 
        GT -> x 
        _  -> y
    cmp' x y = cmp x y == EQ
{-# INLINE minimumBy #-}
        
elem :: (Foldable t, Eq a) => (Int -> Bool) -> a -> t a -> Bool
elem g = any g . (==)
{-# INLINE elem #-}

elemBy :: Foldable t => (a -> a -> Bool) -> (Int -> Bool) -> a -> t a -> Bool
elemBy cmp g = any g . cmp
{-# INLINE elemBy #-}

notElem :: (Foldable t, Eq a) => (Int -> Bool) -> a -> t a -> Bool
notElem g a = not . elem g a
{-# INLINE notElem #-}

notElemBy :: Foldable t => (a -> a -> Bool) -> (Int -> Bool) -> a -> t a -> Bool
notElemBy cmp g a = not . elemBy cmp g a
{-# INLINE notElemBy #-}

find :: (Foldable t, Eq a) => (Int -> Maybe a) -> (a -> Bool) -> t a -> Maybe a
find = findBy (==) 

findBy :: Foldable t => (Maybe a -> Maybe a -> Bool) -> (Int -> Maybe a) -> (a -> Bool) -> t a -> Maybe a 
findBy cmp g p = getFirst . foldMapBy (on cmp getFirst) (First . g) (\x -> if p x then First (Just x) else First (Nothing))

