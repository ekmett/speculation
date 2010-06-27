speculation
===========

This package provides speculative evaluation primitives for Haskell, very loosely based on the paper 
"Safe Programmable Speculative Parallelism" by Prabhu, Ramalingam, and Vaswani. 

<http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.19.4622>

## Combinators

### speculative function application

#### spec

    spec :: Eq a => a -> (a -> b) -> a -> b

`spec g f a` evaluates `f g` while forcing `a`, if `g == a` then `f g` is returned. Otherwise `f a` is evaluated.

Furthermore, if the argument has already been evaluated, we avoid sparking the parallel computation at all.

If `g` is a good guess at the value of `a`, this is one way to induce parallelism in an otherwise sequential task.

However, if `g` isn\'t available more cheaply than `a`, then this saves no work, and if `g` is wrong, you risk evaluating the function twice.
    spec a f a = f $! a
    
The best-case timeline looks like:
    [---- f g ----]
       [----- a -----]
    [-- spec g f a --]

The worst-case timeline looks like:
    [---- f g ----]
       [----- a -----]
                     [---- f a ----]
    [------- spec g f a -----------]
    
Compare these to the timeline of @f $! a@:
    [---- a -----]
                 [---- f a ----]

#### specSTM

`specSTM` provides a similar compressed timeline for speculated STM actions, but also rolls back side-effects.

### folds

A number of speculative folds are also provided.
    
These take an extra argument which is a function that guesses the result of of the fold up to a given point.

#### specFoldr

    specFoldr :: (Foldable f, Eq b) => (Int -> b) -> (a -> b -> b) -> b -> f a -> b

Given a valid estimator `g`, `'specFoldr g f z xs` yields the same answer as `foldr' f z xs`.

`g n` should supply an estimate of the value returned from folding over the /last/ `n` elements of the container.

As with `spec`, if the guess `g n` is accurate a reasonable percentage of the time and faster to compute than the fold, then this can provide increased opportunities for parallelism.

#### specFoldl

    specFoldl :: (Foldable f, Eq b) => (Int -> b) -> (b -> a -> b) -> b -> f a -> b

`specFoldl` works similarly to `foldl'`, except that `g n` should provide an estimate for the /first/ `n` elements.
