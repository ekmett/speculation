speculation
===========

This package provides speculative evaluation primitives for Haskell, very loosely based on the paper 
"Safe Programmable Speculative Parallelism" by Prabhu, Ramalingam, and Vaswani. 

<http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.19.4622>

You can download it using 'cabal install speculation', if you have the Haskell Platform installed.

## Combinators

### Speculative Function Application

Various speculative function application combinators are provided. Two fairly canonical samples are described here.

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

### Speculative Folds

A speculative version of Data.Foldable is provided as Data.Foldable.Speculation.
    
Each combinator therein takes an extra argument that is used to speculate on the value of the list.

#### foldr

    foldr :: (Foldable f, Eq b) => (Int -> b) -> (a -> b -> b) -> b -> f a -> b

Given a valid estimator `g`, `'foldr g f z xs` yields the same answer as `Foldable.foldr' f z xs`.

`g n` should supply an estimate of the value returned from folding over the **last** `n` elements of the container.

As with `spec`, if the guess `g n` is accurate a reasonable percentage of the time and faster to compute than the fold, then this can provide increased opportunities for parallelism.

#### foldl

    foldl :: (Foldable f, Eq b) => (Int -> b) -> (b -> a -> b) -> b -> f a -> b

`foldl` works similarly to `Foldable.foldl'`, except that `g n` should provide an estimate for the **first** `n` elements.

Contact Information
-------------------

I can be reached through the user ekmett on github, as edwardk on irc.freenode.net #haskell channel, or by email to <ekmett@gmail.com>.

-Edward Kmett
