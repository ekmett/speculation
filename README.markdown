speculation
===========

Speculative evaluation primitives for Haskell, very loosely based on the paper "Safe Programmable Speculative Parallelism" by
Prabhu, Ramalingam, and Vaswani. <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.19.4622>

spec
----

    spec :: Eq a => a -> (a -> b) -> a -> b

`spec` takes three arguments: An initial guess as to what the argument to the function will be when it is evaluated, a function to evaluate, and the actual argument to the function. 

Spec checks to see if its actual argument has been evaluated, if so it just applies the function to the argument.

Otherwise, it begins evaluating the function with the guessed argument in parallel with evaluating the argument.

If you guessed right, then the result of applying the function to your guess is returned.

Otherwise, it then evaluates the function with the correct argument.

If a good guess is available, this permits us to increase parallelism in the resulting program.

It is subject to the following identity:

    spec a f a = a `seq` f a 

speculative folds
-----------------

A number of speculative folds are also provided via the Speculative class.

These take an extra argument which is a function that guesses the result of of the fold up to a given point.

A minimal definition for Speculative can be derived automatically for any Foldable container.
