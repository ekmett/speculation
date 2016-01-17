/1.5.0.3/

 * Support GHC 8
 * Support `transformers` 0.5

/1.5.0.2/

 * Build warning-free on GHC 7.10+
 * Added an HLint configuration
 * Removed a redundant constraint from the type of `sequenceByA_`.
 * Removed a redundant `Monad m` constraint from `instance MonadSpec (ContT r m)`.

/1.5/

 * Removed the use of `tag-bits`. This enables the API to be `Trustworthy`.

/1.4/

 * Simplified MonadSpec

/1.3/

 * Removed old benchmark/test framework.
 * Make numSparks support mandatory.
 * Moved to Control.Concurrent from Data
 * Added MonadSpec, so we can make instances for Codensity, etc. in other packages
 * Removed the ContT r STM combinators
 * Cleaned out old issues
 * Made compatible with the removal of the Eq/Show superclasses of Num in GHC 7.3+

/1.2.0.2/

 * Fixed name collision with the new Distribution.Simple.testHook in Setup.lhs

/1.2.0.1/

 * Weakened dependencies

/1.2.0.0/:

 * Reorganized the module hierarchy into Data.Speculation

/1.1.0.0/:

 * Added support for numSparks

/1.0.0.0/:

 * Released

/0.9.0/:

 * Removed interim boxing in the unsafeIsEvaluated and unsafeGetTagBits check

/0.8.1/:

 * Added Data.List.Foldable
 * Added Data.Traversable.Foldable
 * Fixed an off-by-one error in the arguments to the speculative fold estimators

/0.8.0.2/:

 * changed tests and benchmarks to not build by default to work around corruption in the hackage db

/0.8.0.1/:

 * test suite now forces build
