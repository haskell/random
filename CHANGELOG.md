# 1.1
  * breaking change to `randomIValInteger` to improve RNG quality and performance
    see https://github.com/haskell/random/pull/4 and
    ghc https://ghc.haskell.org/trac/ghc/ticket/8898
  * correct documentation about generated range of Int32 sized values of type Int
    https://github.com/haskell/random/pull/7
  * fix memory leaks by using strict fields and strict atomicModifyIORef'
    https://github.com/haskell/random/pull/8
    related to ghc trac tickets  #7936 and #4218
  * support for base < 4.6 (which doesnt provide strict atomicModifyIORef')
    and integrating Travis CI support.
    https://github.com/haskell/random/pull/12
  * fix C type in test suite https://github.com/haskell/random/pull/9

# 1.0.1.1
bump for overflow bug fixes

# 1.0.1.2
bump for ticket 8704, build fusion

# 1.0.1.0
bump for bug fixes,

# 1.0.0.4
bumped version for float/double range bugfix

# 1.2

 * Breaking change which mostly maintains backwards compatibility.
 * Default generator of [splitmix](https://hackage.haskell.org/package/splitmix).
 * Faster by more x10 (depending on the type).
 * Passes a large number of random number test suites: dieharder,
   TestU01 (SmallCrush, Crush, BigCrush), PractRand, gjrand. See
   [random-quality](https://github.com/tweag/random-quality) for
   details on how to do this yourself.
 * Better quality split as judged by these [tests](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/evaluation-of-splittable-pseudorandom-generators/3EBAA9F14939C5BB5560E32D1A132637)




