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

1. Breaking change which mostly maintains backwards compatibility.
2. Faster by more x10 (depending on the type) - see below for benchmarks.
3. Passes a large number of random number test suites:
  * [dieharder](http://webhome.phy.duke.edu/~rgb/General/dieharder.php "venerable")
  * [TestU01 (SmallCrush, Crush, BigCrush)](http://simul.iro.umontreal.ca/testu01/tu01.html "venerable")
  * [PractRand](http://pracrand.sourceforge.net/ "active")
  * [gjrand](http://gjrand.sourceforge.net/ "active")
  * [rademacher-fpl](https://gitlab.com/christoph-conrads/rademacher-fpl/-/tree/master "active")
  * [gjrand](http://gjrand.sourceforge.net/ "active")
  * See [random-quality](https://github.com/tweag/random-quality)
		 for details on how to do this yourself.
4. Better quality split as judged by these
	[tests](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/evaluation-of-splittable-pseudorandom-generators/3EBAA9F14939C5BB5560E32D1A132637). Again
	see [random-quality](https://github.com/tweag/random-quality) for
	details on how to do this yourself.
5. Unbiased generation of ranges.
6. Updated tests and benchmarks.
7. [Conntinuous integration](https://travis-ci.org/github/idontgetoutmuch/random).
8. Fully documented - for more details see the [haddock](https://htmlpreview.github.io/?https://github.com/idontgetoutmuch/random/blob/release-notes/docs/System-Random.html).

## Benchmarks

### Notes

1. These are **not** percentage (%) increases. Random `Int`s are produced 48.9 times faster!
2. The only type for which generation is slower is for `Integer`s (on
   ranges); in the version 1.1 the generation for `Integer` was
   biased.

### Without Specifying Ranges

    |----------|----------------|----------------|----------------------|
	| Type     | Cycles/Int 1.1 | Cycles/Int 1.2 | Performance Increase |
	|----------|----------------|----------------|----------------------|
	| Ints     |           1508 |          30.84 |                 48.9 |
	| Word16   |            495 |          30.88 |                 16.0 |
	| Floats   |           1036 |          35.11 |                 29.5 |
	| CFloats  |           1054 |          33.75 |                 31.2 |
	| Doubles  |           1875 |          35.77 |                 52.4 |
	| CDoubles |            908 |          33.31 |                 27.3 |
	| Integers |           1578 |          33.09 |                 47.7 |
	| Bools    |            698 |          36.15 |                 19.3 |
	| Chars    |            693 |           57.6 |                 12.0 |
	|----------|----------------|----------------|----------------------|

### Specifying Ranges

	|--------------|----------------|----------------|----------------------|
	| Type         | Cycles/Int 1.1 | Cycles/Int 1.2 | Performance Increase |
	|--------------|----------------|----------------|----------------------|
	| Ints         |            734 |            102 |                  7.2 |
	| Word16s      |            748 |            115 |                  6.5 |
	| Floats       |           2055 |          35.88 |                 57.3 |
	| CFloats      |           1071 |          34.96 |                 30.6 |
	| Doubles      |           3050 |          35.89 |                 85.0 |
	| CDoubles     |           1112 |          34.87 |                 31.9 |
	| Integers     |            534 |            868 |                  0.6 |
	| Bools        |            739 |          35.22 |                 21.0 |
	| Chars        |            790 |            133 |                  5.9 |
	| BIG Integers |         199848 |         103056 |                  1.9 |
	|--------------|----------------|----------------|----------------------|




