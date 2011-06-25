

DEVLOG: A collection of notes accumulated during development.
=============================================================


[2011.06.24] (transient) Regression in stdGen performance.
----------------------------------------------------------

I just added a simple benchmark to make sure that whatever fix I
introduce for trac ticket #5133 does not regress performance.  Yet in
doing so I discovered that I'm getting much worse performance out of
rev 130e421e912d than I'm seeing in my installed random-1.0.0.3 package.

Current version:
    How many random numbers can we generate in a second on one thread?
      Cost of rdtsc (ffi call):    100
      Approx getCPUTime calls per second: 234,553
      Approx clock frequency:  3,335,220,196
      First, timing with System.Random interface:
	 68,550,189 random ints generated [constant zero gen]         ~ 48.65 cycles/int
	    900,889 random ints generated [System.Random stdGen]      ~ 3,702 cycles/int

random-1.0.0.3 version:
    How many random numbers can we generate in a second on one thread?
      Cost of rdtsc (ffi call):    75
      Approx getCPUTime calls per second: 215,332
      Approx clock frequency:  3,334,964,738
      First, timing with System.Random interface:
	 71,683,748 random ints generated [constant zero gen]         ~ 46.52 cycles/int
	 13,609,559 random ints generated [System.Random stdGen]      ~ 245 cycles/int

A >13X difference!! 
Both are compiled with the same options.  The only difference is which
System.Random is used.

When did the regression occur?  

 * e059ed15172585310f9c -- 10/13/2010 perf still good
 * 6c43f80f48178ac617   -- SplittableGen introduced, still good perf
 * 130e421e912d394653a4 -- most recent, bad performance

Ok... this is very odd.  It was a heisenbug becuase it's disappeared
now!  I'll leave this note here to help remember to look for it in the
future.
  -Ryan


[2011.06.24] Timing non-int types
---------------------------------

The results are highly uneven:

    Cost of rdtsc (ffi call):    84
    Approx getCPUTime calls per second: 220,674
    Approx clock frequency:  3,336,127,273
    First, timing with System.Random interface:
      112,976,933 randoms generated [constant zero gen]         ~ 29.53 cycles/int
       14,415,176 randoms generated [System.Random stdGen]      ~ 231 cycles/int
	   70,751 randoms generated [System.Random Floats]      ~ 47,153 cycles/int
	   70,685 randoms generated [System.Random CFloats]     ~ 47,197 cycles/int
	2,511,635 randoms generated [System.Random Doubles]     ~ 1,328 cycles/int
	   70,494 randoms generated [System.Random CDoubles]    ~ 47,325 cycles/int
	  858,012 randoms generated [System.Random Integers]    ~ 3,888 cycles/int
	4,756,213 randoms generated [System.Random Bools]       ~ 701 cycles/int

As you can see, all the types that use the generic randomIvalFrac /
randomFrac definitions perform badly.  What's more, the above results
INCLUDE an attempt to inline:

    {-# INLINE randomIvalFrac #-}
    {-# INLINE randomFrac #-}
    {-# INLINE randomIvalDouble #-}

After reimplementing random/Float these are the new results:

  Cost of rdtsc (ffi call):    100
  Approx getCPUTime calls per second: 200,582
  Approx clock frequency:  3,334,891,942
  First, timing with System.Random interface:
    105,266,949 randoms generated [constant zero gen]         ~ 31.68 cycles/int
     13,593,392 randoms generated [System.Random stdGen]      ~ 245 cycles/int
     10,962,597 randoms generated [System.Random Floats]      ~ 304 cycles/int
     11,926,573 randoms generated [System.Random CFloats]     ~ 280 cycles/int
      2,421,520 randoms generated [System.Random Doubles]     ~ 1,377 cycles/int
      2,535,087 randoms generated [System.Random CDoubles]    ~ 1,315 cycles/int
        856,276 randoms generated [System.Random Integers]    ~ 3,895 cycles/int
      4,976,373 randoms generated [System.Random Bools]       ~ 670 cycles/int

(But I still need to propagate these changes throughout all types / API calls.)


