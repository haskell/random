

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



 [2011.06.26] Comparing against other Hackage RNG packagas.
----------------------------------------------------------

Here are some new results comparing against mwc-random and mersenne-random-pure64:

    How many random numbers can we generate in a second on one thread?
      Cost of rdtsc (ffi call):    533
      Approx getCPUTime calls per second: 269,011
      Approx clock frequency:  3,335,818,164
      First, timing System.Random.next:
	192,084,254 randoms generated [constant zero gen]         ~ 17.37 cycles/int
	 14,609,734 randoms generated [System.Random stdGen/next] ~ 228 cycles/int

      Second, timing System.Random.random at different types:
	    829,214 randoms generated [System.Random Ints]        ~ 4,023 cycles/int
	  5,375,807 randoms generated [System.Random Word16]      ~ 621 cycles/int
	  2,549,551 randoms generated [System.Random Floats]      ~ 1,308 cycles/int
	  2,497,583 randoms generated [System.Random CFloats]     ~ 1,336 cycles/int
	    854,666 randoms generated [System.Random Doubles]     ~ 3,903 cycles/int
	  2,469,775 randoms generated [System.Random CDoubles]    ~ 1,351 cycles/int
	    826,764 randoms generated [System.Random Integers]    ~ 4,035 cycles/int
	  4,660,286 randoms generated [System.Random Bools]       ~ 716 cycles/int

      Next test other RNG packages on Hackage:
	 96,085,080 randoms generated [System.Random.Mersenne.Pure64 next] ~ 34.72 cycles/int
	    757,057 randoms generated [System.Random.Mersenne.Pure64 Ints] ~ 4,406 cycles/int
	  1,395,998 randoms generated [System.Random.Mersenne.Pure64 Floats] ~ 2,390 cycles/int
	 19,629,482 randoms generated [System.Random.MWC next]    ~ 170 cycles/int
	    716,775 randoms generated [System.Random.MWC Ints]    ~ 4,654 cycles/int
	  1,348,543 randoms generated [System.Random.MWC Floats]  ~ 2,474 cycles/int

      Next timing range-restricted System.Random.randomR:
	  5,113,175 randoms generated [System.Random Ints]        ~ 652 cycles/int
	  5,187,887 randoms generated [System.Random Word16s]     ~ 643 cycles/int
	    109,662 randoms generated [System.Random Floats]      ~ 30,419 cycles/int
	    109,762 randoms generated [System.Random CFloats]     ~ 30,391 cycles/int
	  2,429,840 randoms generated [System.Random Doubles]     ~ 1,373 cycles/int
	    108,239 randoms generated [System.Random CDoubles]    ~ 30,819 cycles/int
	  4,641,610 randoms generated [System.Random Integers]    ~ 719 cycles/int
	  4,745,208 randoms generated [System.Random Bools]       ~ 703 cycles/int
    Finished.


 [2011.06.26] Created new branch, added genBits to the API
----------------------------------------------------------

genBits enabled tweaking the Float instance to bring the performance back up:

	 15,037,231 randoms generated [System.Random stdGen/next] ~ 222 cycles/int
      Second, timing System.Random.random at different types:
	    833,750 randoms generated [System.Random Ints]        ~ 4,013 cycles/int
	  5,486,673 randoms generated [System.Random Word16]      ~ 610 cycles/int
	 13,027,632 randoms generated [System.Random Floats]      ~ 257 cycles/int
	 12,788,272 randoms generated [System.Random CFloats]     ~ 262 cycles/int
	    818,754 randoms generated [System.Random Doubles]     ~ 4,086 cycles/int
	  2,507,631 randoms generated [System.Random CDoubles]    ~ 1,334 cycles/int
	    885,137 randoms generated [System.Random Integers]    ~ 3,780 cycles/int
	  4,999,175 randoms generated [System.Random Bools]       ~ 669 cycles/int

Next, I added a new "randomBits" function that uses genBits to fill up
N random bits, which is sufficient for the non-range restricted
randoms.  This improved all-around performance:

    Second, timing System.Random.random at different types:
      4,568,027 randoms generated [System.Random Ints]        ~ 730 cycles/int
     12,526,044 randoms generated [System.Random Word16]      ~ 266 cycles/int
      6,134,361 randoms generated [System.Random Word32]      ~ 544 cycles/int
     12,729,520 randoms generated [System.Random Floats]      ~ 262 cycles/int
     12,827,441 randoms generated [System.Random CFloats]     ~ 260 cycles/int
      4,213,043 randoms generated [System.Random Doubles]     ~ 792 cycles/int
      2,273,839 randoms generated [System.Random CDoubles]    ~ 1,467 cycles/int
        842,605 randoms generated [System.Random Integers]    ~ 3,959 cycles/int
      5,060,505 randoms generated [System.Random Bools]       ~ 659 cycles/int

Now all are in the millions at least except Integers.


 [2011.06.27] Implemented randomIvalBits to replace randomIvalIntegral
----------------------------------------------------------------------

Initially this appears to slow it down slightly.  

 randomIvalBits:
      Next timing range-restricted System.Random.randomR:
	  4,052,337 randoms generated [System.Random Ints]        ~ 823 cycles/int
	  4,802,383 randoms generated [System.Random Word16s]     ~ 695 cycles/int
	    111,996 randoms generated [System.Random Floats]      ~ 29,789 cycles/int
	    111,401 randoms generated [System.Random CFloats]     ~ 29,948 cycles/int
	  2,656,560 randoms generated [System.Random Doubles]     ~ 1,256 cycles/int
	    108,190 randoms generated [System.Random CDoubles]    ~ 30,837 cycles/int
	  5,152,061 randoms generated [System.Random Integers]    ~ 648 cycles/int
	  4,833,272 randoms generated [System.Random Bools]       ~ 690 cycles/int

 randomIvalIntegral:

      Next timing range-restricted System.Random.randomR:
	  5,246,059 randoms generated [System.Random Ints]        ~ 638 cycles/int
	  5,310,547 randoms generated [System.Random Word16s]     ~ 630 cycles/int
	    110,109 randoms generated [System.Random Floats]      ~ 30,378 cycles/int
	    110,010 randoms generated [System.Random CFloats]     ~ 30,405 cycles/int
	  2,290,480 randoms generated [System.Random Doubles]     ~ 1,460 cycles/int
	    107,945 randoms generated [System.Random CDoubles]    ~ 30,987 cycles/int
	  5,161,738 randoms generated [System.Random Integers]    ~ 648 cycles/int
	  4,759,361 randoms generated [System.Random Bools]       ~ 703 cycles/int

This is actually a good result!  This prototype version of
randomIvalBits uses a very inefficient bitScanReverse which can be
improved.  And in spite of that it didn't slow down TOO much.  Also,
randomIvalBits can fix the problems in tickets #5278 and #5280 having
to do with uniformity and assumptions about the generators.


Oops, there were some bugs.  Here are new times as of revision 3581598e57ef995f:

    Next timing range-restricted System.Random.randomR:
	3,738,614 randoms generated [System.Random Ints]        ~ 892 cycles/int
	7,516,652 randoms generated [System.Random Word16s]     ~ 444 cycles/int
	  110,307 randoms generated [System.Random Floats]      ~ 30,234 cycles/int
	  110,507 randoms generated [System.Random CFloats]     ~ 30,179 cycles/int
	2,538,000 randoms generated [System.Random Doubles]     ~ 1,314 cycles/int
	  108,386 randoms generated [System.Random CDoubles]    ~ 30,770 cycles/int
	5,398,820 randoms generated [System.Random Integers]    ~ 618 cycles/int
	4,758,575 randoms generated [System.Random Bools]       ~ 701 cycles/int

Finally, in revision a837e1ffb294234dc I tweaked the restricted
Float/Double instances to use the new versions:

    Next timing range-restricted System.Random.randomR:
	4,015,910 randoms generated [System.Random Ints]        ~ 831 cycles/int
	7,572,249 randoms generated [System.Random Word16s]     ~ 440 cycles/int
       12,768,688 randoms generated [System.Random Floats]      ~ 261 cycles/int
       12,716,471 randoms generated [System.Random CFloats]     ~ 262 cycles/int
	3,948,403 randoms generated [System.Random Doubles]     ~ 845 cycles/int
	2,469,778 randoms generated [System.Random CDoubles]    ~ 1,350 cycles/int
	4,542,423 randoms generated [System.Random Integers]    ~ 734 cycles/int
	4,884,380 randoms generated [System.Random Bools]       ~ 683 cycles/int

Why would Floats be faster than Word16s though?  Still some exploring left to do...

