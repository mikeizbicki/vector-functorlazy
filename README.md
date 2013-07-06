# Functor-lazy vectors

Functor-lazy vectors are boxed vectors that support a fast `fmap` operation.  Calling `fmap` on a functor-lazy vector takes O(1) time, but calling `fmap` on a standard [boxed vector](http://hackage.haskell.org/package/vector) takes O(n) time.  The downside for functor-lazy vectors is that slicing cannot be handled efficiently.  But this is not important in many applications; for example, they are used internally by the [HLearn library](http://github.com/mikeizbicki/hlearn) to provide a clean, fast interface for certain machine learning tasks.  (I'll have a detailed writeup later.)  Functor-lazy vectors are easy to use because they implement the same interface as the boxed and unboxed vectors in the `vector` module (see the [hackage documentation](http://hackage.haskell.org/package/vector-functorlazy)).

<p align="center">
<img src="https://raw.github.com/mikeizbicki/vector-functorlazy/master/img/functorlazy-v-boxed.png" />
</p>

Another downside is that the current implementation is not as efficient as boxed vectors.  For some applications, the functor-lazy vector can be almost 4x slower than boxed vectors.  I believe this is mostly due to cache misses (see below), and that a more efficient implementation could avoid this problem.  

<p align="center">
<img src="https://raw.github.com/mikeizbicki/vector-functorlazy/master/img/algorithm-compare.png" />
</p>

In the picture above, the hashed green line represents a functor-lazy vector that has had a lazy `fmap` application before running the algorithm.  Sorting this vector requires strictly more work than sorting the functor-lazy vector without `fmap` applied.  But for some reason, it still runs faster.   This is one of the reasons I believe someone more familiar with CPU-level optimizations could make this data structure much more efficient.

## How they work

A standard boxed vector can be drawn as: 

Every element in the vector is really a pointer.  They might point to values or unevaluated expressions; many elements can even point to the same thing.

## Final notes

The [examples directory](https://github.com/mikeizbicki/vector-functorlazy/tree/master/src/examples) contains all the code I used for run time and correctness testing.

If you have any questions/comments/suggestions, please let me know!
