# Functor-lazy vectors

Functor-lazy vectors are boxed vectors that support a fast fmap operation.  Calling `fmap` on a functor-lazy vector takes O(1) time, but calling `fmap` on a standard boxed vector takes O(n) time.  The downside is that slicing cannot be handled efficiently.  They are used internally by the [HLearn library](http://github.com/mikeizbicki/hlearn) to provide a clean, fast interface for certain machine learning tasks.  (I'll have a detailed writeup later.)

Functor-lazy vectors are easy to use because they implement the same interface as the boxed and unboxed vectors in the `vector` module (see the [hackage documentation](http://hackage.haskell.org/package/vector-functorlazy)).

This README first explains how functor-lazy vectors work internally, then gives a detailed performance comparison with boxed vectors.  They are quite fast :)

## How they work

A standard boxed vector can be drawn as: 

Every element in the vector is really a pointer.  They might point to values or unevaluated expressions; many elements can even point to the same thing.


## Performance

Functor-lazy vectors have good performance.

We can also compare performance on some standard algorithms.  The [vector-algorithms](http://hackage.haskell.org/package/vector-algorithms) package has already implemented many common tests for us.

## Final notes

If you have any questions/comments/suggestions, please let me know!
