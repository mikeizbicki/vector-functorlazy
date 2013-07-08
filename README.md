# Functor-lazy vectors

Functor-lazy vectors are boxed vectors that support a fast `fmap` operation.  Calling `fmap` on a functor-lazy vector takes O(1) time, but calling `fmap` on a standard [boxed vector](http://hackage.haskell.org/package/vector) takes O(n) time [*].  The downside for functor-lazy vectors is that slicing cannot be handled efficiently.  But this is not important in many applications; for example, functor-lazy vectors are used internally by the [HLearn library](http://github.com/mikeizbicki/hlearn) to provide a clean, fast interface for certain machine learning tasks.  (I'll have a detailed writeup later.)  Functor-lazy vectors are easy to use because they implement the same interface as the boxed and unboxed vectors in the `vector` module (see the [hackage documentation](http://hackage.haskell.org/package/vector-functorlazy)).  All that stream fusion goodness still works!

<sub>* Actually, it takes more like O(n<sup>1.5</sup>) time as seen in the figure below.  (It's a log-log scale, so the slope of the line is the exponent.)  This is *very* weird. </sub>

<p align="center">
<img src="https://raw.github.com/mikeizbicki/vector-functorlazy/master/img/functorlazy-v-boxed.png" />
</p>

Another downside is that the current implementation is not as efficient as boxed vectors.  For some applications, the functor-lazy vector is about 4x slower than boxed vectors.  I believe this is mostly due to cache misses ([see below](https://github.com/mikeizbicki/vector-functorlazy/#cache-misses-galore)), and that a more efficient implementation could avoid this problem.  

<p align="center">
<img src="https://raw.github.com/mikeizbicki/vector-functorlazy/master/img/algorithm-compare.png" />
</p>

In the picture above, the hashed green line represents a functor-lazy vector that has had a lazy `fmap` application before running the algorithm.  Sorting this vector requires strictly more work than sorting the functor-lazy vector without `fmap` applied, but it still runs faster.   This is one of the reasons I believe someone more familiar with CPU-level optimizations could make this data structure much more efficient.

## How they work

The easiest way to see the difference between boxed and functor-lazy vectors is by example.  I've drawn the two data structures below.

<p align="center">
<img src="https://raw.github.com/mikeizbicki/vector-functorlazy/master/img/fig1.png" />
</p>

In the boxed vector, every element is really a pointer.  They might point to values or unevaluated expressions; many elements can even point to the same thing.  The functor-lazy vector, in contrast, has a boxed vector inside of it (called *vecAny* ), an unboxed vector ( *vecInt* ), and a list of functions ( *funcList* ).

Now let's run the following code:

    fmap (*2) vector

Changes in the diagrams are highlighted below.

<p align="center">
<img src="https://raw.github.com/mikeizbicki/vector-functorlazy/master/img/fig2.png" />
</p>

The boxed vector must visit every single element and apply the `(*2)` function.  The boxes themselves are lazy and won't be evaluated immediately, but we still must create n new thunks.  That's why it takes linear time.  The functor-lazy vector doesn't visit any of the elements.  Instead, it just adds `(*2)` to *funcList*.  Internally, the library uses `unsafeCoerce` to allow us to append functions of any type to the list.

Now, let's actually visit the nodes to force evaluation of the values:

    print (vector ! 1)
    print (vector ! 4)
    
Our diagrams become:

<p align="center">
<img src="https://raw.github.com/mikeizbicki/vector-functorlazy/master/img/fig3.png" />
</p>

The boxed vector accesses the elements, sees that there is a computation waiting to be performed, permorms it, then stores the result.  The functor-lazy vector does something quite different.  First, it checks to see how many functions have been applied to the element (by looking up the appropriate index in *vecInt*).  In this case, the elements are not up-to-date because there is one function in the list, but *vecInt* says that no functions have been applied to the box.  Therefore, we apply all the functions in the list and update the box and *vecInt*.  Since we are actually modifying the values of these vectors, this requires a call to `unsafePerformIO`.  Assuming there are no bugs, this should actually be safe :)

Now, let's `fmap` once more:

    fmap (+7) vector

<p align="center">
<img src="https://raw.github.com/mikeizbicki/vector-functorlazy/master/img/fig4.png" />
</p>

And evaluate some more elements:

    print (vector ! 0)
    print (vector ! 2)

<p align="center">
<img src="https://raw.github.com/mikeizbicki/vector-functorlazy/master/img/fig5.png" />
</p>

### Cache misses galore

In the functor-lazy vector, we must access four completely different regions of memory every time we do a read.  In particular, there are two different vectors that we must access.  There is no way for both of these vectors to fit in the cache at the same time, so we are basically guaranteed to get a cache miss everytime we access a vector element.  I'm sure there must be a way to avoid this...

## Final notes

The [examples directory](https://github.com/mikeizbicki/vector-functorlazy/tree/master/src/examples) contains all the code I used for run time and correctness testing.

If you have any questions/comments/bug reports, please let me know!
