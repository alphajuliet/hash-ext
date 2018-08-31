hash-ext
========

This provides extensions to the existing `racket/hash` library for doing calculations over hashes
of numerical values. They are effectively treated as vectors with keyed elements. Hashes are combined first
with a union or intersection, depending on the function.

See the unit tests in `main.rkt` for examples.

The exported functions are:

* `(hash-add h1 h2)`: add two hashes, element-wise
* `(hash-mul h1 h2)`: multiply two hashes, element-wise
* `(hash-sum h)`: sum the elements of the hash
* `(hash-combine f g h1 h2)`: a general function that combines `h1` and `h2` with function `f` and then applies
`g` pair-wise. For example, `hash-mul` is implemented defining `f` as `hash-intersection` and `g` as `*`.
* `(my-hash-map f h)`: map a function `f` over a hash, and return the hash, unlike the `racket/hash` version
* `(hash-intersection h1 h2)`: implement the missing intersection function of two hashes
* `(hash-dotp h1 h2)`: pair-wise multiply and sum
* `(hash-scale n h)`: scale all hash values by `n`
