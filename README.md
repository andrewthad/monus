monus
=====

A 'Monus' is a commutative monoid that allows a notion of substraction, with the following laws:

* x <> (y - x) = y <> (x - y)
* (x - y) - z = x - (y <> z)
* x - x = mempty
* mempty - x = mempty

You can read more about them here: https://en.wikipedia.org/wiki/Monus

## Installation

Install with `cabal-install`