# graph-ranges

*graph-ranges* is a header-only C++ library designed to work with
the `<ranges>` library. 

Graphs are modelled as container adaptors, with the containers they
adapt customisable. They expose an interface quite similar to that of
the standard containers.

The use of the `<ranges>` library makes graphs amenable to
C++ coroutines; in particular, they work well with `std::generator`
with minimal additional work. Further, paradigms involving views
(again, from the `<ranges>` library) for quick and simple needs are
as easily usable with graphs as they are with the standard containers.

## Requirements

This library requires C++23 or greater.

## Status

Currently, this library is only expositional. It implements only an
adjacency list graph representation and provides algorithms only for
depth-first traversal. However, it already demonstrates the features
described above.
