# graph-ranges

*graph-ranges* is a header-only C++ graph library designed to work with
the `<ranges>` library. 

Graphs are modelled as container adaptors, with the containers they
adapt customisable. They expose an interface quite similar to that of
the standard containers.

The use of the `<ranges>` library makes graphs amenable to
C++ coroutines; in particular, they work well with `std::generator`
with minimal additional work. Further, paradigms involving views
(again, from the `<ranges>` library) for quick and simple needs are
as easily usable with graphs as they are with the standard containers.

The policy-based design of the library provides flexibility in choosing
the underlying representation of the graph. Finer-grained control can
be obtained by specifying options in policy class instances.

## Requirements

This library requires C++23 or greater.

GCC 15.1.0 supports the C++23 standard to the extent required by the
library. Currently, the library also compiles with Clang 19.1.0
and above with a few workarounds. MSVC is not supported.

## Status

Currently, this library is only expositional. It implements only an
adjacency list graph representation and provides algorithms only for
depth-first and breadth-first traversals. However, it already
demonstrates the features described above.

## Examples

See the [`examples`](examples) directory for usage examples.
