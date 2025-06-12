/**
 * @file graph/detail/breadth_first.h
 * 
 * Coroutines to perform breadth-first traversal and related traversals.
 */

#pragma once

#include "graph/graph_traits.h"

#include <generator>
#include <queue>
#include <ranges>
#include <vector>

#if __cpp_impl_coroutine < 201902L
#warning This header requires support for C++ coroutines.
#endif

#if __cpp_lib_ranges < 202211L
#warning This header requires support for C++23 <ranges>.
#endif

namespace graph
{
    /// @brief Obtain a generator that traverses a graph breadth-first
    ///        starting at a specified node and yields the keys to all
    ///        vertices encountered on the way.
    /// 
    ///        All nodes in the connected component of the graph the
    ///        specified root node belongs to are yielded exactly once.
    /// 
    /// @tparam G The type of the graph to traverse.
    /// @param g The graph to traverse.
    /// @param root The key of the node to start traversal from.
    /// 
    /// @return An @c `std::generator` that yields vertex keys as
    ///         described above.
    /// 
    /// @warning @c `g` must not be modified during traversal. If
    ///          @c `g` needs to be modified, pass a copy of @c `g`
    ///          to the function and work on the original.
    template <class G>
    std::generator <typename graph_traits <G>::vertex_key_type>
    get_vertices_breadth_first(const G& g, const typename graph_traits <G>::vertex_key_type& root)
    {
        enum State : bool
        {
            unvisited = 0,
            visited
        };

        std::queue <typename graph_traits <G>::vertex_key_type> queue;

        typename graph_traits <G>::vertex_container <
            typename graph_traits <G>::vertex_key_type,
            enum State
        > vertex_state(
            std::from_range,
            std::views::zip(g.vertices() | std::views::keys, std::views::repeat(unvisited))
        ); // Initialise vertex states to unvisited.

        queue.push(root);
        vertex_state[root] = visited;

        do {
            const auto u = std::move(queue.front()); // Copy to avoid dangling reference.
            queue.pop();
            co_yield u;

            for (const auto& v : g.successors(u)) {
                if (vertex_state[v] == unvisited) {
                    queue.push(v);
                    vertex_state[v] = visited;
                }
            }
        } while (!queue.empty());

        co_return;
    }

} // namespace graph