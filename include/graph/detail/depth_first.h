/**
 * @file graph/detail/depth_first.h
 * 
 * Coroutines to perform depth-first traversal and related traversals,
 * such as preorder and postorder traversals.
 */

#pragma once

#include "graph/graph_traits.h"

#include <generator>
#include <ranges>
#include <stack>
#include <vector>

#if __cpp_impl_coroutine < 201902L
#warning This header requires support for C++ coroutines.
#endif

#if __cpp_lib_ranges < 202211L
#warning This header requires support for C++23 <ranges>.
#endif

namespace graph
{
    /// @brief Obtain a generator that traverses a graph depth-first
    ///        starting at a specified node and yields the keys to all
    ///        vertices encountered on the way.
    /// 
    ///        All nodes (including leaf nodes) are yielded twice: once
    ///        when entering the subtree rooted at the node, and once
    ///        when exiting it.
    /// 
    /// @tparam G The type of the graph to traverse.
    /// @param g The graph to traverse.
    /// @param root The key of the node to start traversal from
    ///             (the root of the DFS tree).
    /// 
    /// @return An @c `std::generator` that yields vertex keys as
    ///         described above.
    /// 
    /// @warning @c `g` must not be modified during traversal. If
    ///          @c `g` needs to be modified, pass a copy of @c `g`
    ///          to the function and work on the original.
    template <class G>
    std::generator <typename graph_traits <G>::vertex_key_type>
    get_vertices_depth_first(const G& g, const typename graph_traits <G>::vertex_key_type& root)
    {
        enum State : unsigned char
        {
            unvisited = 0,
            inside_subtree,
            subtree_visited
        };

        std::stack <typename graph_traits <G>::vertex_key_type> stack;

        typename graph_traits <G>::template vertex_container <
            typename graph_traits <G>::vertex_key_type,
            enum State
        > vertex_state
#ifndef __clang__
        (
            std::from_range,
            std::views::zip(g.vertices() | std::views::keys, std::views::repeat(unvisited))
        ); // Initialise vertex states to unvisited.
#else   /** Clang does not support @c `std::from_range` yet. */
        ;
        for (const auto& v : g.vertices() | std::views::keys) {
            vertex_state.emplace(v, unvisited);
        }
#endif

        stack.push(root);

        do {
            const auto& u = stack.top();

            switch (auto& state_ref = vertex_state[u]; state_ref) {
                case unvisited:
                    co_yield u;

                    state_ref = inside_subtree;

                    for (const auto& v : g.successors(u)) {
                        if (vertex_state[v] == unvisited) {
                            stack.push(v);
                        }
                    }

                    break;

                case inside_subtree:
                    co_yield u;
                    stack.pop(); // Scheduled after yielding to prevent a dangling reference.

                    state_ref = subtree_visited;
                    break;

                default:
                    stack.pop();
            }
        } while (!stack.empty());

        co_return;
    }

    /// @brief Obtain a generator that traverses a graph depth-first
    ///        starting at a specified vertex and yields the keys to all
    ///        vertices encountered on the way.
    /// 
    ///        All nodes (including leaf nodes) are yielded twice: once
    ///        when entering the subtree rooted at the node, and once
    ///        when exiting it.
    /// 
    /// @tparam G The type of the graph to traverse.
    /// @param g The graph to traverse.
    /// @param root_iter Iterator to the node to start traversal from
    ///                  (the root of the DFS tree).
    /// 
    /// @return An @c `std::generator` that yields vertex keys as
    ///         described above.
    /// 
    /// @warning @c `g` must not be modified during traversal. If
    ///          @c `g` needs to be modified, pass a copy of @c `g`
    ///          to the function and work on the original.
    template <class G>
    std::generator <typename graph_traits <G>::vertex_key_type>
    get_vertices_depth_first(const G& g, const typename graph_traits <G>::vertex_iterator root_iter)
    {
        return get_vertices_depth_first(g, std::get <0>(*root_iter));
    }

    /// @brief Obtain a generator that performs a preorder traversal of
    ///        a graph starting at a specified vertex and yields the
    ///        keys to vertices in the order of
    ///        traversal.
    /// 
    ///        The generator traverses only those vertices that are
    ///        reachable from the specified start vertex (the root of
    ///        the corresponding DFS tree).
    /// 
    /// @tparam G The type of the graph to traverse.
    /// @param g The graph to traverse.
    /// @param root The key of the node to start traversal from
    ///             (the root of the DFS tree).
    /// 
    /// @return An @c `std::generator` that yields vertex keys as
    ///         described above.
    /// 
    /// @warning @c `g` must not be modified during traversal. If
    ///          @c `g` needs to be modified, pass a copy of @c `g`
    ///          to the function and work on the original.
    template <class G>
    std::generator <typename graph_traits <G>::vertex_key_type>
    get_preordering(const G& g, const typename graph_traits <G>::vertex_key_type& root)
    {
        enum State : bool
        {
            unvisited = 0,
            visited
        };

        std::stack <typename graph_traits <G>::vertex_key_type> stack;

        typename graph_traits <G>::template vertex_container <
            typename graph_traits <G>::vertex_key_type,
            enum State
        > vertex_state
#ifndef __clang__
        (
            std::from_range,
            std::views::zip(g.vertices() | std::views::keys, std::views::repeat(unvisited))
        ); // Initialise vertex states to unvisited.
#else   /** Clang does not support @c `std::from_range` yet. */
        ;
        for (const auto& v : g.vertices() | std::views::keys) {
            vertex_state.emplace(v, unvisited);
        }
#endif

        stack.push(root);

        do {
            const auto& u = stack.top();

            switch (auto& state_ref = vertex_state[u]; state_ref) {
                case unvisited:
                    co_yield u;

                    state_ref = visited;

                    for (const auto& v : g.successors(u)) {
                        if (vertex_state[v] == unvisited) {
                            stack.push(v);
                        }
                    }

                    break;

                default:
                    stack.pop();
            }
        } while (!stack.empty());

        co_return;
    }

    /// @brief Obtain a generator that performs a postorder traversal of
    ///        a graph starting at a specified vertex and yields the
    ///        keys to vertices in the order of
    ///        traversal.
    /// 
    ///        The generator traverses only those vertices that are
    ///        reachable from the specified start vertex (the root of
    ///        the corresponding DFS tree).
    /// 
    /// @tparam G The type of the graph to traverse.
    /// @param g The graph to traverse.
    /// @param root The key of the node to start traversal from
    ///             (the root of the DFS tree).
    /// 
    /// @return An @c `std::generator` that yields vertex keys as
    ///         described above.
    /// 
    /// @warning @c `g` must not be modified during traversal. If
    ///          @c `g` needs to be modified, pass a copy of @c `g`
    ///          to the function and work on the original.
    template <class G>
    std::generator <typename graph_traits <G>::vertex_type>
    get_postordering(const G& g, const typename graph_traits <G>::vertex_iterator root)
    {
        enum State : unsigned char
        {
            unvisited = 0,
            inside_subtree,
            subtree_visited
        };

        std::stack <typename graph_traits <G>::vertex_key_type> stack;

        typename graph_traits <G>::template vertex_container <
            typename graph_traits <G>::vertex_key_type,
            enum State
        > vertex_state
#ifndef __clang__
        (
            std::from_range,
            std::views::zip(g.vertices() | std::views::keys, std::views::repeat(unvisited))
        ); // Initialise vertex states to unvisited.
#else   /** Clang does not support @c `std::from_range` yet. */
        ;
        for (const auto& v : g.vertices() | std::views::keys) {
            vertex_state.emplace(v, unvisited);
        }
#endif

        stack.push(root);

        do {
            const auto& u = stack.top();

            switch (auto& state_ref = vertex_state[u]; state_ref) {
                case unvisited:
                    state_ref = inside_subtree;

                    for (const auto& v : g.successors(u)) {
                        if (vertex_state[v] == unvisited) {
                            stack.push(v);
                        }
                    }

                    break;

                case inside_subtree:
                    co_yield u;
                    stack.pop(); // Scheduled after yielding to prevent a dangling reference.

                    state_ref = subtree_visited;
                    break;

                default:
                    stack.pop();
            }
        } while (!stack.empty());

        co_return;
    }

} // namespace graph