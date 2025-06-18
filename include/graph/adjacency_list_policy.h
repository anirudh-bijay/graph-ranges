/**
 * @file graph/adjacency_list_policy.h
 * 
 * Policy to indicate that the underlying representation of a graph
 * is an adjacency list and to specify its properties.
 */

#pragma once

#include <unordered_map>
#include <vector>

namespace graph
{
    template <
        typename VertexKey,
        typename VertexData,
        typename EdgeData,
        template <typename Key, typename T> class VertexMap = std::unordered_map,
        template <typename T> class EdgeList = std::vector
    >
    struct adjacency_list_policy
    {
        using vertex_key    = VertexKey;    // Vertex key type.
        using vertex_data   = VertexData;   // Vertex mapped type.
        using edge_data     = EdgeData;     // Edge data type.

        template <typename Key, typename T>
        using vertex_map    = VertexMap <Key, T>;   // Vertex container type.
        template <typename T>
        using edge_list     = EdgeList <T>;         // Edge container type.

        bool is_directed = true;                // Whether the graph is directed.
        bool is_multigraph = true;              // Whether the graph is a multigraph.
        bool forbid_self_loops = false;         // Whether self-loops are allowed.
        bool support_inedge_iteration = false;  // Whether to support in-edge iteration.

        constexpr bool operator==(const adjacency_list_policy&) const noexcept = default;
    };

} // namespace graph