/**
 * @file examples/adjacency_list/adjacency_list.cpp
 * 
 * Program demonstrating the construction and traversal of an
 * adjacency list-based graph using member functions as well
 * as coroutines.
 * 
 * The container @c `AdaptedVector` included from "adapted_vector.h"
 * wraps an @c `std::vector`. The wrapper can be changed to use
 * an @c `std::list` simply by including the @c `<list>` header and
 * changing the base class and the
 * member typedef @c `Base` of @c `AdaptedVector` suitably. This
 * program can then be run using the new container with no change.
 * 
 * Likewise, @c `std::unordered_map` can be replaced with @c `std::map`
 * or with a custom associative container with a similar interface.
 * For instance, @c `dat::direct_address_table` defined in
 * @c `examples/containers/direct_address_table.h` can be used as a
 * drop-in replacement.
 */

#include "../containers/adapted_vector.h"

#include "graph/adjacency_list.h"
#include "graph/graph_algorithms.h"
#include "graph/graph_traits.h"

#include <iostream>
#include <unordered_map>
#include <vector>

/// @brief Print the contents (vertices and edges) of a graph.
/// @param G Graph to print.
void print(const auto& G)
{
    for (const auto& u : G.vertices()) {
        std::cout << "Vertex " << std::get <0>(u) << ": " << std::get <1>(u) << '\n';
        std::cout << "Children: ";

        for (auto& v : G.successors(std::get <0>(u))) {
            std::cout << std::get <1>(*G.find_vertex(v)) << ' ';
        }

        std::cout << "\n\n";
    }

    std::cout << "\nEdges:\n";
    const auto V = G.vertices();
    for (auto it = std::ranges::cbegin(V); it != std::cend(V); ++it)
        for (auto [i, j, k] : G.outedges(it))
            std::cout << '(' << std::get <1>(*G.find_vertex(i)) << ", " << std::get <1>(*G.find_vertex(j)) << "): " << k << '\n';
}

int main()
{
    graph::adjacency_list <unsigned, char, char, std::unordered_map, AdaptedVector> G = {
        {
            { 1, 'A'},
            { 2, 'B'},
            { 3, 'C'},
            { 4, 'D'},
            { 5, 'E'},
            { 6, 'F'},
            { 7, 'G'},
            { 8, 'H'},
            { 9, 'I'},
            {10, 'J'}
        },
        {
            {1,  2, 'a'},
            {2,  3, 'b'},
            {2,  4, 'c'},
            {1,  5, 'd'},
            {5,  6, 'e'},
            {5,  7, 'f'},
            {7,  8, 'g'},
            {1,  8, 'h'},
            {1,  9, 'i'},
            {9, 10, 'j'}
        }
    };

    print(G);

    /// @section Depth-first traversal

    std::cout << "\nVertices depth-first with A as root:\n\n";

    for (const auto& u : graph::get_vertices_depth_first(G, 1)) {
        std::cout << std::get <1>(*G.find_vertex(u)) << ' ';
    }
    
    std::cout << '\n';

    /// @section Preorder traversal

    std::cout << "\nPreordering with A as root:\n\n";

    for (const auto& u : graph::get_preordering(G, 1)) {
        std::cout << std::get <1>(*G.find_vertex(u)) << ' ';
    }

    std::cout << '\n';

    /// @section Breadth-first traversal

    std::cout << "\nVertices breadth-first with A as root:\n\n";

    for (const auto& u : graph::get_vertices_breadth_first(G, 1)) {
        std::cout << std::get <1>(*G.find_vertex(u)) << ' ';
    }

    std::cout << '\n';
}