/**
 * @file tests/adjacency_list/directed_multigraph.cpp
 */

#include "direct_address_table.h"

#include <gtest/gtest.h>
#include <graph/graph.h>
#include <graph/sequence_containers.h>

#include <map>
#include <ranges>
#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>

namespace
{
    TEST(DirectedAdjacencyListMultigraph, TemplateParameters)
    {
        /// @brief Test the directed multigraph with template parameters.
        
        // Create a directed multigraph (with unsupported edge list type `std::vector`)
        graph::graph <
            graph::adjacency_list_policy <
                int, std::string, std::pair <int, int>, std::unordered_map, std::vector
            >{
                .is_directed = true,
                .is_multigraph = true,
                .forbid_self_loops = false,
                .support_inedge_iteration = false
            }
        > g1;

        struct Empty {};

        // Create a directed multigraph (with unsupported edge list type `std::vector`)
        // using an empty vertex data type and edge weight type.
        graph::graph <
            graph::adjacency_list_policy <
                int, Empty, Empty, std::map, std::vector
            >{
                .is_directed = true,
                .is_multigraph = true,
                .forbid_self_loops = false,
                .support_inedge_iteration = false
            }
        > g2;
    }

    TEST(DirectedAdjacencyListMultigraph, Constructors)
    {
        /// @section Test construction from initializer lists.
        
        graph::graph <
            graph::adjacency_list_policy <
                int, std::string, std::pair <int, int>, std::unordered_map, graph::vector
            >{
                .is_directed = true,
                .is_multigraph = true,
                .forbid_self_loops = false,
                .support_inedge_iteration = false
            }
        > g1{
            {{1, "A"}, {2, "B"}, {3, "C"}},
            {{1, 2, {1, 2}}, {2, 3, {2, 3}}, {1, 3, {1, 3}}}
        };

        // Constexpr construction
        // Expects the constexpr member function `size()` to be defined.
        constexpr auto size = graph::graph <
            graph::adjacency_list_policy <
                unsigned, std::string, std::pair <int, int>, dat::direct_address_table, graph::vector
            >{
                .is_directed = true,
                .is_multigraph = true,
                .forbid_self_loops = false,
                .support_inedge_iteration = false
            }
        >{
            {{1, "A"}, {2, "B"}, {3, "C"}},
            {{1, 2, {1, 2}}, {2, 3, {2, 3}}, {1, 3, {1, 3}}}
        }.size();

        /// @section Test construction from range.
        graph::graph <
            graph::adjacency_list_policy <
                int, std::string, std::pair <int, int>, std::unordered_map, graph::vector
            >{
                .is_directed = true,
                .is_multigraph = true,
                .forbid_self_loops = false,
                .support_inedge_iteration = false
            }
        > g2{
            std::vector {std::make_pair(1, "A"), {2, "B"}, {3, "C"}}
        };

        /// @section Test construction from range.
        graph::graph <
            graph::adjacency_list_policy <
                int, std::string, char, std::unordered_map, graph::vector
            >{
                .is_directed = true,
                .is_multigraph = true,
                .forbid_self_loops = false,
                .support_inedge_iteration = false
            }
        > g3{
            std::vector {std::make_pair(1, "A"), {2, "B"}, {3, "C"}},
            std::vector {std::make_tuple(1, 2, 'a'), {2, 3, 'b'}, {1, 3, 'c'}, {1, 1, 'd'}, {1, 3, 'e'}}
        };
    }
}