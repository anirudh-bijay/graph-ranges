/**
 * @file tests/adjacency_list/directed_multigraph.cpp
 * 
 * @brief Unit tests for the directed adjacency list multigraph implementation.
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

    TEST(DirectedAdjacencyListMultigraph, InsertionMethods)
    {
        /// @section Set up a directed multigraph with initial vertices and edges.

        graph::graph <
            graph::adjacency_list_policy <
                int, std::string, std::pair <int, int>, std::unordered_map, graph::vector
            >{
                .is_directed = true,
                .is_multigraph = true,
                .forbid_self_loops = false,
                .support_inedge_iteration = false
            }
        > g{
            {{1, "A"}, {2, "B"}, {3, "C"}},
            {{1, 2, {1, 2}}, {2, 3, {2, 3}}}
        };

        // Check whether vertex and edge counts are correct.
        ASSERT_EQ(g.order(), 3);
        ASSERT_EQ(g.size(), 2);

        /// @section Test vertex insertion.

        auto v_iter = g.insert_vertex({4, "D"});
        ASSERT_EQ(std::get <0>(*v_iter), 4);
        EXPECT_EQ(std::get <1>(*v_iter), "D");
        ASSERT_EQ(g.order(), 4);
        ASSERT_EQ(g.size(), 2);

        EXPECT_TRUE(g.outedges(v_iter).empty());

        v_iter = g.emplace_vertex(5, "E");
        ASSERT_EQ(std::get <0>(*v_iter), 5);
        EXPECT_EQ(std::get <1>(*v_iter), "E");
        ASSERT_EQ(g.order(), 5);
        ASSERT_EQ(g.size(), 2);

        EXPECT_TRUE(g.outedges(v_iter).empty());

        auto v = std::make_pair(6, "F");
        v_iter = g.insert_vertex(v);
        ASSERT_EQ(std::get <0>(*v_iter), 6);
        EXPECT_EQ(std::get <1>(*v_iter), "F");
        ASSERT_EQ(g.order(), 6);
        ASSERT_EQ(g.size(), 2);

        EXPECT_TRUE(g.outedges(v_iter).empty());

        /// @section Test edge insertion.

        auto e = std::make_tuple(1, 2, std::make_pair(1, 2));
        g.insert_edge({1, 2, {1, 2}});
        g.insert_edge({2, 3, {2, 3}});
        auto e_iter = g.insert_edge({1, 3, {1, 3}});

        EXPECT_EQ(g.order(), 6);
        EXPECT_EQ(g.size(), 5);

        ASSERT_EQ(*e_iter, std::make_tuple(1, 3, std::make_tuple(1, 3)));

        /// @section Test self-loop insertion.

        e_iter = g.insert_edge(v_iter, v_iter, {6, 6});

        EXPECT_EQ(g.order(), 6);
        EXPECT_EQ(g.size(), 6);

        ASSERT_EQ(*e_iter, std::make_tuple(6, 6, std::make_tuple(6, 6)));

        /// @section Test edge insertion with emplace.

        e_iter = g.emplace_edge(4, 5, std::pair{4, 5});

        EXPECT_EQ(g.order(), 6);
        EXPECT_EQ(g.size(), 7);

        ASSERT_EQ(*e_iter, std::make_tuple(4, 5, std::make_pair(4, 5)));
    }
}