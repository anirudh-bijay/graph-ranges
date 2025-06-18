/**
 * @file graph.h
 * 
 * Graph implementations.
 */

#pragma once

#include "graph/detail/adjacency_list/directed.h"

namespace graph
{
    template <auto Policy>
    class graph;

    template <auto Policy>
    class directed_graph;

    template <auto Policy>
    class undirected_graph;
    
} // namespace graph
