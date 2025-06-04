/**
 * @file graph/graph_traits.h
 * 
 * Declaration of templated struct @c `graph_traits` for
 * specialisation by classes implementing graphs.
 */

#pragma once

namespace graph
{
    /// @brief Type traits for graphs.
    /// 
    ///        Eligible graph classes can specialise this template
    ///        appropriately.
    /// 
    /// @tparam G A graph type.
    template <class G>
    struct graph_traits;

} // namespace graph
