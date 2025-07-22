/**
 * @file graph/detail/adjacency_list/directed.h
 * 
 * Generic adjacency list implementation of digraphs for fast lookup
 * of nodes and iteration over neighbours/successors/outedges at the
 * cost of slower edge lookup.
 */

#pragma once

#include "graph/adjacency_list_policy.h"
#include "graph/graph_traits.h"
#include "graph/detail/utils.h"

#include <algorithm>
#include <concepts>
#include <initializer_list>
#include <iterator>
#include <limits>
#include <ranges>
#include <stdexcept>
#include <tuple>
#include <type_traits>
#include <utility>

#if __cpp_lib_ranges < 202211L
#warning This header requires support for C++23 <ranges>.
#endif

namespace graph
{
    /// @brief Forward declaration of the graph classes.

    template <auto Policy>
    class graph;
    
} // namespace graph

namespace graph::detail::adjacency_list
{
    /// @brief Forward declaration of the vertex and edge views.

    template <auto Policy, std::ranges::view V>
    class vertex_view;

    // // Deduction guide to convert ranges to views (as if by `std::views::all`).
    // template <class Graph, std::ranges::viewable_range R>
    // vertex_view(Graph, R&&) -> vertex_view <Graph::policy, detail::views::all_t <R> >;

    template <auto Policy, std::input_iterator Iter>
    class outedge_view;

    template <auto Policy, std::input_iterator Iter>
    class inedge_view;

} // namespace graph::detail::adjacency_list

#pragma region      Implementation of (standard) directed adjacency list graph without inedge iteration

template <
    typename vId,
    typename vTp,
    typename eTp,
    template <typename Key, typename T> class vCont,
    template <typename T> class eCont,
    graph::adjacency_list_policy <vId, vTp, eTp, vCont, eCont> Policy
> requires (Policy.is_directed && Policy.is_multigraph && !Policy.support_inedge_iteration)
class graph::graph <Policy>
{
    using Class = graph;                       // The class.

    friend struct ::graph::graph_traits <Class>;

private:
    using row_type      = eCont <std::pair <vId, eTp> >;
    using table_type    = vCont <vId, std::pair <vTp, row_type> >;

public:
    using policy_t = decltype(Policy);
    inline static constexpr policy_t policy = Policy;

public:
    using size_type = std::conditional_t <
        std::numeric_limits <std::ranges::range_size_t <table_type> >::max()
            < std::numeric_limits <std::ranges::range_size_t <row_type> >::max(),
        std::ranges::range_size_t <table_type>,
        std::ranges::range_size_t <row_type>
    >;

    using difference_type = std::conditional_t <
        std::numeric_limits <std::ranges::range_difference_t <table_type> >::max()
            < std::numeric_limits <std::ranges::range_difference_t <row_type> >::max(),
        std::ranges::range_difference_t<table_type>,
        std::ranges::range_difference_t <row_type>
    >;

protected:
    table_type adj{};
    size_type edge_count = {};

private:
    // template <auto Policy_, std::ranges::viewable_range R>
    // using vertex_view = detail::adjacency_list::vertex_view <Policy_, R>;

    static constexpr auto vertex_view(auto&& range)
    {
        return detail::adjacency_list::vertex_view <
            Policy,
            detail::views::all_t <decltype(range)>
        >(std::forward <decltype(range)>(range));
    }

    static constexpr auto outedge_view(auto iter)
    {
        return detail::adjacency_list::outedge_view <Policy, decltype(iter)>(iter);
    }

    // template <std::input_iterator Iter>
    // using outedge_view = detail::adjacency_list::outedge_view <Policy, Iter>;

    using vertex_view_iterator_t = std::ranges::iterator_t <decltype(vertex_view(adj))>;
    using vertex_view_const_iterator_t = std::ranges::iterator_t <const decltype(vertex_view(adj))>;

    using outedge_view_iterator_t = std::ranges::iterator_t <detail::adjacency_list::outedge_view <Policy, std::ranges::iterator_t <table_type> > >;
    using outedge_view_const_iterator_t = std::ranges::iterator_t <const detail::adjacency_list::outedge_view <Policy, std::ranges::iterator_t <table_type> > >;

public:
    /// @section Constructors

    constexpr graph() = default;

    constexpr graph(
        std::initializer_list <std::tuple <vId, vTp> > v_set,
        std::initializer_list <std::tuple <vId, vId, eTp> > e_set = {}
    ) : graph(v_set | std::views::all, e_set | std::views::all) {}
    
    template <
        std::ranges::input_range VertexRange,
        std::ranges::input_range EdgeRange = std::ranges::empty_view <std::tuple <vId, vId, eTp> >
    >
        requires std::convertible_to <std::ranges::range_reference_t <VertexRange>, std::tuple <vId, vTp> >
        && std::convertible_to <std::ranges::range_reference_t <EdgeRange>, std::tuple <vId, vId, eTp> >
    constexpr graph(VertexRange&& v_range, EdgeRange&& e_range = {})
    {
        for (const auto& elem : v_range) {
            emplace_vertex(std::get <0>(elem), std::get <1>(elem));
        }
        for (const auto& elem : e_range) {
            emplace_edge(std::get <0>(elem), std::get <1>(elem), std::get <2>(elem));
        }
    }

    /// @section Vertex insertion and erasure

    constexpr vertex_view_iterator_t
    insert_vertex(const std::tuple <const vId, vTp>& v_elem)
    {
        auto&& [v_key, v_data] = v_elem;
        return vertex_view_iterator_t{std::get <0>(adj.emplace(v_key, std::pair <vTp, row_type>{v_data, {}}))};
    }

    constexpr vertex_view_iterator_t
    insert_vertex(std::tuple <const vId, vTp>&& v_elem)
    {
        auto&& [v_key, v_data] = std::move(v_elem);
        return vertex_view_iterator_t{std::get <0>(adj.emplace(std::move(v_key), std::pair <vTp, row_type>{std::move(v_data), {}}))};
    }

    template <class... Args>
    constexpr vertex_view_iterator_t
    emplace_vertex(Args&&... args)
    {
        return insert_vertex(std::tuple <const vId, vTp>(std::forward <Args>(args)...));
    }

    constexpr void erase_vertex(const vId& v_key)
    {
        erase_vertex(find_vertex(v_key));
    }

    constexpr void erase_vertex(const vertex_view_iterator_t v_iter)
    {
        const auto v_key = std::get <0>(*v_iter);
        
        edge_count -= std::ranges::size(outedges(v_iter));
        adj.erase(v_iter.base());
        
        auto vertex_set = vertices();

        for (auto it = std::ranges::begin(vertex_set); it != std::ranges::end(vertex_set); ++it) {
            /// @note @c `std::erase_if` should be found by ADL;
            ///       hence, @c `erase_if` is used unqualified.
            edge_count -= erase_if(std::get <1>(std::get <1>(*it.base())), [&v_key](const auto& elem) {
                return std::get <0>(elem) == v_key;
            });
        }
    }

    /// @section Edge insertion and erasure

    constexpr auto
    insert_edge(const std::tuple <const vId, const vId, eTp>& e_elem)
    {
        auto&& [tail_key, head_key, e_data] = e_elem;

        if constexpr (Policy.forbid_self_loops)
            if (tail_key == head_key) [[unlikely]]
                throw std::invalid_argument("Self-loops are not allowed in this graph");

        const auto row_iter = adj.find(tail_key);

        if (row_iter == std::ranges::end(adj)) [[unlikely]]
            throw std::out_of_range("The vertex key for the tail does not exist in the graph");

        const auto it = outedge_view_iterator_t{row_iter, std::get <1>(std::get <1>(*row_iter)).emplace(head_key, e_data)};
        ++edge_count; // Increment the edge count. Done after insertion for strong exception guarantee.
        return it;
    }

    constexpr auto
    insert_edge(std::tuple <const vId, const vId, eTp>&& e_elem)
    {
        auto&& [tail_key, head_key, e_data] = std::move(e_elem);

        if constexpr (Policy.forbid_self_loops)
            if (tail_key == head_key) [[unlikely]]
                throw std::invalid_argument("Self-loops are not allowed in this graph");

        const auto row_iter = adj.find(tail_key);   /** TODO: Change to `find_vertex` */

        if (row_iter == std::ranges::end(adj)) [[unlikely]]
            throw std::out_of_range("The vertex key for the tail does not exist in the graph");

        const auto it = outedge_view_iterator_t{row_iter, std::get <1>(std::get <1>(*row_iter)).emplace(head_key, std::move(e_data))};
        ++edge_count; // Increment the edge count. Done after insertion for strong exception guarantee.
        return it;
    }

    template <class... Args>
    constexpr auto
    emplace_edge(Args&&... args)
    {
        return insert_edge(std::tuple <const vId, const vId, eTp>(std::forward <Args>(args)...));
    }

    constexpr auto insert_edge(
        const vertex_view_iterator_t tail_iter,
        vertex_view_iterator_t head_iter,
        const eTp& e_data
    )
    {
        const auto it = outedge_view_iterator_t{
            tail_iter.base(),
            std::get <1>(std::get <1>(*tail_iter.base())).emplace(std::get <0>(*std::move_if_noexcept(head_iter)), e_data)
        };
        ++edge_count; // Increment the edge count. Done after insertion for strong exception guarantee.
        return it;
    }

    constexpr auto insert_edge(
        const vertex_view_iterator_t tail_iter,
        vertex_view_iterator_t head_iter,
        eTp&& e_data
    )
    {
        const auto it = outedge_view_iterator_t{
            tail_iter,
            std::get <1>(std::get <1>(*tail_iter.base())).emplace(std::get <0>(*std::move_if_noexcept(head_iter)), std::move(e_data))
        };
        ++edge_count; // Increment the edge count. Done after insertion for strong exception guarantee.
        return it;
    }

    constexpr void erase_edge(outedge_view_iterator_t e_iter)
    {
        /** REQUIREMENT:
         *      @c e_iter.base() gives an @c std::pair with
         *      @c .first being the iterator to the tail row and
         *      @c .second being the iterator to the element in the
         *      row of the outedge list of the tail.
         */
        
        const auto& row_iter = e_iter.base().first;

        std::get <1>(std::get <1>(*row_iter)).erase(/* Access edge iter */ e_iter.base().second);
        --edge_count; // Decrement the edge count. Done after erasure for strong exception guarantee.
    }

    constexpr size_type erase_edges(const vId& tail_key, const vId& head_key)
    {
        /** REQUIREMENT: Same as above */

        if constexpr (Policy.forbid_self_loops)
            if (tail_key == head_key) [[unlikely]]
                return 0; // No self-loops allowed, so no edges deleted.

        /// @note @c `std::erase_if` should be found by ADL;
        ///       hence, @c `erase_if` is used unqualified.
        const auto change = erase_if(
            std::get <1>(std::get <1>(*adj.find(tail_key))),
            [&head_key](const auto& elem) -> bool {
                return elem.first == head_key;
            }
        );

        edge_count -= change; // Decrement the edge count.
        return change;
    }

    constexpr size_type erase_edges(
        vertex_view_iterator_t tail_iter,
        const vertex_view_iterator_t head_iter
    )
    {
        /** REQUIREMENT: Same as above */

        if constexpr (Policy.forbid_self_loops)
            if (std::get <0>(*tail_iter) == std::get <0>(*head_iter)) [[unlikely]]
                return 0; // No self-loops allowed, so no edges deleted.
        
        const auto& head_key = std::get <0>(*head_iter);

        /// @note @c `std::erase_if` should be found by ADL;
        ///       hence, @c `erase_if` is used unqualified.
        const auto change = erase_if(
            std::get <0>(std::get <0>(std::move_if_noexcept(tail_iter).base())),
            [&head_key](const auto& elem) -> bool {
                return elem.first == head_key;
            }
        );

        edge_count -= change; // Decrement the edge count.
        return change;
    }

    /// @section Vertex and edge views

    [[nodiscard]]
    constexpr std::ranges::range auto vertices() &
    {
        return vertex_view(adj);
    }

    [[nodiscard]]
    constexpr std::ranges::constant_range auto
    vertices() const&
    {
        return std::views::as_const(vertex_view(adj));
    }

    [[nodiscard]]
    constexpr std::ranges::range auto vertices() &&
    {
        return vertex_view(std::move(adj));
    }

    [[nodiscard]]
    constexpr std::ranges::constant_range auto
    vertices() const&&
    {
        return std::views::as_const(vertex_view(std::move(adj)));
    }

    [[nodiscard]]
    constexpr std::ranges::range auto
    outedges(const vertex_view_iterator_t tail_iter) noexcept(noexcept(outedge_view(tail_iter.base())))
    {
        const auto& row_iter = tail_iter.base();
        return outedge_view(row_iter);
    }

    [[nodiscard]]
    constexpr std::ranges::constant_range auto
    outedges(const vertex_view_const_iterator_t tail_iter) const noexcept(noexcept(std::views::as_const(outedge_view(tail_iter.base()))))
    {
        const auto& row_iter = tail_iter.base();
        return std::views::as_const(outedge_view(row_iter));
    }

    [[nodiscard]]
    constexpr std::ranges::range auto
    outedges(const vId& tail_key)
    {
        const auto row_iter = adj.find(tail_key);

        if (row_iter == std::ranges::end(adj)) [[unlikely]]
            throw std::out_of_range("The vertex key for the tail does not exist in the graph");

        return outedges(vertex_view_iterator_t(row_iter));
    }
    
    [[nodiscard]]
    constexpr std::ranges::constant_range auto
    outedges(const vId& tail_key) const
    {
        const auto row_iter = adj.find(tail_key);

        if (row_iter == std::ranges::end(adj)) [[unlikely]]
            throw std::out_of_range("The vertex key for the tail does not exist in the graph");

        return outedges(vertex_view_const_iterator_t(row_iter));
    }

    [[nodiscard]]
    constexpr std::ranges::view auto
    successors(vertex_view_iterator_t node_iter)
    {
        return std::views::values(outedges(std::move_if_noexcept(node_iter)));
    }

    [[nodiscard]]
    constexpr std::ranges::view auto
    successors(vertex_view_iterator_t node_iter) const
    {
        return std::views::values(outedges(std::move_if_noexcept(node_iter)));
    }

    [[nodiscard]]
    constexpr std::ranges::view auto
    successors(const vId& node_key)
    {
        return std::views::values(outedges(node_key));
    }

    [[nodiscard]]
    constexpr std::ranges::view auto
    successors(const vId& node_key) const
    {
        return std::views::values(outedges(node_key));
    }

    /// @section Graph properties

    [[nodiscard]]
    constexpr bool empty() const noexcept(noexcept(std::ranges::empty(adj)))
    {
        return std::ranges::empty(adj);
    }

    /// @brief Return the number of @b vertices in the graph.
    /// 
    /// @note This function is not intended for
    ///       bounds checking or similar operations that use
    ///       range sizes. To determine the size of a range
    ///       being iterated over, use `std::ranges::size` on
    ///       the range itself.
    [[nodiscard]]
    constexpr size_type order() const noexcept(noexcept(std::ranges::size(adj)))
    {
        return std::ranges::size(adj);
    }

    /// @brief Return the number of @b edges in the graph.
    /// 
    /// @note This function is not intended for
    ///       bounds checking or similar operations that use
    ///       range sizes. To determine the size of a range
    ///       being iterated over, use `std::ranges::size` on
    ///       the range itself.
    /// 
    ///       The intention is to allow the user or algorithms
    ///       to draw quick conclusions based on the number of
    ///       edges in the graph, such as whether the graph is
    ///       disconnected, cyclic, or sparse.
    [[nodiscard]]
    constexpr size_type size() const noexcept
    {
        return edge_count;
    }

    /// @section Vertex lookup

    [[nodiscard]]
    constexpr auto find_vertex(const vId& node_key)
    {
        return vertex_view_iterator_t{adj.find(node_key)};
    }

    [[nodiscard]]
    constexpr auto find_vertex(const vId& node_key) const
    {
        return vertex_view_const_iterator_t{adj.find(node_key)};
    }
};

#pragma region      Vertex and edge views

template <
    typename vId,
    typename vTp,
    typename eTp,
    template <typename Key, typename T> class vCont,
    template <typename T> class eCont,
    graph::adjacency_list_policy <vId, vTp, eTp, vCont, eCont> Policy,
    std::ranges::view V
> requires (Policy.is_directed && Policy.is_multigraph && !Policy.support_inedge_iteration)
class graph::detail::adjacency_list::vertex_view <Policy, V>
    : public std::ranges::view_interface <vertex_view <Policy, V> >
{
    using Class = vertex_view;                          // The class.
    using Base  = std::ranges::view_interface <Class>;  // The base class.

protected:
    V view;

    template <bool Const> class iterator_impl;
    template <bool Const> class sentinel_impl;

public:
    constexpr vertex_view() requires std::default_initializable <V> = default;
    explicit constexpr vertex_view(V view) : view(std::move(view)) {}
    
    constexpr auto begin() requires (!detail::ranges::simple_view <V>)
    {
        return iterator_impl <false>{std::ranges::begin(view)};
    }

    constexpr auto begin() const requires std::ranges::range <const V>
    {
        return iterator_impl <true>{std::ranges::begin(view)};
    }

    constexpr auto end() requires (!detail::ranges::simple_view <V>)
    {
        if constexpr (std::ranges::common_range <V>)
            return iterator_impl <false>{std::ranges::end(view)};
        else
            return sentinel_impl <false>{std::ranges::end(view)};
    }

    constexpr auto end() const requires std::ranges::range <const V>
    {
        if constexpr (std::ranges::common_range <V>)
            return iterator_impl <true>{std::ranges::end(view)};
        else
            return sentinel_impl <true>{std::ranges::end(view)};
    }
};

template <
    typename vId,
    typename vTp,
    typename eTp,
    template <typename Key, typename T> class vCont,
    template <typename T> class eCont,
    graph::adjacency_list_policy <vId, vTp, eTp, vCont, eCont> Policy,
    std::ranges::view V
> requires (Policy.is_directed && Policy.is_multigraph && !Policy.support_inedge_iteration)
template <bool Const>
class graph::detail::adjacency_list::vertex_view <Policy, V>::iterator_impl
{
    using Class = iterator_impl;                        // The class.

    friend class ::graph::graph <Policy>;

    template <bool OtherConst>
    friend class detail::adjacency_list::vertex_view <Policy, V>::sentinel_impl;

private:
    using base_iterator_type = std::ranges::iterator_t <std::conditional_t <Const, const V, V> >;
    base_iterator_type base_iter;

    using base_return_type = base_iterator_type;

    /// @section Access to the underlying iterator

    [[nodiscard]]
    constexpr const base_return_type& base() const& noexcept
    {
        return base_iter;
    }

    [[nodiscard]]
    constexpr base_return_type&& base() && noexcept(noexcept(std::move_if_noexcept(base_iter)))
    {
        return std::move(base_iter);
    }

    [[nodiscard]]
    constexpr const base_return_type&& base() const&& noexcept(noexcept(std::move_if_noexcept(base_iter)))
    {
        return std::move(base_iter);
    }

public:
    using difference_type   = std::iter_difference_t <base_iterator_type>;
    using value_type        = std::tuple <const vId, std::tuple_element_t <0, std::tuple_element_t <1, std::ranges::range_value_t <V> > > >;
    using reference         = std::tuple <const vId&, decltype(std::get <0>(std::get <1>(std::declval <std::ranges::range_reference_t <std::conditional_t <Const, const V, V> > >())))>;
    using const_reference   = std::tuple <const vId&, const vTp&>;
    using iterator_category = std::iterator_traits <base_iterator_type>::iterator_category;
    using iterator_concept  = detail::iterator_concept <base_iterator_type>;

    class [[nodiscard]] pointer
    {        
    private:
        reference ref;

    public:
        explicit constexpr pointer(reference ref) noexcept : ref{std::move(ref)} {}

        constexpr reference operator*() const noexcept
        {
            return ref;
        }

        constexpr const reference* operator->() const noexcept
        {
            return std::addressof(ref);
        }
    };

public:
    /// @section Constructors

    /// @brief Default constructor to satisfy the @c `sentinel_for <iterator>`
    ///        concept. This concept is used by @c `std::ranges::end`
    ///        to ensure that a sentinel can be constructed and
    ///        compared against an iterator.
    /// 
    ///        Also a part of iterator requirements for iterators satisfying
    ///        the @c `std::forward_iterator` concept.
    explicit constexpr iterator_impl() noexcept = default;

    explicit constexpr iterator_impl(base_iterator_type base_iter) noexcept(noexcept(base_iterator_type{std::move_if_noexcept(base_iter)}))
        : base_iter{std::move_if_noexcept(base_iter)} {}

    /// @section Iterator operations

    [[nodiscard]]
    constexpr reference operator*() const
    {
        return reference{std::get <0>(*base_iter), std::get <0>(std::get <1>(*base_iter))};
    }

    /// @brief 
    /// @return 
    /// @warning Unnecessary use.
    constexpr pointer operator->() const
    {
        return pointer{**this};
    }

    constexpr Class& operator++()
        requires std::incrementable <base_iterator_type>
    {
        ++base_iter;
        return *this;
    }

    constexpr Class operator++(int)
        requires std::incrementable <base_iterator_type>
    {
        const auto copy = *this;
        ++*this;
        return copy;
    }

    constexpr Class& operator--()
        requires std::bidirectional_iterator <base_iterator_type>
    {
        --base_iter;
        return *this;
    }

    constexpr Class operator--(int)
        requires std::bidirectional_iterator <base_iterator_type>
    {
        const auto copy = *this;
        --*this;
        return copy;
    }

    constexpr Class& operator+=(difference_type n)
        requires std::random_access_iterator <base_iterator_type>
    {
        base_iter += std::move(n);
        return *this;
    }

    [[nodiscard]]
    constexpr Class operator+(difference_type n) const
        requires std::random_access_iterator <base_iterator_type>
    {
        auto copy = *this;
        return copy += std::move(n);
    }

    constexpr Class& operator-=(difference_type n)
        requires std::random_access_iterator <base_iterator_type>
    {
        base_iter -= std::move(n);
        return *this;
    }

    [[nodiscard]]
    constexpr Class operator-(difference_type n) const
        requires std::random_access_iterator <base_iterator_type>
    {
        auto copy = *this;
        return copy -= std::move(n);
    }
    
    [[nodiscard]]
    friend constexpr difference_type operator-(const Class& lhs, const Class& rhs)
        requires std::random_access_iterator <base_iterator_type>
    {
        return lhs.base() - rhs.base();
    }

    /// @section Comparison operators

    [[nodiscard]]
    friend constexpr bool operator==(const Class& lhs, const Class& rhs)
        requires std::equality_comparable <base_iterator_type> = default;

    [[nodiscard]]
    friend constexpr auto operator<=>(const Class& lhs, const Class& rhs)
        requires std::three_way_comparable <base_iterator_type> = default;

    /// @section Conversion operators

    constexpr operator iterator_impl <true>() const noexcept requires (!Const && std::ranges::range <const V>)
    {
        return iterator_impl <true>(base());
    }

    template <std::constructible_from <base_return_type> T>
        requires (!std::same_as <T, iterator_impl <true> >)
    constexpr operator T() const noexcept(std::is_nothrow_constructible_v <T, base_return_type>)
    {
        return T{base()};
    }
};

template <
    typename vId,
    typename vTp,
    typename eTp,
    template <typename Key, typename T> class vCont,
    template <typename T> class eCont,
    graph::adjacency_list_policy <vId, vTp, eTp, vCont, eCont> Policy,
    std::ranges::view V
> requires (Policy.is_directed && Policy.is_multigraph && !Policy.support_inedge_iteration)
template <bool Const>
class graph::detail::adjacency_list::vertex_view <Policy, V>::sentinel_impl
{
    using Class = sentinel_impl;                         // The class.

    friend class ::graph::graph <Policy>;

private:
    using base_sentinel_type = std::ranges::sentinel_t <std::conditional_t <Const, const V, V> >;
    base_sentinel_type base_sent;

    using base_return_type = base_sentinel_type;

    /// @section Access to the underlying iterator

    [[nodiscard]]
    constexpr const base_return_type& base() const& noexcept
    {
        return base_sent;
    }

    [[nodiscard]]
    constexpr base_return_type&& base() && noexcept(noexcept(std::move_if_noexcept(base_sent)))
    {
        return std::move_if_noexcept(base_sent);
    }

    [[nodiscard]]
    constexpr const base_return_type&& base() const&& noexcept(noexcept(std::move_if_noexcept(base_sent)))
    {
        return std::move_if_noexcept(base_sent);
    }

public:
    /// @section Constructors

    /// @brief Default constructor to satisfy the @c `sentinel_for <iterator>`
    ///        concept. This concept is used by @c `std::ranges::end`
    ///        to ensure that a sentinel can be constructed and
    ///        compared against an iterator.
    constexpr sentinel_impl() noexcept = default;

    explicit constexpr sentinel_impl(base_sentinel_type base_sent) noexcept(noexcept(base_sentinel_type{std::move_if_noexcept(base_sent)}))
        : base_sent{std::move_if_noexcept(base_sent)} {}
    
    /// @section Difference and comparison operations

    template <bool OtherConst> [[nodiscard]]
    friend constexpr std::ranges::range_difference_t <std::conditional_t <OtherConst, const V, V> >
    operator-(const iterator_impl <OtherConst>& lhs, const Class& rhs)
        requires std::sized_sentinel_for <base_sentinel_type, std::ranges::iterator_t <std::conditional_t <OtherConst, const V, V> > >
    {
        return lhs.base() - rhs.base();
    }

    template <bool OtherConst> [[nodiscard]]
    friend constexpr std::ranges::range_difference_t <std::conditional_t <OtherConst, const V, V> >
    operator-(const Class& lhs, const iterator_impl <OtherConst>& rhs)
        requires std::sized_sentinel_for <base_sentinel_type, std::ranges::iterator_t <std::conditional_t <OtherConst, const V, V> > >
    {
        return lhs.base() - rhs.base();
    }

    template <bool OtherConst> [[nodiscard]]
    friend constexpr bool operator==(const iterator_impl <OtherConst>& lhs, const Class& rhs)
        requires std::sentinel_for <base_sentinel_type, std::ranges::iterator_t <std::conditional_t <OtherConst, const V, V> > >
    {
        return lhs.base() == rhs.base();
    }

    template <bool OtherConst> [[nodiscard]]
    friend constexpr bool operator==(const Class& lhs, const iterator_impl <OtherConst>& rhs)
        requires std::sentinel_for <base_sentinel_type, std::ranges::iterator_t <std::conditional_t <OtherConst, const V, V> > >
    {
        return lhs.base() == rhs.base();
    }

    /// @section Conversion operators

    constexpr operator sentinel_impl <true>() const requires (!Const && std::ranges::range <const V>)
    {
        return sentinel_impl <true>(base());
    }
};

template <
    typename vId,
    typename vTp,
    typename eTp,
    template <typename Key, typename T> class vCont,
    template <typename T> class eCont,
    graph::adjacency_list_policy <vId, vTp, eTp, vCont, eCont> Policy,
    std::input_iterator Iter
> requires (Policy.is_directed && Policy.is_multigraph && !Policy.support_inedge_iteration)
class graph::detail::adjacency_list::outedge_view <Policy, Iter>
    : public std::ranges::view_interface <outedge_view <Policy, Iter> >
{
    using Class = outedge_view;                          // The class.
    using Base  = std::ranges::view_interface <Class>;   // The base class.

protected:
    using V = detail::views::all_t <decltype(std::get <1>(std::get <1>(*std::declval <Iter>())))>;

    Iter tail_iter;
    V children_view;

    template <bool Const> class iterator_impl;
    template <bool Const> class sentinel_impl;

public:
    constexpr outedge_view() requires std::default_initializable <V> = default;
    explicit constexpr outedge_view(Iter tail_iter)
        : tail_iter{tail_iter}, children_view{std::get <1>(std::get <1>(*tail_iter))} {}

    constexpr auto begin() requires (!detail::ranges::simple_view <V>)
    {
        return iterator_impl <false>{tail_iter, std::ranges::begin(children_view)};
    }

    constexpr auto begin() const requires std::ranges::range <const V>
    {
        return iterator_impl <true>{tail_iter, std::ranges::begin(children_view)};
    }

    constexpr auto end() requires (!detail::ranges::simple_view <V>)
    {
        return iterator_impl <false>{tail_iter, std::ranges::end(children_view)};
    }

    constexpr auto end() const requires std::ranges::range <const V>
    {
        return iterator_impl <true>{tail_iter, std::ranges::end(children_view)};
    }
};

template <
    typename vId,
    typename vTp,
    typename eTp,
    template <typename Key, typename T> class vCont,
    template <typename T> class eCont,
    graph::adjacency_list_policy <vId, vTp, eTp, vCont, eCont> Policy,
    std::input_iterator Iter
> requires (Policy.is_directed && Policy.is_multigraph && !Policy.support_inedge_iteration)
template <bool Const>
class graph::detail::adjacency_list::outedge_view <Policy, Iter>::iterator_impl
{
    using Class = iterator_impl;                        // The class.

    friend class ::graph::graph <Policy>;

    template <bool OtherConst>
    friend class detail::adjacency_list::outedge_view <Policy, Iter>::sentinel_impl;

private:
    using V = detail::views::all_t <decltype(std::get <1>(std::get <1>(*std::declval <Iter>())))>;

    using tail_iterator_type = Iter;
    using base_iterator_type = std::ranges::iterator_t <std::conditional_t <Const, const V, V> >;
    using base_return_type = std::pair <tail_iterator_type, base_iterator_type>;

    base_return_type base_iter;

    /// @section Access to the underlying iterator

    [[nodiscard]]
    constexpr const base_return_type& base() const& noexcept
    {
        return base_iter;
    }

    [[nodiscard]]
    constexpr base_return_type&& base() && noexcept(noexcept(std::move_if_noexcept(base_iter)))
    {
        return std::move_if_noexcept(base_iter);
    }

    [[nodiscard]]
    constexpr const base_return_type&& base() const&& noexcept(noexcept(std::move_if_noexcept(base_iter)))
    {
        return std::move_if_noexcept(base_iter);
    }

public:
    using difference_type   = std::iter_difference_t <base_iterator_type>;
    using value_type        = std::tuple <const vId, const vId, std::tuple_element_t <1, std::ranges::range_value_t <V> > >;
    using reference         = std::tuple <const vId&, const vId&, decltype(std::get <1>(std::declval <std::ranges::range_reference_t <std::conditional_t <Const, const V, V> > >()))>;
    using const_reference   = std::tuple <const vId&, const vId&, const eTp&>;
    using iterator_category = std::iterator_traits <base_iterator_type>::iterator_category;
    using iterator_concept  = detail::iterator_concept <base_iterator_type>;

    class [[nodiscard]] pointer
    {       
    private:
        reference ref;

    public:
        explicit constexpr pointer(reference ref) : ref{std::move(ref)} {}

        constexpr reference operator*() const noexcept
        {
            return ref;
        }

        constexpr const reference* operator->() const noexcept
        {
            return std::addressof(ref);
        }
    };

public:
    /// @section Constructors

    /// @brief Default constructor to satisfy the @c `sentinel_for <iterator>`
    ///        concept. This concept is used by @c `std::ranges::end`
    ///        to ensure that a sentinel can be constructed and
    ///        compared against an iterator.
    /// 
    ///        Also a part of iterator requirements for iterators satisfying
    ///        the @c `std::forward_iterator` concept.
    explicit constexpr iterator_impl() noexcept = default;

    explicit constexpr iterator_impl(tail_iterator_type tail_iter, base_iterator_type base_iter)
        : base_iter{std::move_if_noexcept(tail_iter), std::move_if_noexcept(base_iter)} {}

    /// @section Iterator operations

    [[nodiscard]]
    constexpr reference operator*() const
    {
        const auto& tail_iter = base_iter.first;
        const auto& outedge_iter = base_iter.second;

        return reference{std::get <0>(*tail_iter), std::get <0>(*outedge_iter), std::get <1>(*outedge_iter)};
    }

    /// @brief 
    /// @return 
    /// @warning Unnecessary use.
    constexpr pointer operator->() const
    {
        return pointer{**this};
    }

    constexpr Class& operator++()
        requires std::incrementable <base_iterator_type>
    {
        ++base_iter.second;
        return *this;
    }

    constexpr Class operator++(int)
        requires std::incrementable <base_iterator_type>
    {
        const auto copy = *this;
        ++*this;
        return copy;
    }

    constexpr Class& operator--()
        requires std::bidirectional_iterator <base_iterator_type>
    {
        --base_iter.second;
        return *this;
    }

    constexpr Class operator--(int)
        requires std::bidirectional_iterator <base_iterator_type>
    {
        const auto copy = *this;
        --*this;
        return copy;
    }

    constexpr Class& operator+=(difference_type n)
        requires std::random_access_iterator <base_iterator_type>
    {
        base_iter.second += std::move(n);
        return *this;
    }

    [[nodiscard]]
    constexpr Class operator+(difference_type n) const
        requires std::random_access_iterator <base_iterator_type>
    {
        auto copy = *this;
        return copy += std::move(n);
    }

    constexpr Class& operator-=(difference_type n)
        requires std::random_access_iterator <base_iterator_type>
    {
        base_iter.second -= std::move(n);
        return *this;
    }

    [[nodiscard]]
    constexpr Class operator-(difference_type n) const
        requires std::random_access_iterator <base_iterator_type>
    {
        auto copy = *this;
        return copy -= std::move(n);
    }
    
    [[nodiscard]]
    friend constexpr difference_type operator-(const Class& lhs, const Class& rhs)
        requires std::random_access_iterator <base_iterator_type>
    {
        return lhs.base().second - rhs.base().second;
    }

    /// @section Comparison operators

    [[nodiscard]]
    friend constexpr bool operator==(const Class& lhs, const Class& rhs)
        requires std::equality_comparable <base_return_type> = default;

    [[nodiscard]]
    friend constexpr auto operator<=>(const Class& lhs, const Class& rhs)
        requires std::three_way_comparable <base_iterator_type>
    {
        if constexpr (std::three_way_comparable <base_return_type>) {
            return lhs.base() <=> rhs.base();
        } else {
            return lhs.base().first != rhs.base().first
                ? std::partial_ordering::unordered
                : lhs.base().second <=> rhs.base().second;
        }
    };
    
    /// @section Conversion operators

    constexpr operator iterator_impl <true>() const noexcept requires (!Const && std::ranges::range <const V>)
    {
        return iterator_impl <true>(base_iter.first, base_iter.second);
    }

    template <std::constructible_from <base_return_type> T>
        requires (!std::same_as <T, iterator_impl <true> >)
    constexpr operator T() const noexcept(std::is_nothrow_constructible_v <T, base_return_type>)
    {
        return T{base_iter};
    }
};

template <
    typename vId,
    typename vTp,
    typename eTp,
    template <typename Key, typename T> class vCont,
    template <typename T> class eCont,
    graph::adjacency_list_policy <vId, vTp, eTp, vCont, eCont> Policy,
    std::input_iterator Iter
> requires (Policy.is_directed && Policy.is_multigraph && !Policy.support_inedge_iteration)
template <bool Const>
class graph::detail::adjacency_list::outedge_view <Policy, Iter>::sentinel_impl
{
    using Class = sentinel_impl;                         // The class.

    friend class ::graph::graph <Policy>;

private:
    using V = detail::views::all_t <decltype(std::get <1>(std::get <1>(*std::declval <Iter>())))>;

    using tail_iterator_type = Iter;
    using base_sentinel_type = std::ranges::sentinel_t <std::conditional_t <Const, const V, V> >;
    using base_return_type = std::pair <tail_iterator_type, base_sentinel_type>;

    base_return_type base_sent;

    /// @section Access to the underlying iterator

    [[nodiscard]]
    constexpr const base_return_type& base() const& noexcept
    {
        return base_sent;
    }

    [[nodiscard]]
    constexpr base_return_type&& base() && noexcept(noexcept(std::move_if_noexcept(base_sent)))
    {
        return std::move_if_noexcept(base_sent);
    }

    [[nodiscard]]
    constexpr const base_return_type&& base() const&& noexcept(noexcept(std::move_if_noexcept(base_sent)))
    {
        return std::move_if_noexcept(base_sent);
    }

public:
    /// @section Constructors

    /// @brief Default constructor to satisfy the @c `sentinel_for <iterator>`
    ///        concept. This concept is used by @c `std::ranges::end`
    ///        to ensure that a sentinel can be constructed and
    ///        compared against an iterator.
    constexpr sentinel_impl() noexcept = default;

    explicit constexpr sentinel_impl(tail_iterator_type tail_iter, base_sentinel_type base_sent)
        : base_sent{std::move_if_noexcept(tail_iter), std::move_if_noexcept(base_sent)} {}
    
    template <bool OtherConst> [[nodiscard]]
    friend constexpr std::ranges::range_difference_t <std::conditional_t <OtherConst, const V, V> >
    operator-(const iterator_impl <OtherConst>& lhs, const Class& rhs) 
        requires std::sized_sentinel_for <base_sentinel_type, std::ranges::iterator_t <std::conditional_t <OtherConst, const V, V> > >
    {
        return lhs.base().second - rhs.base().second;
    }

    template <bool OtherConst> [[nodiscard]]
    friend constexpr std::ranges::range_difference_t <std::conditional_t <OtherConst, const V, V> >
    operator-(const Class& lhs, const iterator_impl <OtherConst>& rhs)
        requires std::sized_sentinel_for <base_sentinel_type, std::ranges::iterator_t <std::conditional_t <OtherConst, const V, V> > >
    {
        return lhs.base().second - rhs.base().second;
    }

    template <bool OtherConst> [[nodiscard]] 
    friend constexpr bool operator==(const iterator_impl <OtherConst>& lhs, const Class& rhs)
        requires std::sentinel_for <base_sentinel_type, std::ranges::iterator_t <std::conditional_t <OtherConst, const V, V> > >
    {
        return lhs.base() == rhs.base();
    }

    template <bool OtherConst> [[nodiscard]]
    friend constexpr bool operator==(const Class& lhs, const iterator_impl <OtherConst>& rhs)
        requires std::sentinel_for <base_sentinel_type, std::ranges::iterator_t <std::conditional_t <OtherConst, const V, V> > >
    {
        return lhs.base() == rhs.base();
    }

    constexpr operator sentinel_impl <true>() const noexcept requires (!Const && std::ranges::range <const V>)
    {
        return sentinel_impl <true>(base_sent.first, base_sent.second);
    }
};

namespace std::ranges
{
    /// @brief Explicit partial specialisation for @c `::graph::detail::adjacency_list::vertex_view`.
    /// 
    ///        If the view @c `V` is the template argument for the view adapted by @c `vertex_view`,
    ///        then @c `std::ranges::enable_borrowed_range` is @c `true` for @c `vertex_view <V>`
    ///        iff @c `std::ranges::enable_borrowed_range <V>` is @c `true`.
    template <
        typename vId,
        typename vTp,
        typename eTp,
        template <typename Key, typename T> class vCont,
        template <typename T> class eCont,
        graph::adjacency_list_policy <vId, vTp, eTp, vCont, eCont> Policy,
        std::ranges::view V
    > requires (Policy.is_directed && !Policy.support_inedge_iteration)
    constexpr bool enable_borrowed_range <typename ::graph::detail::adjacency_list::vertex_view <Policy, V> >
        = std::ranges::borrowed_range <V>;

} // namespace std::ranges

#pragma endregion   Vertex and edge views

#pragma region      Graph traits

template <
    typename vId,
    typename vTp,
    typename eTp,
    template <typename Key, typename T> class vCont,
    template <typename T> class eCont,
    graph::adjacency_list_policy <vId, vTp, eTp, vCont, eCont> Policy
> requires (Policy.is_directed && !Policy.support_inedge_iteration)
struct graph::graph_traits <::graph::graph <Policy> >
{
    using policy_t = decltype(Policy);
    static constexpr policy_t policy = Policy;

    template <typename Key, typename T> using vertex_container = vCont <Key, T>;
    template <typename T> using edge_container = eCont <T>;

    using vertex_key_type       = vId;
    using vertex_mapped_type    = vTp;
    using vertex_value_type     = std::tuple <const vId, vTp>;
    using edge_type             = eTp;

    using vertex_iterator           = ::graph::graph <Policy>::vertex_view_iterator_t;
    using const_vertex_iterator     = ::graph::graph <Policy>::vertex_view_const_iterator_t;
    using outedge_iterator          = ::graph::graph <Policy>::outedge_view_iterator_t;
    using const_outedge_iterator    = ::graph::graph <Policy>::outedge_view_const_iterator_t;

    using size_type         = ::graph::graph <Policy>::size_type;
    using difference_type   = ::graph::graph <Policy>::difference_type;
};

#pragma endregion   Graph traits

#pragma endregion   Implementation of (standard) directed adjacency list graph without inedge iteration
