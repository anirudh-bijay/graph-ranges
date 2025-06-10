/**
 * @file graph/adjacency_list.h
 * 
 * Generic adjacency list implementation of graphs for fast lookup
 * of nodes and iteration over neighbours/successors/outedges at the
 * cost of slower edge lookup.
 */

#pragma once

#if __cpp_lib_ranges < 202211L

#warning "This header requires support for C++23 <ranges>."

#endif

#include "detail/utils.h"
#include "graph_traits.h"

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

namespace graph
{
    namespace detail
    {
        namespace adjacency_list
        {
            /// @brief The concept @c `adjacency_list_compatible_range` is
            ///        satisfied by the class @c `R` if:
            /// 
            ///        - @c `R` satisfies @c `std::ranges::range`;
            /// 
            ///        - the value type of @c `R` is @em pair-like;
            /// 
            ///        - the second element of a value in @c `R` is
            ///          @em pair-like; and
            /// 
            ///        - the second element of the above element
            ///          satisfies @c `std::ranges::range`.
            /// 
            ///        Here, an object is 'pair-like' if its first and second elements
            ///        can be accessed through @c `std::get <0>` and @c `std::get <1>`,
            ///        respectively.
            template <class R>
            concept adjacency_list_compatible_range
                = std::ranges::range <R>
                && requires (std::ranges::range_value_t <R> value) {
                    std::get <0>(value);                                        // Vertex key
                    std::get <0>(std::get <1>(value));                          // Vertex data
                    {std::get <1>(std::get <1>(value))} -> std::ranges::range;  // Outedge list
                };

        } // namespace adjacency_list

    } // namespace detail

} // namespace graph
    
namespace graph
{
    /// @brief Graph represented by an adjacency list.
    /// 
    /// @tparam vId Key type for vertices.
    /// @tparam vTp Mapped type for vertices.
    /// @tparam eTp Satellite data for edges.
    /// @tparam vCont Template for an associative container taking
    ///               two template parameters: `Key` (the key type)
    ///               and `T` (the mapped type).
    /// @tparam eCont Template for a sequence container taking a
    ///               single template parameter: `T` (the value type).
    /// 
    /// ----
    /// 
    /// ### Introduction
    /// 
    /// The adjacency list representation of a graph is a
    /// collection of lists mapped to individual vertices
    /// with each list consisting of the direct successors of the
    /// corresponding vertex. For weighted graphs, the
    /// structure is augmented to store edge weights
    /// alongside the list entries.
    /// 
    /// ### Implementation
    /// 
    /// The implementation uses an associative container to
    /// map vertices to their corresponding lists of outedges.
    /// As it may not be desirable (or feasible) to use the
    /// entire vertex as a key to the associative container,
    /// the vertex is split into two different types: `vId`, 
    /// which is used as the key, and `vTp`, which consists
    /// of satellite data associated with the vertex.
    /// 
    /// The container type to use is specified by the template
    /// template argument `vCont`.
    /// The container should not permit duplicate keys;
    /// therefore, each vertex in the graph must have a unique
    /// key.
    /// 
    /// To make optimal use of the separation of the vertex
    /// representation into `vId` and `vTp`, the contents
    /// of `vId` must be constant for a given vertex; the
    /// only way to change the key of a vertex is to remove
    /// it from the graph and insert it with the new key,
    /// during which its adjacency list is destroyed. Any
    /// mutable data must be stored in `vTp`.
    /// 
    /// Ideally, `vId` should also be cheap to construct as
    /// it is frequently used as a handle to vertices.
    /// 
    /// The lists of outedges/direct successors of each
    /// vertex are implemented as sequence containers as
    /// specified by the template template argument `eCont`.
    /// 
    /// ### Containers
    /// 
    /// The template template arguments corresponding to
    /// the containers described above have certain constraints,
    /// as described below.
    /// 
    /// #### `template <typename Key, typename T> class vCont`
    /// 
    /// - `Key` corresponds to the key type.
    /// 
    /// - `T` corresponds to the mapped type.
    /// 
    /// - Lookup should ideally be a constant- or
    ///   logarithmic-time operation.
    /// 
    /// - The container should not permit duplicate keys.
    /// 
    /// #### Construction and assignment
    /// 
    /// - The container should be default-constructible.
    /// 
    /// - The container should be copy-constructible and
    ///   copy-assignable.
    /// 
    /// - The container should be constructible from a
    ///   pair of input iterators.
    /// 
    /// - The container should be constructible from an input
    ///   range of @c `std::pair <Key, T>`. This constructor
    ///   should take @c `std::from_range` as its first
    ///   argument and the range as its second argument.
    /// 
    /// - The container should be constructible and assignable
    ///   from an initializer list of @c `std::pair <Key, T>`.
    /// 
    /// #### Insertion
    /// 
    /// - Member functions `emplace`, `insert`, `try_emplace`,
    ///   and `insert_range` should be
    ///   available with every instance of `vCont` instantiated
    ///   with any set of
    ///   syntactically correct template arguments. They should
    ///   take arguments and return values with the same
    ///   semantics as in the C++ named requirement
    ///   @em [UnorderedAssociativeContainer](https://en.cppreference.com/w/cpp/named_req/UnorderedAssociativeContainer).
    /// 
    /// - Besides single-element insertion, the `insert` method
    ///   should also accept a pair of input iterators defining a
    ///   range of elements to insert.
    /// 
    /// - Insertion methods should return an @c `std::pair`
    ///   consisting of an iterator to the inserted element and a
    ///   boolean indicating successful insertion, in that order;
    ///   the returned iterator should
    ///   be an instance of the type aliased by
    ///   @c `std::ranges::iterator_t`.
    /// 
    /// #### Erasure
    /// 
    /// - Member functions `erase` and `clear` should be
    ///   available with every instance of `vCont` instantiated
    ///   with any set of syntactically correct template
    ///   arguments. They should take arguments and return
    ///   values with the same semantics as in the C++ named
    ///   requirement @em [UnorderedAssociativeContainer](https://en.cppreference.com/w/cpp/named_req/UnorderedAssociativeContainer).
    /// 
    /// - Erasure should be possible by key, by iterator, and
    ///   by a pair of iterators defining a range.
    /// 
    /// - The function `erase_if` should be overloaded for the
    ///   container and should be found by ADL.
    /// 
    /// #### Iteration
    /// 
    /// - The container should be an iterable: calls to
    ///   @c `std::ranges::begin` and @c `std::ranges::end`
    ///   with a container instance as their only argument should
    ///   be well-formed.
    /// 
    /// #### Lookup
    /// 
    /// - Member function `find` and `operator[]` should be
    ///   available with every instance of `vCont` instantiated
    ///   with any set of syntactically correct template
    ///   arguments. They should take arguments and return
    ///   values with the same semantics as in the C++ named
    ///   requirement @em [UnorderedAssociativeContainer](https://en.cppreference.com/w/cpp/named_req/UnorderedAssociativeContainer).
    /// 
    /// #### Capacity
    /// 
    /// - Member functions `empty`, `size`, and `max_size`
    ///   should be available with every instance of `vCont`
    ///   instantiated with any set of syntactically correct
    ///   template arguments. They should take no arguments
    ///   and return values with the same semantics as in the
    ///   C++ named requirement @em [Container](https://en.cppreference.com/w/cpp/named_req/Container).
    /// 
    /// #### Swap
    /// 
    /// - The function `swap` should be overloaded for the
    ///   container and should be found by ADL.
    /// 
    /// #### Other requirements
    /// 
    /// - @c `std::ranges::range_value_t <vCont <Key, T> >` should be
    ///   @em pair-like (as defined in the C++26 standard). If
    ///   @c `v` is an instance thereof, then
    ///   @c `decltype(std::get <0>(v))` should be @c `Key` and
    ///   @c `decltype(std::get <1>(v))` should be @c `T`.
    /// 
    /// #### `template <typename T> class eCont`
    /// 
    /// - `T` corresponds to the value type.
    /// 
    /// - Iteration over the entire container should take time
    ///   linear in the size of the container.
    ///
    /// #### Construction and assignment
    /// 
    /// - The container should be default-constructible.
    /// 
    /// - The container should be copy-constructible and
    ///   copy-assignable.
    /// 
    /// - The container should be constructible from a
    ///   pair of input iterators.
    /// 
    /// - The container should be constructible from an input
    ///   range of @c `T`. This constructor
    ///   should take @c `std::from_range` as its first
    ///   argument and the range as its second argument.
    /// 
    /// - The container should be constructible and assignable
    ///   from an initializer list of @c `T`.
    /// 
    /// #### Insertion
    /// 
    /// - Member functions `emplace`, `insert`, and `insert_range`
    ///   should be available with every instance of 
    ///   `eCont` instantiated with any set of
    ///   syntactically correct template arguments. They should
    ///   take arguments corresponding to the value(s) to insert
    ///   and return an iterator to the inserted element
    ///   (the first element in case of a range). The element
    ///   should preferably be inserted at a position that makes
    ///   the insertion as efficient as possible.
    /// 
    /// - Besides single-element insertion, the `insert` method
    ///   should also accept a pair of input iterators defining a
    ///   range of elements to insert.
    /// 
    /// - Insertion methods should return an iterator to the
    ///   inserted element. The iterator should be an instance
    ///   of the type aliased by @c `std::ranges::iterator_t`.
    /// 
    /// #### Erasure
    /// 
    /// - Member functions `erase` and `clear` should be
    ///   available with every instance of `eCont` instantiated
    ///   with any set of syntactically correct template
    ///   arguments. They should take arguments and return
    ///   values with the same semantics as in the C++ named
    ///   requirement @em [SequenceContainer](https://en.cppreference.com/w/cpp/named_req/SequenceContainer).
    /// 
    /// - Erasure should be possible by iterator and
    ///   by a pair of iterators defining a range.
    /// 
    /// - The function `erase_if` should be overloaded for the
    ///   container and should be found by ADL.
    /// 
    /// #### Iteration
    /// 
    /// - The container should be an iterable: calls to
    ///   @c `std::ranges::begin` and @c `std::ranges::end`
    ///   with a container instance as their only argument should
    ///   be well-formed.
    /// 
    /// #### Capacity
    /// 
    /// - Member functions `empty`, `size`, and `max_size`
    ///   should be available with every instance of `eCont`
    ///   instantiated with any set of syntactically correct
    ///   template arguments. They should take no arguments
    ///   and return values with the same semantics as in the
    ///   C++ named requirement @em [Container](https://en.cppreference.com/w/cpp/named_req/Container).
    /// 
    /// #### Swap
    /// 
    /// - The function `swap` should be overloaded for the
    ///   container and should be found by ADL.
    /// 
    /// #### Other requirements
    /// 
    /// - @c `std::ranges::range_value_t <eCont <T> >` should
    ///   alias @c `T`.
    /// 
    /// ### Undirected Graphs
    /// 
    /// A directed edge is represented in an adjacency list by
    /// an entry of the head vertex in the list of direct successors
    /// of the tail vertex.
    /// 
    /// Traditionally, an undirected edge is represented in an
    /// adjacency list as a pair of directed edges in opposite
    /// directions with the same ends as the undirected edge. This
    /// means that two entries of the edge are present in the
    /// adjacency list.
    /// 
    /// However, when edges have associated satellite data,
    /// the implementation
    /// creates separate `eTp` instances for each directed edge.
    /// This may be undesirable in circumstances where construction
    /// of `eTp` instances is costly.
    /// 
    /// In such scenarios, it is advisable to maintain a
    /// separate container holding edge data and configure
    /// `eTp` to hold handles to the data. An @c `std::list`
    /// would be a good choice for such a container, since:
    /// 
    /// - insertion and deletion do not invalidate iterators,
    ///   pointers, or references (except the past-the-end iterator
    ///   and, in the case of deletion, iterators/pointers/references
    ///   to the element being deleted); and
    /// 
    /// - insertion and deletion are constant-time operations.
    /// 
    /// @c `std::forward_list` cannot serve this purpose well
    /// due to its @c `erase_after` semantics; however, a custom
    /// implementation that handles this can be more
    /// memory-efficient than @c `std::list`.
    template <
        typename vId,
        typename vTp,
        typename eTp,
        template <typename Key,
                  typename T> class vCont,
        template <typename T> class eCont
    >
    class adjacency_list
    {
        friend struct graph_traits <adjacency_list <vId, vTp, eTp, vCont, eCont> >;

        private:
            using row_type = std::pair <vTp, eCont <std::pair <vId, eTp> > >;
            using table_type = vCont <vId, row_type>;
            using size_type = std::conditional <
                std::numeric_limits <
                    std::ranges::range_size_t <table_type>
                >::max() < std::numeric_limits <
                    std::ranges::range_size_t <std::tuple_element_t <1, row_type> >
                >::max(),
                std::ranges::range_size_t <table_type>,
                std::ranges::range_size_t <std::tuple_element_t <1, row_type> >
            >;
            using difference_type = std::conditional <
                std::numeric_limits <
                    std::ranges::range_difference_t <table_type>
                >::max() < std::numeric_limits <
                    std::ranges::range_difference_t <std::tuple_element_t <1, row_type> >
                >::max(),
                std::ranges::range_difference_t<table_type>,
                std::ranges::range_difference_t <std::tuple_element_t <1, row_type> >
            >;

        protected:
            table_type adj{};

        public:
            template <std::ranges::view V>
                requires std::ranges::input_range <V>
                && detail::adjacency_list::adjacency_list_compatible_range <V>
            class vertex_view;

            // Deduction guide to convert ranges to views (as if by `std::views::all`).
            template <std::ranges::viewable_range R>
            vertex_view(R&&) -> vertex_view <detail::views::all_t <R> >;
            
            template <std::input_iterator Iter>
            class outedge_view;

        private:
            using vertex_view_iterator_t = std::ranges::iterator_t <decltype(vertex_view(adj))>;
            using vertex_view_const_iterator_t = std::ranges::iterator_t <const decltype(vertex_view(adj))>;

            using outedge_view_iterator_t = std::ranges::iterator_t <outedge_view <std::ranges::iterator_t <table_type> > >;
            using outedge_view_const_iterator_t = std::ranges::iterator_t <const outedge_view <std::ranges::iterator_t <table_type> > >;

            static_assert(std::is_convertible_v <std::ranges::iterator_t <decltype(vertex_view(const_cast <const decltype(adj)&>(adj)))>, typename table_type::const_iterator>);
            static_assert(std::is_same_v <typename vertex_view_const_iterator_t::base_return_type, typename table_type::const_iterator>);

        public:
            constexpr adjacency_list() = default;

            constexpr adjacency_list(
                std::initializer_list <std::pair <vId, vTp> > v_set,
                std::initializer_list <std::tuple <vId, vId, eTp> > e_set = {}
            )
            {
                for (const auto& elem : v_set) {
                    insert_vertex(std::get <0>(elem), std::get <1>(elem));
                }
                for (const auto& elem : e_set) {
                    insert_edge(std::get <0>(elem), std::get <1>(elem), std::get <2>(elem));
                }
            }
        
            template <std::input_iterator VIter>
            constexpr adjacency_list(VIter first, VIter last)
            {
                while (first != last) {
                    insert_vertex(std::get <0>(*first), std::get <1>(*first));
                    ++first;
                }
            }

            constexpr vertex_view_iterator_t
            insert_vertex(const vId& v_key, const vTp& v_data)
            {
                return vertex_view_iterator_t{std::get <0>(adj.emplace(v_key, row_type{v_data, {}}))};
            }

            constexpr vertex_view_iterator_t
            insert_vertex(const vId& v_key, vTp&& v_data)
            {
                return vertex_view_iterator_t{std::get <0>(adj.emplace(v_key, row_type{std::move(v_data), {}}))};
            }

            constexpr void erase_vertex(const vId& v_key)
            {
                adj.erase(v_key);
                
                auto vertex_set = vertices();
                for (auto it = std::ranges::begin(vertex_set); it != std::ranges::end(vertex_set); ++it) {
                    /// @note @c `std::erase_if` should be found by ADL;
                    ///       hence, @c `erase_if` is used unqualified.
                    erase_if(std::get <1>(std::get <1>(*it.base())), [&v_key](const auto& elem) {
                        return elem.first == v_key;
                    });
                }
            }

            constexpr void erase_vertex(const vertex_view_iterator_t v_iter)
            {
                adj.erase(v_iter.base());
                
                const auto& v_key = std::get <0>(v_iter);
                auto vertex_set = vertices();

                for (auto it = std::ranges::begin(vertex_set); it != std::ranges::end(vertex_set); ++it) {
                    /// @note @c `std::erase_if` should be found by ADL;
                    ///       hence, @c `erase_if` is used unqualified.
                    erase_if(std::get <1>(std::get <1>(*it.base())), [&v_key](const auto& elem) {
                        return elem.first == v_key;
                    });
                }
            }

            constexpr auto
            insert_edge(const vId& tail_key, const vId& head_key, const eTp& e_data)
            {
                const auto row_iter = adj.find(tail_key);

                if (row_iter == std::ranges::end(adj)) [[unlikely]]
                    throw std::out_of_range("The vertex key for the tail does not exist in the graph");

                return outedge_view_iterator_t{row_iter, std::get <1>(std::get <1>(*row_iter)).emplace(head_key, e_data)};
            }

            constexpr auto
            insert_edge(const vId& tail_key, const vId& head_key, eTp&& e_data)
            {
                const auto row_iter = adj.find(tail_key);

                if (row_iter == std::ranges::end(adj)) [[unlikely]]
                    throw std::out_of_range("The vertex key for the tail does not exist in the graph");

                return outedge_view_iterator_t{row_iter, std::get <1>(std::get <1>(*row_iter)).emplace(head_key, std::move_if_noexcept(e_data))};
            }

            constexpr auto insert_edge(
                const vertex_view_iterator_t tail_iter,
                vertex_view_iterator_t head_iter,
                const eTp& e_data
            )
            {
                return outedge_view_iterator_t{
                    tail_iter.base(),
                    std::get <1>(std::get <1>(*tail_iter)).emplace(std::get <0>(*std::move_if_noexcept(head_iter)), e_data)
                };
            }

            constexpr auto insert_edge(
                const vertex_view_iterator_t tail_iter,
                vertex_view_iterator_t head_iter,
                eTp&& e_data
            )
            {
                return outedge_view_iterator_t{
                    tail_iter,
                    std::get <1>(std::get <1>(*tail_iter)).emplace(std::get <0>(*std::move_if_noexcept(head_iter)), std::move_if_noexcept(e_data))
                };
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
            }

            constexpr size_type erase_edges(const vId& tail_key, const vId& head_key)
            {
                /** REQUIREMENT: Same as above */

                /// @note @c `std::erase_if` should be found by ADL;
                ///       hence, @c `erase_if` is used unqualified.
                return erase_if(
                    std::get <1>(std::get <1>(*adj.find(tail_key))),
                    [&head_key](const auto& elem) -> bool {
                        return elem.first == head_key;
                    }
                );
            }

            constexpr size_type erase_edges(
                vertex_view_iterator_t tail_iter,
                const vertex_view_iterator_t head_iter
            )
            {
                /** REQUIREMENT: Same as above */
                
                const auto& head_key = std::get <0>(*head_iter);

                /// @note @c `std::erase_if` should be found by ADL;
                ///       hence, @c `erase_if` is used unqualified.
                return erase_if(
                    std::get <0>(std::get <0>(std::move_if_noexcept(tail_iter).base())),
                    [&head_key](const auto& elem) -> bool {
                        return elem.first == head_key;
                    }
                );
            }
    
            [[nodiscard]]
            constexpr std::ranges::range auto vertices() & noexcept(noexcept(vertex_view{adj}))
            {
                return vertex_view{adj};
            }

            [[nodiscard]]
            constexpr std::ranges::constant_range auto
            vertices() const& noexcept(noexcept(std::views::as_const(vertex_view{adj})))
            {
                return std::views::as_const(vertex_view{adj});
            }

            [[nodiscard]]
            constexpr std::ranges::range auto vertices() && noexcept(noexcept(vertex_view{std::move_if_noexcept(adj)}))
            {
                return vertex_view{std::move_if_noexcept(adj)};
            }

            [[nodiscard]]
            constexpr std::ranges::constant_range auto
            vertices() const&& noexcept(noexcept(std::views::as_const(vertex_view{std::move_if_noexcept(adj)})))
            {
                return std::views::as_const(vertex_view{std::move_if_noexcept(adj)});
            }
    
            [[nodiscard]]
            constexpr std::ranges::range auto
            outedges(const vertex_view_iterator_t tail_iter) noexcept(noexcept(outedge_view{tail_iter.base()}))
            {
                const auto& row_iter = tail_iter.base();

                return outedge_view{row_iter};
            }

            [[nodiscard]]
            constexpr std::ranges::constant_range auto
            outedges(const vertex_view_const_iterator_t tail_iter) const noexcept(noexcept(std::views::as_const(outedge_view{tail_iter.base()})))
            {
                const auto& row_iter = tail_iter.base();

                // NO LONGER NEEDED
                // // Temporary workaround for std::basic_const_iterator (reproducible by
                // // iterating over the edges of a const-qualified graph):
                // if constexpr (
                //     std::same_as <decltype(row_iter), std::ranges::iterator_t <table_type> >
                //     || std::same_as <decltype(row_iter), std::ranges::iterator_t <const table_type> >
                // )
                    return std::views::as_const(outedge_view{row_iter});
                // else
                //     return std::views::as_const(outedge_view{row_iter.base()});
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

            // [[nodiscard]]
            // constexpr auto edges() &
            // {
            //     return std::views::transform(
            //         std::views::zip(
            //             // Tail keys
            //             std::views::join(std::views::transform(adj, [](const auto& row) {
            //                 return std::views::repeat(row.second.second.size(), row.first);
            //             })),
            //             // Head and tail keys
            //             std::views::join(std::views::values(std::views::values(adj)))
            //         ),
            //         [](auto&& elem) {
            //             return std::tuple <const vId&, const vId&, eTp&>{
            //                 std::get <0>(elem),
            //                 std::get <0>(std::get <1>(elem)),
            //                 std::get <1>(std::get <1>(elem))
            //             };
            //         }
            //     );
            // }

            // [[nodiscard]]
            // constexpr std::ranges::constant_range auto
            // edges() const&
            // {
            //     return std::views::transform(
            //         std::views::zip(
            //             // Tail keys
            //             std::views::join(std::views::transform(adj, [](const auto& row) {
            //                 return std::views::repeat(row.second.second.size(), row.first);
            //             })),
            //             // Head and tail keys
            //             std::views::join(std::views::values(std::views::values(adj)))
            //         ),
            //         [](const auto& elem) {
            //             return std::tuple <const vId&, const vId&, const eTp&>{
            //                 std::get <0>(elem),
            //                 std::get <0>(std::get <1>(elem)),
            //                 std::get <1>(std::get <1>(elem))
            //             };
            //         }
            //     );
            // }

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

            [[nodiscard]]
            constexpr bool empty() const noexcept(noexcept(adj.empty()))
            {
                return std::ranges::empty(adj);
            }

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

    template <
        typename vId,
        typename vTp,
        typename eTp,
        template <typename Key,
                  typename T> class vCont,
        template <typename T> class eCont
    >
    template <std::ranges::view V>
        requires std::ranges::input_range <V>
        && detail::adjacency_list::adjacency_list_compatible_range <V>
    class adjacency_list <vId, vTp, eTp, vCont, eCont>::vertex_view
        : public std::ranges::view_interface <vertex_view <V> >
    {
        using Base  = std::ranges::view_interface <vertex_view>;    // The base class.
        using Class = vertex_view;                                  // The class.

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
    
            // template <class... Args>
            // constexpr decltype(auto) operator[](Args&&... args)
            //     requires (!std::ranges::random_access_range <V>)
            //     // && requires (V view, Args&&... args) {
            //     //     view[std::forward <Args>(args)...];
            //     // })
            // {
            //     return view[std::forward <Args>(args)...];
            // }

            // template <class... Args>
            // constexpr decltype(auto) operator[](Args&&... args) const
            //     requires (!std::ranges::random_access_range <V>)
            //     // && requires (const V view, Args&&... args) {
            //     //     view[std::forward <Args>(args)...];
            //     // })
            // {
            //     return view[std::forward <Args>(args)...];
            // }
    };

    template <
        typename vId,
        typename vTp,
        typename eTp,
        template <typename Key,
                  typename T> class vCont,
        template <typename T> class eCont
    >
    template <std::ranges::view V>
        requires std::ranges::input_range <V>
        && detail::adjacency_list::adjacency_list_compatible_range <V>
    template <bool Const>
    class adjacency_list <vId, vTp, eTp, vCont, eCont>::vertex_view <V>::iterator_impl
    {
        using Class = iterator_impl;                                // The class.

        friend class adjacency_list;

        template <bool OtherConst>
        friend class sentinel_impl;

        // friend constexpr const_iterator::const_iterator(const iterator&) noexcept;
        // friend constexpr const_iterator::const_iterator(iterator&&) noexcept;

        public:
            using base_iterator_type = std::ranges::iterator_t <std::conditional_t <Const, const V, V> >;
            base_iterator_type base_iter;

            using base_return_type = base_iterator_type;

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
            using value_type        = std::tuple <const vId, std::tuple_element_t <0, std::tuple_element_t <1, std::ranges::range_value_t <V> > > >;
            using reference         = std::tuple <const vId&, decltype(std::get <0>(std::get <1>(std::declval <std::ranges::range_reference_t <std::conditional_t <Const, const V, V> > >())))>;
            using const_reference   = std::tuple <const vId&, const vTp&>;
            using iterator_category = std::iterator_traits <base_iterator_type>::iterator_category;
            using iterator_concept  = detail::iterator_concept <base_iterator_type>;

            class [[nodiscard]] pointer
            {
                friend class iterator_impl;
                
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
    
            [[nodiscard]]
            friend constexpr bool operator==(const Class& lhs, const Class& rhs)
                requires std::equality_comparable <base_iterator_type> = default;

            [[nodiscard]]
            friend constexpr bool operator<=>(const Class& lhs, const Class& rhs)
                requires std::random_access_iterator <base_iterator_type> = default;

            constexpr operator iterator_impl <true>() requires (!Const && std::ranges::range <const V>)
            {
                return iterator_impl <true>(base_iter);
            }

            template <std::constructible_from <base_return_type> T>
                requires (!std::same_as <T, iterator_impl <true> >)
            constexpr operator T() noexcept(std::is_nothrow_constructible_v <T, base_return_type>)
            {
                return T{base_iter};
            }
    };

    template <
        typename vId,
        typename vTp,
        typename eTp,
        template <typename Key,
                  typename T> class vCont,
        template <typename T> class eCont
    >
    template <std::ranges::view V>
        requires std::ranges::input_range <V>
        && detail::adjacency_list::adjacency_list_compatible_range <V>
    template <bool Const>
    class adjacency_list <vId, vTp, eTp, vCont, eCont>::vertex_view <V>::sentinel_impl
    {
        using Class = sentinel_impl;                                // The class.

        friend class adjacency_list;

        private:
            using base_sentinel_type = std::ranges::sentinel_t <std::conditional_t <Const, const V, V> >;
            base_sentinel_type base_sent;

            using base_return_type = base_sentinel_type;

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
            /// @brief Default constructor to satisfy the @c `sentinel_for <iterator>`
            ///        concept. This concept is used by @c `std::ranges::end`
            ///        to ensure that a sentinel can be constructed and
            ///        compared against an iterator.
            constexpr sentinel_impl() noexcept = default;

            explicit constexpr sentinel_impl(base_sentinel_type base_sent) noexcept(noexcept(base_sentinel_type{std::move_if_noexcept(base_sent)}))
                : base_sent{std::move_if_noexcept(base_sent)} {}
            
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

            constexpr operator sentinel_impl <true>() requires (!Const && std::ranges::range <const V>)
            {
                return sentinel_impl <true>(base_sent);
            }
    };

    template <
        typename vId,
        typename vTp,
        typename eTp,
        template <typename Key,
                  typename T> class vCont,
        template <typename T> class eCont
    >
    template <std::input_iterator Iter>
    class adjacency_list <vId, vTp, eTp, vCont, eCont>::outedge_view
        : public std::ranges::view_interface <outedge_view <Iter> >
    {
        using Base  = std::ranges::view_interface <outedge_view>;  // The base class.
        using Class = outedge_view;                                // The class.
        
        protected:
            using V = detail::views::all_t <decltype(std::get <1>(std::get <1>(*std::declval <Iter>())))>;

            Iter tail_iter;
            V children_view;

            template <bool Const> class iterator_impl;
            template <bool Const> class sentinel_impl;

        public:
            /// TODO: Support initialisation from @c `std::ranges::const_iterator_t <adjacency_list::table_type>`

            constexpr outedge_view() = default;
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
        template <typename Key,
                  typename T> class vCont,
        template <typename T> class eCont
    >
    template <std::input_iterator Iter>
    template <bool Const>
    class adjacency_list <vId, vTp, eTp, vCont, eCont>::outedge_view <Iter>::iterator_impl
    {
        using Class = iterator_impl;                                // The class.

        friend class adjacency_list;

        template <bool OtherConst>
        friend class sentinel_impl;

        private:
            using V = detail::views::all_t <decltype(std::get <1>(std::get <1>(*std::declval <Iter>())))>;

            using tail_iterator_type = Iter;
            using base_iterator_type = std::ranges::iterator_t <std::conditional_t <Const, const V, V> >;
            using base_return_type = std::pair <
                tail_iterator_type,
                base_iterator_type
            >;
            base_return_type base_iter;

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
                friend class iterator_impl;
                
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
    
            [[nodiscard]]
            friend constexpr bool operator==(const Class& lhs, const Class& rhs)
                requires std::equality_comparable <base_return_type> = default;
            
            constexpr operator iterator_impl <true>() requires (!Const && std::ranges::range <const V>)
            {
                return iterator_impl <true>(base_iter);
            }

            template <std::constructible_from <base_return_type> T>
                requires (!std::same_as <T, iterator_impl <true> >)
            constexpr operator T() noexcept(std::is_nothrow_constructible_v <T, base_return_type>)
            {
                return T{base_iter};
            }
    };

    template <
        typename vId,
        typename vTp,
        typename eTp,
        template <typename Key,
                  typename T> class vCont,
        template <typename T> class eCont
    >
    template <std::input_iterator Iter>
    template <bool Const>
    class adjacency_list <vId, vTp, eTp, vCont, eCont>::outedge_view <Iter>::sentinel_impl
    {
        using Class = sentinel_impl;                                // The class.

        friend class adjacency_list;

        private:
            using V = detail::views::all_t <decltype(std::get <1>(std::get <1>(*std::declval <Iter>())))>;

            using tail_iterator_type = Iter;
            using base_sentinel_type = std::ranges::sentinel_t <std::conditional_t <Const, const V, V> >;
            using base_return_type = std::pair <
                tail_iterator_type,
                base_sentinel_type
            >;
            base_return_type base_sent;

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

            constexpr operator sentinel_impl <true>() requires (!Const && std::ranges::range <const V>)
            {
                return sentinel_impl <true>(base_sent.first, base_sent.second);
            }
    };

    template <
        typename vId,
        typename vTp,
        typename eTp,
        template <typename Key,
                  typename T> class vCont,
        template <typename T> class eCont
    >
    struct graph_traits <adjacency_list <vId, vTp, eTp, vCont, eCont> >
    {
        template <typename Key, typename T> using vertex_container = vCont <Key, T>;
        template <typename T> using edge_container = eCont <T>;

        using vertex_key_type = vId;
        using vertex_mapped_type = vTp;
        using vertex_value_type = std::tuple <vId, vTp>;
        using edge_type = eTp;

        using vertex_iterator = adjacency_list <vId, vTp, eTp, vCont, eCont>::vertex_view_iterator_t;
        using const_vertex_iterator = adjacency_list <vId, vTp, eTp, vCont, eCont>::vertex_view_const_iterator_t;
        using outedge_iterator = adjacency_list <vId, vTp, eTp, vCont, eCont>::outedge_view_iterator_t;
        using const_outedge_iterator = adjacency_list <vId, vTp, eTp, vCont, eCont>::outedge_view_const_iterator_t;

        using size_type = adjacency_list <vId, vTp, eTp, vCont, eCont>::size_type;
        using difference_type = adjacency_list <vId, vTp, eTp, vCont, eCont>::difference_type;
    };

} // namespace graph

namespace std::ranges
{
    /// We cannot perform a partial specialisation this way as
    /// @c `vertex_view` is nested in a templated class. The compiler
    /// cannot look an additional level up the nesting scope and see 
    /// if the concerned class matches the specified rule: it's way
    /// too much work.
    /// 
    /// TODO: The workaround to this is to extract @c `vertex_view`
    /// out of @c `adjacency_list` into its own templated class as
    /// part of implementation detail
    /// (namespace @c `graph::detail::adjacency_list`), then
    /// use a @c `using`-declaration to introduce it into the scope
    /// of @c `adjacency_list`.
    
//     /// @brief Explicit partial specialisation for @c `graph::adjacency_list::vertex_view`.
//     /// 
//     ///        If the view @c `V` is the template argument for @c `vertex_view`, then
//     ///        @c `std::ranges::enable_borrowed_range` is @c `true` for @c `vertex_view <V>`
//     ///        iff @c `std::ranges::enable_borrowed_range <V>` is @c `true`.
//     template <
//         typename vId,
//         typename vTp,
//         typename eTp,
//         template <typename Key,
//                   typename T> class vCont,
//         template <typename T> class eCont,
//         std::ranges::view V
//     > requires std::ranges::input_range <V>
//         && graph::detail::adjacency_list::adjacency_list_compatible_range <V>
//     constexpr bool enable_borrowed_range <typename graph::adjacency_list <vId, vTp, eTp, vCont, eCont>::vertex_view <V> >
//         = std::ranges::borrowed_range <V>;

} // namespace std::ranges
