/**
 * @file graph/detail/utils.h
 * 
 * Utilities for use in other headers.
 */

#pragma once

#if __cpp_lib_ranges < 202211L

#warning "This header requires support for C++23 <ranges>."

#endif

#include <concepts>
#include <iterator>
#include <type_traits>
#include <ranges>

namespace graph
{
    /// @brief This namespace contains implementation details for the
    ///        @c `graph` library. It is not intended for use by
    ///        end-users and may change without notice.
    namespace detail
    {
        /// @brief Helper function for @c `graph::detail::iterator_concept`.
        /// 
        /// @tparam Iterator An iterator type.
        /// 
        /// @return A default-constructed instance of the iterator category tag
        ///         corresponding to the iterator concept of @c `Iterator`.
        /// 
        /// @note If @c `Iterator` does not satisfy one of the standard iterator
        ///       concepts, has no member type
        ///       @c `Iterator::iterator_concept`, and does not explicitly specialise
        ///       @c `std::iterator_traits <Iterator>`, then the program may not
        ///       compile.
        template <class Iterator>
        [[nodiscard]]
        static consteval auto
        get_iterator_concept() noexcept
        {
            if constexpr (requires {
                {typename Iterator::iterator_concept{}} -> std::same_as <typename Iterator::iterator_concept>;
            })
                return typename Iterator::iterator_concept{};
            else if constexpr (requires {
                {typename std::iterator_traits <Iterator>::iterator_concept{}} -> std::same_as <typename std::iterator_traits <Iterator>::iterator_concept>;
            })
                return typename std::iterator_traits <Iterator>::iterator_concept{};
            else if constexpr (std::contiguous_iterator <Iterator>)
                return std::contiguous_iterator_tag{};
            else if constexpr (std::random_access_iterator <Iterator>)
                return std::random_access_iterator_tag{};
            else if constexpr (std::bidirectional_iterator <Iterator>)
                return std::bidirectional_iterator_tag{};
            else if constexpr (std::forward_iterator <Iterator>)
                return std::forward_iterator_tag{};
            else if constexpr (std::input_iterator <Iterator>)
                return std::input_iterator_tag{};
            else if constexpr (std::input_or_output_iterator <Iterator>)
                return std::output_iterator_tag{};
            else
                static_assert(std::input_or_output_iterator <Iterator>, "'Iterator' is not an iterator");
        }

        /// @brief The iterator concept tag for the iterator type
        ///        corresponding to the standard iterator concept satisfied
        ///        by the type, unless overridden by a member typedef or
        ///        explicit specialisation of
        ///        @c `std::iterator_traits <Iterator>`.
        /// 
        /// @tparam Iterator An iterator type.
        /// 
        /// @note If @c `Iterator` does not satisfy one of the standard iterator
        ///       concepts, has no member type
        ///       @c `Iterator::iterator_concept`, and does not explicitly specialise
        ///       @c `std::iterator_traits <Iterator>`, then the program may not
        ///       compile.
        template <class Iterator>
        using iterator_concept = decltype(get_iterator_concept <Iterator>());
        
    } // namespace detail

} // namespace graph

namespace graph::detail
{
    /// @brief This namespace contains "fixes" to some aspects of the
    ///        @c `<ranges>` library defined in the C++26 standard
    ///        that do not sit well with the codebase due to apparently
    ///        insane design.
    /// 
    ///        It also contains some exposition-only (helper) concepts
    ///        defined in the C++26 standard.
    namespace ranges
    {
        /// @brief Implementation of the exposition-only helper concept
        ///        @e `simple-view` as defined in the C++26 standard (section
        ///        [range.utility.helpers]).
        /// 
        /// #### Implementation
        /// 
        /// The implementation is the same as that laid down in the
        /// Standard, save for the concept name.
        /// 
        /// ```c++
        /// template <class Range>
        /// concept simple_view
        ///     = std::ranges::view <Range>
        ///     && std::ranges::range <const Range>
        ///     && std::same_as <std::ranges::iterator_t <Range>, std::ranges::iterator_t <const Range> >
        ///     && std::same_as <std::ranges::sentinel_t <Range>, std::ranges::sentinel_t <const Range> >;
        /// ```
        template <class Range>
        concept simple_view
            = std::ranges::view <Range>
            && std::ranges::range <const Range>
            && std::same_as <std::ranges::iterator_t <Range>, std::ranges::iterator_t <const Range> >
            && std::same_as <std::ranges::sentinel_t <Range>, std::ranges::sentinel_t <const Range> >;
           
        /// @brief Implementation of the exposition-only helper concept
        ///        @e `different-from` as defined in the C++26 standard
        ///        (section [range.utility.helpers]).
        /// 
        /// #### Implementation
        /// 
        /// The implementation is the same as that laid down in the
        /// Standard, save for the concept name.
        /// 
        /// ```c++
        /// template <class T, class U>
        /// concept different_from
        ///     = !std::same_as <std::remove_cvref_t <T>, std::remove_cvref_t <U> >;
        /// ```
        template <class T, class U>
        concept different_from = !std::same_as <std::remove_cvref_t <T>, std::remove_cvref_t <U> >;

        /// @brief A modified implementation of @c `std::ranges::ref_view`
        ///        that emulates the behaviour of the wrapped range on
        ///        `const`-qualification. This means that, for instance,
        ///        @c `std::ranges::cbegin` returns the same
        ///        @c `const_iterator` instance as the underlying range.
        /// 
        ///        More precisely, the `const`-qualified
        ///        overloads of member functions cast the underlying range
        ///        to a `const`-range where the Standard simply specifies the
        ///        return value
        ///        to be computed from the lvalue reference obtained by
        ///        dereferencing the underlying pointer to the wrapped range.
        /// 
        ///        Without this modification, @c `std::ranges::cbegin` called
        ///        on an instance of @c `std::ranges::ref_view <R>` would
        ///        return an instance of
        ///        @c `std::const_iterator <std::ranges::iterator_t <R> >`,
        ///        which may differ from
        ///        @c `std::ranges::iterator_t <const R>`. Worse still, 
        ///        @c `std::ranges::begin` called on an instance of
        ///        @c `const std::ranges::ref_view <R>` would return
        ///        an instance of @c `std::ranges::iterator_t <R>`.
        ///        
        /// @tparam R Range.
        template <std::ranges::range R>
            requires std::is_object_v <R>
        class ref_view : public std::ranges::view_interface <ref_view <R> >
        {
            private:
                R* r_;

                static void FUN(R&);
                static void FUN(R&&) = delete;

            public:
                template <different_from <ref_view> T>
                    requires std::convertible_to <T, R&>
                    && requires { FUN(std::declval <T>()); }
                constexpr ref_view(T&& t)
                    : r_{std::addressof(static_cast <R&>(std::forward <T>(t)))} {}

                constexpr R& base()
                { return *r_; }

                /// @brief 
                /// @return 
                /// @note The return type differs from that specified
                ///       by the C++26 standard.
                constexpr const R& base() const
                { return *r_; }

                constexpr std::ranges::iterator_t <R> begin()
                { return std::ranges::begin(*r_); }

                /// @brief 
                /// @return 
                /// @note The returned value differs from that specified
                ///       by the C++26 standard.
                constexpr auto begin() const requires std::ranges::range <const R>
                { return std::ranges::begin(static_cast <const R&>(*r_)); }

                constexpr std::ranges::sentinel_t <R> end()
                { return std::ranges::end(*r_); }

                /// @brief 
                /// @return 
                /// @note The returned value differs from that specified
                ///       by the C++26 standard.
                constexpr auto end() const requires std::ranges::range <const R>
                { return std::ranges::end(static_cast <const R&>(*r_)); }

                constexpr bool empty() const
                    requires requires { std::ranges::empty(*r_); }
                { return std::ranges::empty(*r_); }

                constexpr auto size() const requires std::ranges::sized_range <R>
                { return std::ranges::size(*r_); }

#if __cpp_lib_ranges_reserve_hint >= 202502L
                constexpr auto reserve_hint() const requires std::ranges::approximately_sized_range <R>
                { return std::ranges::reserve_hint(*r_); }
#endif

                constexpr auto data() requires std::ranges::contiguous_range <R>
                { return std::ranges::data(*r_); }

                /// @brief 
                /// @return 
                /// @note The returned value differs from that specified
                ///       by the C++26 standard.
                constexpr auto data() const requires std::ranges::contiguous_range <const R>
                { return std::ranges::data(static_cast <const R&>(*r_)); }
        
                // template <class... Args>
                // constexpr decltype(auto) operator[](Args&&... args)
                //     requires (!std::ranges::random_access_range <R>
                //     && requires (R range, Args&&... args) {
                //         range[std::forward <Args>(args)...];
                //     })
                // {
                //     return (*r_)[std::forward <Args>(args)...];
                // }

                // template <class... Args>
                // constexpr decltype(auto) operator[](Args&&... args) const
                //     requires (!std::ranges::random_access_range <const R>
                //     && requires (const R range, Args&&... args) {
                //         range[std::forward <Args>(args)...];
                //     })
                // {
                //     return static_cast <const R&>(*r_)[std::forward <Args>(args)...];
                // }
        };

        /// @brief Deduction guide for lvalue references.
        template <class R>
        ref_view(R&) -> ref_view <R>;

        /// @brief This namespace contains "fixes" to some aspects of the
        ///        @c `<ranges>` library defined in the C++26 standard
        ///        that do not sit well with the codebase due to apparently
        ///        insane design.
        /// 
        ///        It also contains some exposition-only (helper) concepts
        ///        defined in the C++26 standard.
        namespace views
        {
            namespace detail
            {
                struct All : std::ranges::range_adaptor_closure <All>
                {
                    private:
                        template <typename Range>
                        static consteval bool can_form_ref_view()
                        {
                            return requires {
                                graph::detail::ranges::ref_view{std::declval <Range>()};
                            };
                        }

                        template <typename Range>
                        static consteval bool can_form_owning_view()
                        {
                            return requires {
                                std::ranges::owning_view{std::declval <Range>()};
                            };
                        }

                        template <typename Range>
                        static consteval bool is_noexcept()
                        {
                            if constexpr (std::ranges::view <std::decay_t <Range> >)
                                return std::is_nothrow_constructible_v <std::decay_t <Range>, Range>;
                            else if constexpr (can_form_ref_view <Range>())
                                return true;
                            else
                                return noexcept(std::ranges::owning_view{std::declval <Range>()});
                        }

                    public:
                        /// @brief Return a view of all elements of the range
                        ///        @c `r`.
                        ///
                        ///        If @c `std::decay_t <Range>` is a view, it
                        ///        is returned unchanged. Else, if @c `Range`
                        ///        is a range, a view of type
                        ///        @c `graph::detail::views::all_t <Range>`
                        ///        is returned.
                        ///
                        /// @tparam Range Type of the range to be converted.
                        /// @param r Range to convert to a view.
                        /// @return 
                        template <std::ranges::viewable_range Range>
                            requires std::ranges::view <std::decay_t <Range> >
                            || (can_form_ref_view <Range>())
                            || (can_form_owning_view <Range>())
                        [[nodiscard]] constexpr auto
                        operator()(Range&& r) const noexcept(is_noexcept <Range>())
                        {
                            if constexpr (std::ranges::view <std::decay_t <Range> >)
                                return std::forward <Range>(r);
                            else if constexpr (can_form_ref_view <Range>())
                                return graph::detail::ranges::ref_view{std::forward <Range>(r)};
                            else
                                return std::ranges::owning_view{std::forward <Range>(r)};
                        }
                };
                
            } // namespace detail

            /// @brief A modified implementation of @c `std::views::all`
            ///        that replaces @c `std::ranges::ref_view` with
            ///        @c `graph::detail::ranges::ref_view`. Everything
            ///        else is exactly as per the C++26 standard and uses
            ///        entities from the @c `std` namespace (such as
            ///        @c `std::ranges::owning_view`).
            inline constexpr detail::All all;

            /// @brief A modified implementation of @c `std::views::all_t`
            ///        that replaces @c `std::views::all` with
            ///        @c `graph::detail::views::all`. Everything
            ///        else is exactly as per the C++26 standard and uses
            ///        entities from the @c `std` namespace.
            template <std::ranges::viewable_range Range>
            using all_t = decltype(all(std::declval <Range>()));
                        
        } // namespace views

    } // namespace ranges

    /// @brief This namespace contains "fixes" to some aspects of the
    ///        @c `<ranges>` library defined in the C++26 standard
    ///        that do not sit well with the codebase due to apparently
    ///        insane design.
    /// 
    ///        It also contains some exposition-only (helper) concepts
    ///        defined in the C++26 standard.
    namespace views
    {
        using namespace ranges::views;
        
    } // namespace views
        
} // namespace graph::detail

namespace std::ranges
{
    /// @brief Explicit partial specialisation for
    ///        @c `graph::detail::ranges::ref_view`.
    /// 
    /// @tparam R Range wrapped by @c `graph::detail::ranges::ref_view`.
    template <class R>
    inline constexpr bool enable_borrowed_range <graph::detail::ranges::ref_view <R> > = true;

} // namespace std::ranges
