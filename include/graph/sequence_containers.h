/**
 * @file graph/sequence_containers.h
 * 
 * Wrapper classes for sequence containers.
 */

#pragma once

#include <deque>
#include <list>
#include <memory>
#include <ranges>
#include <vector>

namespace graph
{
    template <typename T, class Allocator = std::allocator <T> >
    class vector;

    template <typename T, class Allocator = std::allocator <T> >
    class list;

    template <typename T, class Allocator = std::allocator <T> >
    class deque;
    
} // namespace graph

/// @brief Wrapper around @c `std::vector` that provides default
///        behaviour for @c `insert()` and related methods in the
///        absence of a position argument.
/// @tparam T Type of the elements stored.
/// @tparam Allocator Allocator type.
template <typename T, class Allocator>
class graph::vector : public std::vector <T, Allocator>
{
    using Base = std::vector <T, Allocator>;
    using Class = vector;

public:
    using Base::Base; // Inherit constructors
    
    using typename Base::size_type;
    using typename Base::iterator;
    using Base::insert;
    using Base::emplace;
    using Base::insert_range;

    /// @brief Call @c `push_back()` on @c `value` and return an
    ///        iterator to the newly inserted element.
    /// @param value Element to insert into the container.
    /// @return An iterator to the newly inserted element.
    /// @exception Any exceptions thrown by @c `push_back()`.
    constexpr iterator insert(const T& value)
    {
        Base::push_back(value);
        return std::prev(Base::end());
    }

    /// @brief Call @c `push_back()` on @c `value` and return an
    ///        iterator to the newly inserted element.
    /// @param value Element to insert into the container.
    /// @return An iterator to the newly inserted element.
    /// @exception Any exceptions thrown by @c `push_back()`.
    constexpr iterator insert(T&& value)
    {
        Base::push_back(std::move(value));
        return std::prev(Base::end());
    }

    /// @brief Call @c `append_range()` on
    ///        @c `std::ranges::subrange(first, last)`
    ///        and return an iterator to
    ///        the first newly inserted element.
    /// @tparam Iter Iterator type that meets the requirements of
    ///         an input iterator.
    /// @param first Iterator to the first element in the range.
    /// @param last Iterator to one past the last element in the range.
    /// @return An iterator to the first newly inserted element.
    /// @exception Any exceptions thrown by @c `append_range()`.
    template <std::input_iterator Iter>
    constexpr iterator insert(Iter first, Iter last)
    {
        return insert_range(std::ranges::subrange(first, last));
    }

    /// @brief Call @c `this->insert(this->end(), count, value)`.
    /// @param count Number of copies of @c `value` to insert.
    /// @param value Element to insert into the container.
    /// @return An iterator to the first newly inserted element.
    /// @exception Any exceptions thrown by @c `insert()`.
    constexpr iterator insert(size_type count, const T& value)
    {
        return Base::insert(Base::end(), count, value);
    }

    /// @brief Call @c `this->insert(this->end(), ilist)`.
    /// @param ilist Initializer list of elements to insert.
    /// @return An iterator to the first newly inserted element.
    /// @exception Any exceptions thrown by @c `insert()`.
    constexpr iterator insert(std::initializer_list <T> ilist)
    {
        return Base::insert(Base::end(), ilist);
    }

    /// @brief Call @c `emplace_back()` on
    ///        @c `std::forward <Args>(args)...`.
    /// @tparam Args... Argument types to forward to @c `emplace_back()`.
    /// @param args... Arguments to forward to @c `emplace_back()`.
    /// @return An iterator to the newly inserted element.
    /// @exception Any exceptions thrown by @c `emplace_back()`.
    template <class... Args>
    constexpr iterator emplace(Args&&... args)
    {
        Base::emplace_back(std::forward <Args>(args)...);
        return std::prev(Base::end());
    }

    /// @brief Call @c `append_range()` on @c `std::forward <R>(range)`.
    /// @tparam R Input range type.
    /// @param range Input range of elements to insert.
    /// @return An iterator to the first newly inserted element.
    /// @exception Any exceptions thrown by @c `append_range()`.
    template <std::ranges::input_range R>
        requires std::convertible_to <std::ranges::range_reference_t <R>, T>
    constexpr iterator insert_range(R&& range)
    {
        const auto size_before = Base::size();
        append_range(std::forward <R>(range));
        return Base::begin() + size_before;
    }
};

/// @brief Wrapper around @c `std::list` that provides default
///        behaviour for @c `insert()` and related methods in the
///        absence of a position argument.
/// @tparam T Type of the elements stored.
/// @tparam Allocator Allocator type.
template <typename T, class Allocator>
class graph::list : public std::list <T, Allocator>
{
    using Base = std::list <T, Allocator>;
    using Class = list;

public:
    using Base::Base; // Inherit constructors
    
    using typename Base::size_type;
    using typename Base::iterator;
    using Base::insert;
    using Base::emplace;
    using Base::insert_range;

    /// @brief Call @c `push_back()` on @c `value` and return an
    ///        iterator to the newly inserted element.
    /// @param value Element to insert into the container.
    /// @return An iterator to the newly inserted element.
    /// @exception Any exceptions thrown by @c `push_back()`.
    constexpr iterator insert(const T& value)
    {
        Base::push_back(value);
        return std::prev(Base::end());
    }

    /// @brief Call @c `push_back()` on @c `value` and return an
    ///        iterator to the newly inserted element.
    /// @param value Element to insert into the container.
    /// @return An iterator to the newly inserted element.
    /// @exception Any exceptions thrown by @c `push_back()`.
    constexpr iterator insert(T&& value)
    {
        Base::push_back(std::move(value));
        return std::prev(Base::end());
    }

    /// @brief Call @c `append_range()` on
    ///        @c `std::ranges::subrange(first, last)`
    ///        and return an iterator to
    ///        the first newly inserted element.
    /// @tparam Iter Iterator type that meets the requirements of
    ///         an input iterator.
    /// @param first Iterator to the first element in the range.
    /// @param last Iterator to one past the last element in the range.
    /// @return An iterator to the first newly inserted element.
    /// @exception Any exceptions thrown by @c `append_range()`.
    template <std::input_iterator Iter>
    constexpr iterator insert(Iter first, Iter last)
    {
        return insert_range(std::ranges::subrange(first, last));
    }

    /// @brief Call @c `this->insert(this->end(), count, value)`.
    /// @param count Number of copies of @c `value` to insert.
    /// @param value Element to insert into the container.
    /// @return An iterator to the first newly inserted element.
    /// @exception Any exceptions thrown by @c `insert()`.
    constexpr iterator insert(size_type count, const T& value)
    {
        return Base::insert(Base::end(), count, value);
    }

    /// @brief Call @c `this->insert(this->end(), ilist)`.
    /// @param ilist Initializer list of elements to insert.
    /// @return An iterator to the first newly inserted element.
    /// @exception Any exceptions thrown by @c `insert()`.
    constexpr iterator insert(std::initializer_list <T> ilist)
    {
        return Base::insert(Base::end(), ilist);
    }
    
    /// @brief Call @c `emplace_back()` on
    ///        @c `std::forward <Args>(args)...`.
    /// @tparam Args... Argument types to forward to @c `emplace_back()`.
    /// @param args... Arguments to forward to @c `emplace_back()`.
    /// @return An iterator to the newly inserted element.
    /// @exception Any exceptions thrown by @c `emplace_back()`.
    template <class... Args>
    constexpr iterator emplace(Args&&... args)
    {
        Base::emplace_back(std::forward <Args>(args)...);
        return std::prev(Base::end());
    }

    template <std::ranges::input_range R>
        requires std::convertible_to <std::ranges::range_reference_t <R>, T>
    constexpr iterator insert_range(R&& range)
    {
        const auto iter_before = std::prev(Base::end());
        append_range(std::forward <R>(range));
        return std::next(iter_before);
    }
};

/// @brief Wrapper around @c `std::deque` that provides default
///        behaviour for @c `insert()` and related methods in the
///        absence of a position argument.
/// @tparam T Type of the elements stored.
/// @tparam Allocator Allocator type.
template <typename T, class Allocator>
class graph::deque : public std::deque <T, Allocator>
{
    using Base = std::deque <T, Allocator>;
    using Class = deque;

public:
    using Base::Base; // Inherit constructors
    
    using typename Base::size_type;
    using typename Base::iterator;
    using Base::insert;
    using Base::emplace;
    using Base::insert_range;

    /// @brief Call @c `push_back()` on @c `value` and return an
    ///        iterator to the newly inserted element.
    /// @param value Element to insert into the container.
    /// @return An iterator to the newly inserted element.
    /// @exception Any exceptions thrown by @c `push_back()`.
    constexpr iterator insert(const T& value)
    {
        Base::push_back(value);
        return std::prev(Base::end());
    }

    /// @brief Call @c `push_back()` on @c `value` and return an
    ///        iterator to the newly inserted element.
    /// @param value Element to insert into the container.
    /// @return An iterator to the newly inserted element.
    /// @exception Any exceptions thrown by @c `push_back()`.
    constexpr iterator insert(T&& value)
    {
        Base::push_back(std::move(value));
        return std::prev(Base::end());
    }

    /// @brief Call @c `append_range()` on
    ///        @c `std::ranges::subrange(first, last)`
    ///        and return an iterator to
    ///        the first newly inserted element.
    /// @tparam Iter Iterator type that meets the requirements of
    ///         an input iterator.
    /// @param first Iterator to the first element in the range.
    /// @param last Iterator to one past the last element in the range.
    /// @return An iterator to the first newly inserted element.
    /// @exception Any exceptions thrown by @c `append_range()`.
    template <std::input_iterator Iter>
    constexpr iterator insert(Iter first, Iter last)
    {
        return insert_range(std::ranges::subrange(first, last));
    }

    /// @brief Call @c `this->insert(this->end(), count, value)`.
    /// @param count Number of copies of @c `value` to insert.
    /// @param value Element to insert into the container.
    /// @return An iterator to the first newly inserted element.
    /// @exception Any exceptions thrown by @c `insert()`.
    constexpr iterator insert(size_type count, const T& value)
    {
        return Base::insert(Base::end(), count, value);
    }

    /// @brief Call @c `this->insert(this->end(), ilist)`.
    /// @param ilist Initializer list of elements to insert.
    /// @return An iterator to the first newly inserted element.
    /// @exception Any exceptions thrown by @c `insert()`.
    constexpr iterator insert(std::initializer_list <T> ilist)
    {
        return Base::insert(Base::end(), ilist);
    }
    
    /// @brief Call @c `emplace_back()` on
    ///        @c `std::forward <Args>(args)...`.
    /// @tparam Args... Argument types to forward to @c `emplace_back()`.
    /// @param args... Arguments to forward to @c `emplace_back()`.
    /// @return An iterator to the newly inserted element.
    /// @exception Any exceptions thrown by @c `emplace_back()`.
    template <class... Args>
    constexpr iterator emplace(Args&&... args)
    {
        Base::emplace_back(std::forward <Args>(args)...);
        return std::prev(Base::end());
    }

    /// @brief Call @c `append_range()` on @c `std::forward <R>(range)`.
    /// @tparam R Input range type.
    /// @param range Input range of elements to insert.
    /// @return An iterator to the first newly inserted element.
    /// @exception Any exceptions thrown by @c `append_range()`.
    template <std::ranges::input_range R>
        requires std::convertible_to <std::ranges::range_reference_t <R>, T>
    constexpr iterator insert_range(R&& range)
    {
        const auto iter_before = std::prev(Base::end());
        append_range(std::forward <R>(range));
        return std::next(iter_before);
    }
};