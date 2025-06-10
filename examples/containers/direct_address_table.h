/**
 * @file direct_address_table.h
 * 
 * Header-only implementation of a direct-address table.
 * 
 * @cite Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest,
 *       and Clifford Stein. 2009. Introduction to Algorithms, Fourth
 *       Edition (4th. ed.), pp. 273-274. The MIT Press.
 * 
 * @link https://github.com/anirudh-bijay/direct-address-table
 */

#pragma once

#include <algorithm>
#include <cassert>
#include <concepts>
#include <iterator>
#include <memory>
#include <stdexcept>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

namespace dat
{
    template <
        std::unsigned_integral Key,
        typename T,
        class Allocator = std::allocator <T>
    >
    class direct_address_table
    {
        public:
            using key_type                  = Key;
            using mapped_type               = T;
            using value_type                = std::pair <const key_type, mapped_type>;
            using allocator_type            = Allocator;

            static_assert(std::same_as <
                typename std::allocator_traits <allocator_type>::value_type,
                mapped_type
            >, "Allocator should have value type T");

            using mapped_container_type     = std::vector <mapped_type, allocator_type>;
            using size_type                 = typename mapped_container_type::size_type;
            using difference_type           = typename mapped_container_type::difference_type;

            class iterator;
            class const_iterator;
            using reverse_iterator          = std::reverse_iterator <iterator>;
            using const_reverse_iterator    = std::reverse_iterator <const_iterator>;

            using reference                 = std::iter_reference_t <iterator>;
            using const_reference           = std::iter_reference_t <const_iterator>;
            using pointer                   = std::iterator_traits <iterator>::pointer;
            using const_pointer             = std::iterator_traits <const_iterator>::pointer;
            
            static_assert(std::same_as <const_reference, std::iter_const_reference_t <iterator> >);

            using key_equal                 = std::equal_to <key_type>;

        private:
            using bool_allocator_type       = typename std::allocator_traits <allocator_type>::rebind_alloc <bool>;

        protected:
            mapped_container_type mapped_container{};
            std::vector <bool, bool_allocator_type> occupied{};
            size_type nmemb = 0;

        private:
            constexpr iterator
            make_iterator(const key_type index) noexcept
            { return iterator(this, index); }

            constexpr const_iterator
            make_const_iterator(const key_type index) const noexcept
            { return const_iterator(this, index); }

        public:
            constexpr direct_address_table() = default;

            constexpr direct_address_table(const allocator_type& alloc)
                requires std::constructible_from <bool_allocator_type, allocator_type>
                : mapped_container(alloc), occupied(bool_allocator_type(alloc)) {}

            constexpr direct_address_table(const allocator_type& alloc)
                requires (!std::constructible_from <bool_allocator_type, allocator_type>)
                : mapped_container(alloc), occupied{} {}

            template <std::input_iterator Iter>
            constexpr direct_address_table(Iter first, Iter last)
            {
                insert(first, last);
            }

            constexpr direct_address_table(std::initializer_list <value_type> ilist)
                : direct_address_table(ilist.begin(), ilist.end()) {}

            template <std::input_iterator InputIt>
            constexpr void insert(InputIt first, const InputIt last)
            {
                if constexpr (std::forward_iterator <InputIt>) {
                    /// Avoid multiple reallocations by determining the maximum
                    /// key value beforehand, then performing a single
                    /// reallocation if necessary.

                    const key_type max_key = value_type(*std::max_element(
                        first, last, [](const value_type& l, const value_type& r) {
                            return l.first < r.first;
                        })
                    ).first;

                    const size_type new_size = mapped_container.size() > max_key + 1
                        ? mapped_container.size()
                        : max_key + 1;

                    mapped_container.resize(new_size);
                    occupied.resize(new_size, false);

                    for (; first != last; ++first) {
                        value_type value(*first);
                        mapped_container[value.first] = value.second;
                        if (!occupied[value.first]) {
                            occupied[value.first] = true;
                            ++nmemb;
                        }
                    }
                } else {
                    /// Make repeated calls to @c `emplace`.

                    for (; first != last; ++first) {
                        emplace(*first);
                    }
                }
            }

            constexpr std::pair <iterator, bool>
            insert(const value_type& value)
            {
                return emplace(value);
            }

            constexpr std::pair <iterator, bool>
            insert(value_type&& value)
            {
                return emplace(std::move(value));
            }

            template <class... Args> requires std::is_constructible_v <value_type, Args...>
            constexpr std::pair <iterator, bool>
            emplace(Args&&... args)
            {
                value_type value(std::forward <Args>(args)...);
                return try_emplace(std::move(value.first), std::move(value.second));
            }

            template <typename M> requires std::is_assignable_v <mapped_type&, M>
            constexpr std::pair <iterator, bool>
            insert_or_assign(const key_type& key, M&& obj)
            {
                /// @note This pattern will be followed in other insertion
                ///       methods as well.
                if (key >= mapped_container.size()) {
                    /// We must avoid default-construction at the key
                    /// where insertion will occur.
                    mapped_container.reserve(key + 1);
                    mapped_container.resize(key);
                    mapped_container.emplace_back(std::forward <M>(obj));

                    /// We do not do so for @c `occupied` since the
                    /// unnecessary initial assignment of @c `false`
                    /// to @c `occupied[key]` can easily be optimised
                    /// out by the compiler without side effects.
                    /// 
                    /// Moreover, even with zero optimisation, the cost
                    /// of making two assignments to the internal size
                    /// counter exceeds the cost of an extra @c `bool`
                    /// assignment.
                    occupied.resize(key + 1, false);
                    occupied[key] = true;

                    return {make_iterator(key), true};
                }

                mapped_container[key] = std::forward <M>(obj);

                if (occupied[key]) {
                    return {make_iterator(key), false};
                } else {
                    occupied[key] = true;
                    ++nmemb;
                    return {make_iterator(key), true};
                }
            }

            template <class... Args>
            constexpr std::pair <iterator, bool>
            try_emplace(const key_type& key, Args&&... args)
            {
                if (key >= mapped_container.size()) {
                    /// See @c `insert` for justification.
                    mapped_container.reserve(key + 1);
                    mapped_container.resize(key);
                    mapped_container.emplace_back(std::forward <Args>(args)...);

                    occupied.resize(key + 1, false);
                    occupied[key] = true;

                    return {make_iterator(key), true};
                }

                if (occupied[key]) {
                    return {make_iterator(key), false};
                } else {
                    mapped_container[key] = mapped_type(std::forward <Args>(args)...);
                    occupied[key] = true;
                    ++nmemb;
                    return {make_iterator(key), true};
                }
            }

            constexpr iterator erase(const iterator pos)
            {
                const auto next_pos = std::next(pos);

                assert(occupied[std::get <0>(*pos)] == true);
                occupied[std::get <0>(*pos)] = false;
                --nmemb;

                return next_pos;
            }

            constexpr size_type erase(const key_type& key)
            {
                if (occupied[key]) {
                    occupied[key] = false;
                    --nmemb;
                    return 1;
                }

                return 0;
            }

            constexpr void clear() noexcept
            {
                mapped_container.clear();
                occupied.clear();
                nmemb = 0;
            }

            constexpr iterator begin() noexcept
            {
                return make_iterator(std::find(occupied.cbegin(), occupied.cend(), true) - occupied.cbegin());
            }

            constexpr const_iterator begin() const noexcept
            {
                return make_const_iterator(std::find(occupied.cbegin(), occupied.cend(), true) - occupied.cbegin());
            }

            constexpr const_iterator cbegin() const noexcept
            {
                return begin();
            }

            constexpr iterator end() noexcept
            {
                return make_iterator(mapped_container.size());
            }

            constexpr const_iterator end() const noexcept
            {
                return make_const_iterator(mapped_container.size());
            }

            constexpr const_iterator cend() const noexcept
            {
                return end();
            }

            constexpr iterator rbegin() noexcept
            {
                return std::make_reverse_iterator(end());
            }

            constexpr const_iterator rbegin() const noexcept
            {
                return std::make_reverse_iterator(end());
            }

            constexpr const_iterator crbegin() const noexcept
            {
                return rbegin();
            }

            constexpr iterator rend() noexcept
            {
                return std::make_reverse_iterator(begin());
            }

            constexpr const_iterator rend() const noexcept
            {
                return std::make_reverse_iterator(begin());
            }

            constexpr const_iterator crend() const noexcept
            {
                return rend();
            }

            constexpr allocator_type get_allocator() const noexcept
            {
                return mapped_container.get_allocator();
            }

            constexpr size_type size() const noexcept
            {
                return nmemb;
            }

            constexpr bool empty() const noexcept
            {
                return size() == 0;
            }

            constexpr size_type max_size() const noexcept
            {
                return std::min(mapped_container.max_size(), occupied.max_size());
            }

            constexpr size_type capacity() const noexcept
            {
                return std::min(mapped_container.capacity(), occupied.capacity());
            }

            constexpr void reserve(const size_type max_key)
            {
                mapped_container.reserve(max_key + 1);
                occupied.reserve(max_key + 1);
            }

            constexpr void shrink_to_fit()
            {
                mapped_container.shrink_to_fit();
                occupied.shrink_to_fit();
            }

            constexpr mapped_type& operator[](const key_type& key)
            {
                return try_emplace(key).first->second;
            }

            constexpr mapped_type& at(const key_type& key)
            {
                if (key >= occupied.size() || !occupied[key]) {
                    throw std::out_of_range("Key not in table");
                }

                return mapped_container[key];
            }

            constexpr const mapped_type& at(const key_type& key) const
            {
                if (key >= occupied.size() || !occupied[key]) {
                    throw std::out_of_range("Key not in table");
                }

                return mapped_container[key];
            }

            constexpr iterator find(const key_type& key) noexcept
            {
                if (key >= occupied.size() || !occupied[key])
                    return end();

                return make_iterator(key);
            }

            constexpr const_iterator find(const key_type& key) const noexcept
            {
                if (key >= occupied.size() || !occupied[key])
                    return cend();

                return make_const_iterator(key);
            }

            constexpr bool contains(const key_type& key) const noexcept
            {
                return find(key) != cend();
            }

            constexpr size_type count(const key_type& key) const noexcept
            {
                return contains(key);
            }
    };

    template <
        std::unsigned_integral Key,
        typename T,
        class Allocator
    >
    constexpr bool operator==(const direct_address_table <Key, T, Allocator>& lhs,
                              const direct_address_table <Key, T, Allocator>& rhs) noexcept
    {
        if (lhs.size() != rhs.size())
            return false;

        const auto ei = lhs.cend(), ej = rhs.cend();
        for (auto i = lhs.cbegin(); i != ei; ++i) {
            const auto j = rhs.find(std::get <0>(*i));
            
            if (j == ej || std::get <1>(*i) != std::get <1>(*j))
                return false;
        }

        return true;
    }

    template <
        std::unsigned_integral Key,
        typename T,
        class Allocator,
        class Pred
    >
    constexpr typename direct_address_table <Key, T, Allocator>::size_type
    erase_if(direct_address_table <Key, T, Allocator>& c, Pred pred)
    {
        const auto old_size = c.size();
        for (auto first = c.begin(), last = c.end(); first != last;)
        {
            if (pred(*first)) {
                first = c.erase(first);
            } else {
                ++first;
            }
        }

        return old_size - c.size();
    }

    template <
        std::unsigned_integral Key,
        typename T,
        class Allocator
    >
    class direct_address_table <Key, T, Allocator>::iterator
    {
        private:
            direct_address_table <Key, T, Allocator>* c_ptr;
            key_type key_;

        public:
            using difference_type   = direct_address_table::difference_type;
            using value_type        = direct_address_table::value_type; // `std::pair <const key_type, mapped_type>`
            using reference         = std::pair <const key_type&, mapped_type&>;
            using const_reference   = std::pair <const key_type&, const mapped_type&>;
            using iterator_concept  = std::bidirectional_iterator_tag;

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

            constexpr iterator() noexcept = default;

            explicit constexpr iterator(
                direct_address_table <Key, T, Allocator> * const c_ptr,
                const key_type& key
            ) noexcept : c_ptr(c_ptr), key_(key) {}

            constexpr operator const_iterator() const noexcept
            {
                return const_iterator(c_ptr, key_);
            }

            constexpr key_type key() const noexcept
            {
                return key_;
            }

            constexpr reference operator*() const noexcept
            {
                return {key_, c_ptr->mapped_container[key_]};
            }

            constexpr pointer operator->() const noexcept
            {
                return pointer{**this};
            }

            constexpr iterator& operator++()
            {
                while (++key_ < c_ptr->mapped_container.size() && !c_ptr->occupied[key_]);

                return *this;
            }

            constexpr iterator operator++(int)
            {
                const iterator copy(*this);
                ++*this;
                return copy;
            }

            constexpr iterator& operator--()
            {
                while (key_-- /* --key_ + 1 */ > 0 && !c_ptr->occupied[key_]);
                
                return *this;
            }

            constexpr iterator operator--(int)
            {
                const iterator copy(*this);
                --*this;
                return copy;
            }

            friend constexpr std::strong_ordering
            operator<=>(const iterator& lhs, const iterator& rhs) noexcept
            {
                assert(lhs.c_ptr == rhs.c_ptr);
                return lhs.key_ <=> rhs.key_;
            }

            friend constexpr bool
            operator==(const iterator& lhs, const iterator& rhs) noexcept = default;
    };

    template <
        std::unsigned_integral Key,
        typename T,
        class Allocator
    >
    class direct_address_table <Key, T, Allocator>::const_iterator
    {
        private:
            const direct_address_table* c_ptr;
            key_type key_;

        public:
            using difference_type   = direct_address_table::difference_type;
            using value_type        = direct_address_table::value_type; // `std::pair <const key_type, mapped_type>`
            using reference         = std::pair <const key_type&, const mapped_type&>;
            using const_reference   = std::pair <const key_type&, const mapped_type&>;
            using iterator_concept  = std::bidirectional_iterator_tag;

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

            constexpr const_iterator() noexcept = default;

            explicit constexpr const_iterator(
                const direct_address_table * const c_ptr,
                const key_type& key
            ) noexcept : c_ptr(c_ptr), key_(key) {}

            constexpr key_type key() const noexcept
            {
                return key_;
            }

            constexpr reference operator*() const noexcept
            {
                return {key_, c_ptr->mapped_container[key_]};
            }

            constexpr pointer operator->() const
            {
                return pointer{**this};
            }

            constexpr const_iterator& operator++()
            {
                while (++key_ < c_ptr->mapped_container.size() && !c_ptr->occupied[key_]);

                return *this;
            }

            constexpr const_iterator operator++(int)
            {
                const const_iterator copy(*this);
                ++*this;
                return copy;
            }

            constexpr const_iterator& operator--()
            {
                while (key_-- /* --key_ + 1 */ > 0 && !c_ptr->occupied[key_]);
                
                return *this;
            }

            constexpr const_iterator operator--(int)
            {
                const const_iterator copy(*this);
                --*this;
                return copy;
            }

            friend constexpr std::strong_ordering
            operator<=>(const const_iterator& lhs, const const_iterator& rhs) noexcept
            {
                assert(lhs.c_ptr == rhs.c_ptr);
                return lhs.key_ <=> rhs.key_;
            }
            
            friend constexpr bool
            operator==(const const_iterator& lhs, const const_iterator& rhs) noexcept = default;
    };

} // namespace dat