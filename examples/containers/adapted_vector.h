/**
 * @file examples/containers/adapted_vector.h
 * 
 * Wrapper around @c `std::vector` providing an interface
 * compatible with @c `graph::adjacency_list` as an edge
 * container (as defined in its @c `graph_traits` specialisation).
 */

#pragma once

#include <memory>
#include <ranges>
#include <utility>
#include <vector>

template <typename T, class Alloc = std::allocator <T> >
class AdaptedVector : public std::vector <T, Alloc>
{
    using Base = std::vector <T, Alloc>;

    public:
        using Base::Base;

        constexpr AdaptedVector(const Base& base)
            : Base{base}
        {}

        constexpr AdaptedVector(Base&& base)
            : Base{std::move(base)}
        {}

        auto insert(const std::ranges::range_value_t <Base>& elem)
        {
            this->push_back(elem);
            auto it = this->end();
            return --it;
        }

        auto insert(std::ranges::range_value_t <Base>&& elem)
        {
            this->emplace_back(std::move(elem));
            auto it = this->end();
            return --it;
        }

        template <class... Args>
        auto emplace(Args&&... args)
        {
            this->emplace_back(std::forward <Args>(args)...);
            auto it = this->end();
            return --it;
        }
};