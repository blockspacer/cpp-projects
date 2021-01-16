#pragma once
#include <iterator>
#include <type_traits>

namespace intrusive {
    struct default_tag;

    template<typename Tag = default_tag>
    struct list_element {
        list_element *next{};
        list_element *prev{};

        void bind_next(list_element &el) {
            el.prev = this;
            next = &el;
        }

        void bind_prev(list_element &el) {
            el.next = this;
            prev = &el;
        }

        void unlink() {
            if (prev != nullptr)
                prev->next = next;
            if (next != nullptr)
                next->prev = prev;
            prev = nullptr;
            next = nullptr;
        }

        ~list_element() {
            unlink();
        }
    };

    template<typename T, typename Tag = default_tag>
    struct list {
        template<class l_element>
        struct list_iterator {
            using l_element_type = std::remove_const_t<l_element>;
            using iterator_category = std::bidirectional_iterator_tag;
            using value_type = std::conditional_t<std::is_const_v<l_element>, std::add_const_t<T>, T>;
            using difference_type = std::ptrdiff_t;
            using pointer = value_type *;
            using reference = value_type &;

            template<class M>
            operator list_iterator<M>() const {
                return {current};
            }

            l_element_type *data() const {
                return current;
            }

            reference operator*() const noexcept {
                return static_cast<reference>(*current);
            }

            pointer operator->() const noexcept {
                return static_cast<pointer>(current);
            }

            bool operator==(list_iterator const &rhs) const noexcept {
                return current == rhs.current;
            }

            bool operator!=(list_iterator const &rhs) const noexcept {
                return current != rhs.current;
            }

            list_iterator &operator++() noexcept {
                current = current->next;
                return *this;
            }

            list_iterator &operator--() noexcept {
                current = current->prev;
                return *this;
            }

            list_iterator operator++(int) noexcept {
                list_iterator sv{*this};
                ++(*this);
                return sv;
            }

            list_iterator operator--(int) noexcept {
                list_iterator sv{*this};
                --(*this);
                return sv;
            }

            l_element_type *current;
        };

        using list_element_type = list_element<Tag>;
        using list_element_pointer = list_element_type *;

        using iterator = list_iterator<list_element_type>;
        using const_iterator = list_iterator<const list_element_type>;

        static_assert(std::is_convertible_v<T &, list_element<Tag> &>, "value type is not convertible to list_element");
    private:
        mutable list_element<Tag> fake_{&fake_, &fake_};

        friend void swap(list &lhs, list &rhs) {
            std::pair lfake{lhs.fake_.prev, lhs.fake_.next};
            std::pair rfake{rhs.fake_.prev, rhs.fake_.next};
            if (rfake.second != &rhs.fake_) {
                rfake.second->prev = &lhs.fake_;
                lhs.fake_.next = rfake.second;
            } else {
                lhs.fake_.next = &lhs.fake_;
            }

            if (rfake.first != &rhs.fake_) {
                rfake.first->next = &lhs.fake_;
                lhs.fake_.prev = rfake.first;
            } else {
                lhs.fake_.prev = &lhs.fake_;
            }

            if (lfake.second != &lhs.fake_) {
                lfake.second->prev = &rhs.fake_;
                rhs.fake_.next = lfake.second;
            } else {
                rhs.fake_.next = &rhs.fake_;
            }

            if (lfake.first != &lhs.fake_) {
                lfake.first->next = &rhs.fake_;
                rhs.fake_.prev = lfake.first;
            } else {
                rhs.fake_.prev = &rhs.fake_;
            }
        }

    public:
        list() noexcept = default;

        list(list const &) = delete;

        list(list &&l) {
            swap(*this, l);
            l.clear();
        };

        list &operator=(list const &) = delete;

        list &operator=(list &&l) noexcept {
            swap(*this, l);
            l.clear();
            return *this;
        }

        void clear() noexcept {
            fake_.unlink();
            fake_ = {&fake_, &fake_};
        }

        ~list() {
            clear();
        }

        void push_back(T &el) noexcept {
            auto pointer = static_cast<list_element_pointer>(&el);
            fake_.prev->next = pointer;
            pointer->next = &fake_;
            pointer->prev = fake_.prev;
            fake_.prev = pointer;
        }

        void pop_back() noexcept {
            if (!empty())
                fake_.prev->unlink();
        }

        T &back() noexcept {
            return *(--end());
        }

        T const &back() const noexcept {
            return *(--end());
        }

        void push_front(T &el) noexcept {
            auto pointer = static_cast<list_element_pointer>(&el);
            fake_.next->prev = pointer;
            pointer->prev = &fake_;
            pointer->next = fake_.next;
            fake_.next = pointer;
        }

        void pop_front() noexcept {
            fake_.next->unlink();
        }

        T &front() noexcept {
            return *begin();
        }

        T const &front() const noexcept {
            return *begin();
        }

        bool empty() const noexcept {
            return fake_.next == &fake_;
        }

        iterator begin() noexcept {
            return {fake_.next};
        }

        const_iterator begin() const noexcept {
            return {fake_.next};
        }

        iterator end() noexcept {
            return {&fake_};
        }

        const_iterator end() const noexcept {
            return {&fake_};
        }

        iterator insert(const_iterator pos, T &el) noexcept {
            auto pointer{static_cast<list_element_pointer>(&el)};
            auto prev{pos->prev};
            pos.data()->prev->next = pointer;
            pointer->prev = pos->prev;
            pos.data()->prev = pointer;
            pointer->next = pos.data();
            return {pointer};
        }

        iterator erase(const_iterator pos) noexcept {
            auto prev{pos->prev};
            auto next{pos->next};
            prev->next = next;
            next->prev = prev;
            return {next};
        }

        void splice(const_iterator pos, list &l, const_iterator first, const_iterator last) noexcept {
            if (first == last) {
                return;
            }
            if (std::next(first,1) == last) {
                first.data()->unlink();
                insert(pos, *iterator{first.data()});
                return;
            }
            auto last1 = last.data()->prev;

            first.data()->prev->next = last.data();
            last.data()->prev = first.data()->prev;

            last1->next = pos.data();
            pos.data()->prev->next = first.data();
            first.data()->prev = pos.data()->prev;
            pos.data()->prev = last1;
        }
    };

}
