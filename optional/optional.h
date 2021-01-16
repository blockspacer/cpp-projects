#pragma once
#include <utility>
#include <type_traits>

struct nullopt_t {
};

struct in_place_t {
};

inline constexpr nullopt_t nullopt;
inline constexpr in_place_t in_place;

template<typename T, bool v = std::is_trivially_destructible_v<T>>
struct destructor_base {
    union {
        T data;
        char dummy;
    };
    bool have_value = false;

    constexpr destructor_base() noexcept : dummy() {}

    constexpr destructor_base(nullopt_t) noexcept : dummy() {}

    template<typename... Args>
    explicit constexpr destructor_base(in_place_t, Args &&... args) :
        data(std::forward<Args>(args)...), have_value(true) {}

    ~destructor_base() {
        reset();
    }

    void reset() {
        if (have_value) {
            data.~T();
            have_value = false;
        }
    }

};

template<typename T>
struct destructor_base<T, true> {
    union {
        T data;
        char dummy;
    };

    bool have_value = false;

    constexpr destructor_base() noexcept : dummy() {}

    constexpr destructor_base(nullopt_t) noexcept : dummy() {}

    template<typename... Args>
    explicit constexpr destructor_base(in_place_t, Args &&... args) :
        data(std::forward<Args>(args)...), have_value(true)
    {
    }

    void reset() {
        have_value = false;
    }

};

template<typename T, bool v = std::is_trivially_copyable_v<T>>
struct copy_base : public destructor_base<T> {
    using base = destructor_base<T>;
    using base::base;

    constexpr copy_base(copy_base const &rhs) {
        if (rhs.have_value) {
            new (&(this->data)) T(rhs.data);
            this->have_value = true;
        }
    };

    constexpr copy_base(copy_base &&rhs) noexcept(std::is_nothrow_move_constructible<T>::value){
        if (rhs.have_value) {
            new (&(this->data)) T(std::move(rhs.data));
            this->have_value = true;
        } 
    };

    copy_base &operator=(copy_base const &rhs) {
        if (this != &rhs) {
            if (this->have_value) {
                if (rhs.have_value) {
                    this->data = rhs.data;
                } else {
                    this->reset();
                }
            } else if (rhs.have_value) {
                new (&this->data) T(rhs.data);
                this->have_value = true;
            }
        } 
        return *this;    
    };

    copy_base &operator=(copy_base &&rhs) noexcept(std::is_nothrow_move_constructible<T>::value 
                                                && std::is_nothrow_move_assignable<T>::value){
        if (this != &rhs) {
            if (this->have_value) {
                if (rhs.have_value) {
                    this->data = std::move(rhs.data);
                } else {
                    this->reset();
                }
            } else if (rhs.have_value) {
                new (&this->data) T(std::move(rhs.data));
                this->have_value = true;
            }
        } 
        return *this;
    };
};

template<typename T>
struct copy_base<T, true> : public destructor_base<T> {
    using base = destructor_base<T>;
    using base::base;
};

template<typename T>
struct optional : copy_base<T> {
    using base = copy_base<T>;
    using base::base;

    explicit constexpr optional(T data) : base(in_place, std::move(data)) {}
    
    optional &operator=(nullopt_t) noexcept {
        this->reset();
        return *this;
    }

    constexpr explicit operator bool() const noexcept { 
        return this->have_value; 
    }

    constexpr T &operator*() noexcept { 
        assert(this->have_value); 
        return this->data; 
    }

    constexpr T const &operator*() const noexcept { 
        assert(this->have_value); 
        return this->data; 
    }

    constexpr T *operator->() noexcept { 
        assert(this->have_value); 
        return &(this->data); 
    }

    constexpr T const *operator->() const noexcept { 
        assert(this->have_value); 
        return &(this->data); 
    }

    template<typename... Args>
    void emplace(Args &&... args) {
        this->reset();
        new (&(this->data)) T(std::forward<Args>(args)...);
        this->have_value = true;
    }
};

template<typename T>
constexpr bool operator==(optional<T> const &l, optional<T> const &r) {
    if (bool(l) != bool(r)) return false;
    if (!bool(l) && !bool(r)) return true;
    return *l == *r;
}

template<typename T>
constexpr bool operator!=(optional<T> const &l, optional<T> const &r) {
    return !(l == r);
}

template<typename T>
constexpr bool operator<(optional<T> const &l, optional<T> const &r) {
    if (!r) return false;
    if (!l) return true;
    return *l < *r;
}

template<typename T>
constexpr bool operator<=(optional<T> const &l, optional<T> const &r) {
    return (l == r || l < r);
}

template<typename T>
constexpr bool operator>(optional<T> const &l, optional<T> const &r) {
    if (!l) return false;
    if (!r) return true;
    return *l > *r;
}

template<typename T>
constexpr bool operator>=(optional<T> const &l, optional<T> const &r) {
    return (l == r || l > r);
}
