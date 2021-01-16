#pragma once

#include <array>
#include <functional>
#include <iostream>
#include <stdexcept>
#include <tuple>
#include <type_traits>

template <class... Args> class variant;

struct in_place_t {
    explicit in_place_t() = default;
};

inline constexpr in_place_t in_place{};

template <class T> struct in_place_type_t { explicit in_place_type_t() = default; };

template <class T> inline constexpr in_place_type_t<T> in_place_type{};

template <std::size_t i> struct in_place_index_t { explicit in_place_index_t() = default; };

template <std::size_t I> inline constexpr in_place_index_t<I> in_place_index{};
struct monostate {};

constexpr bool operator==(monostate, monostate) noexcept { return true; }

constexpr bool operator!=(monostate, monostate) noexcept { return false; }

constexpr bool operator<(monostate, monostate) noexcept { return false; }

constexpr bool operator>(monostate, monostate) noexcept { return false; }

constexpr bool operator<=(monostate, monostate) noexcept { return true; }

constexpr bool operator>=(monostate, monostate) noexcept { return true; }

namespace std {
    template <> struct hash<::monostate> {
        size_t operator()(const ::monostate &) { return 0; }
    };
} // namespace std

namespace detail {
    template <class... T> struct Pack;
} // namespace detail

template<std::size_t Np, typename ...Ts>
struct variant_alternative_imp : std::tuple_element<Np, std::tuple<Ts...>>{};

template<std::size_t Np, typename T>
struct variant_alternative;

template<std::size_t Np, typename T>
using variant_alternative_t = typename variant_alternative<Np, T>::type;

template<std::size_t Np, typename T>
struct variant_alternative<Np, const T> {
    using type = std::add_const_t<variant_alternative_t<Np, T>>;
};

template<std::size_t Np, typename Head, typename ...Tail>
struct variant_alternative<Np, variant<Head, Tail...>> {
    using type = typename variant_alternative_imp<Np, Head, Tail...>::type;
};


template <class T> struct variant_size;

template <class... Types>
struct variant_size<variant<Types...>> : std::integral_constant<std::size_t, sizeof...(Types)> {};

template <class T> struct variant_size<const T> : variant_size<T> {};

template <class T> struct variant_size<volatile T> : variant_size<T> {};

template <class T> struct variant_size<const volatile T> : variant_size<T> {};

template <class T> inline constexpr std::size_t variant_size_v = variant_size<T>::value;

inline constexpr size_t variant_npos = -1;

namespace detail {

    template <class T, class... Types> struct IndexOf : std::integral_constant<std::size_t, 0> {};

    template <typename Tp, typename... Types> inline constexpr size_t IndexOf_v = IndexOf<Tp, Types...>::value;

    template <class T, class F, class... Rest>
    struct IndexOf<T, F, Rest...>
            : std::integral_constant<std::size_t, std::is_same_v<T, F> ? 0 : IndexOf_v<T, Rest...> + 1> {};

    template <class A, class B> struct UnionHolder {
        union {
            A first;
            B second;
        };

        constexpr UnionHolder() noexcept(std::is_nothrow_default_constructible_v<A>) : second(){};
        template <class... T>
        constexpr UnionHolder(in_place_index_t<0>, T &&...t) noexcept(std::is_nothrow_constructible_v<A, T...>)
                : first(in_place_index<0>, std::forward<T>(t)...) {}
        template <std::size_t I, class... T>
        constexpr UnionHolder(in_place_index_t<I>, T &&...t) noexcept(std::is_nothrow_constructible_v<B, T...>)
                : second(in_place_index_t<I - 1>{}, std::forward<T>(t)...) {}

        constexpr UnionHolder(UnionHolder &&) = default;
        constexpr UnionHolder(const UnionHolder &) = default;
        constexpr UnionHolder &operator=(UnionHolder &&) = default;
        constexpr UnionHolder &operator=(const UnionHolder &) = default;
    };

    template <class T, bool = std::is_trivially_destructible_v<T>> struct Holder;

    template <class T> struct Holder<T, true> {
        constexpr Holder() = default;

        template <class U> constexpr Holder(U &&u) : value_(std::forward<U>(u)) {}

        template <class... H>
        constexpr Holder(in_place_index_t<0>, H &&...h) noexcept(std::is_nothrow_constructible_v<T, H...>)
                : value_(std::forward<H>(h)...) {}

        constexpr Holder(const Holder &) = default;

        constexpr Holder(Holder &&) = default;

        constexpr Holder &operator=(Holder &&) = default;
        constexpr Holder &operator=(const Holder &) = default;

        constexpr T &get() & { return value_; }

        constexpr const T &get() const & { return value_; }

        constexpr T &&get() && { return std::move(value_); }

        constexpr const T &&get() const && { return std::move(value_); }

    private:
        T value_;
    };

    template <class T> struct MemBuf {
        template <class... U> constexpr MemBuf(U &&...t) noexcept(std::is_nothrow_constructible_v<T, U...>) {
            static_assert(sizeof(value) == sizeof(T));
            ::new (&value) T(std::forward<U>(t)...);
        }

        T *get() { return static_cast<T *>(static_cast<void *>(&value)); }

        const T *get() const { return static_cast<const T *>(static_cast<const void *>(&value)); }

        alignas(alignof(T)) char value[sizeof(T)]{};
    };

    template <class T> struct Holder<T, false> {
        constexpr Holder() = default;

        template <class U> constexpr Holder(U &&u) : value_(std::forward<U>(u)) {}

        template <class... H>
        constexpr Holder(in_place_index_t<0>, H &&...h) noexcept(std::is_nothrow_constructible_v<T, H...>)
                : value_(std::forward<H>(h)...) {}

        T &get() & { return *value_.get(); }

        const T &get() const & { return *value_.get(); }

        T &&get() && { return std::move(*value_.get()); }

        const T &&get() const && { return std::move(*value_.get()); }

    private:
        MemBuf<T> value_;
    };

    template <class T> constexpr T &get(std::integral_constant<std::size_t, 0>, Holder<T> &v) { return v.get(); }

    template <class T> constexpr const T &get(std::integral_constant<std::size_t, 0>, const Holder<T> &v) {
        return v.get();
    }

    template <class T> constexpr T &&get(std::integral_constant<std::size_t, 0>, Holder<T> &&v) {
        return std::move(v).get();
    }

    template <class T> constexpr const T &&get(std::integral_constant<std::size_t, 0>, const Holder<T> &&v) {
        return std::move(v).get();
    }

    template <std::size_t I, class A, class B>
    constexpr auto &get(std::integral_constant<std::size_t, I>, UnionHolder<A, B> &h) {
        if constexpr (I == 0)
            return get(std::integral_constant<std::size_t, I>{}, h.first);
        else
            return get(std::integral_constant<std::size_t, I - 1>{}, h.second);
    }

    template <std::size_t I, class A, class B>
    const constexpr auto &get(std::integral_constant<std::size_t, I>, const UnionHolder<A, B> &h) {
        if constexpr (I == 0)
            return get(std::integral_constant<std::size_t, I>{}, h.first);
        else
            return get(std::integral_constant<std::size_t, I - 1>{}, h.second);
    }

    template <std::size_t I, class A, class B>
    constexpr decltype(auto) get(std::integral_constant<std::size_t, I>, UnionHolder<A, B> &&h) {
        if constexpr (I == 0)
            return get(std::integral_constant<std::size_t, I>{}, std::move(h.first));
        else
            return get(std::integral_constant<std::size_t, I - 1>{}, std::move(h.second));
    }

    template <std::size_t I, class A, class B>
    constexpr const auto &&get(std::integral_constant<std::size_t, I>, const UnionHolder<A, B> &&h) {
        if constexpr (I == 0)
            return get(std::integral_constant<std::size_t, I>{}, std::move(h.first));
        else
            return get(std::integral_constant<std::size_t, I - 1>{}, std::move(h.second));
    }

    template <class... Types> struct PackTypes;

    template <class T, class... Types> struct PackTypes<T, Types...> {
        using type = UnionHolder<Holder<T>, typename PackTypes<Types...>::type>;
    };

    template <class T> struct PackTypes<T> { using type = UnionHolder<Holder<T>, monostate>; };

    template <> struct PackTypes<> {};

    template <class T> struct ArrHelper { T x[1]; };

    template <class R, class T, class Tj, bool = std::is_same_v<std::remove_cv_t<T>, bool>, typename = void>
    struct Function {
        void apply();
    };

    template <class R, class Ti, class Tj>
    struct Function<R, Ti, Tj, false, std::void_t<decltype(ArrHelper<Ti>{{std::declval<Tj>()}})>> {
        static R apply(Ti);
    };

    template <class R, class Ti, class Tj>
    struct Function<R, Ti, Tj, true,
            std::enable_if_t<std::is_same_v<std::remove_cv_t<std::remove_reference_t<Tj>>, bool>>> {
        static R apply(Ti);
    };

    template <class... Ts> struct Overloaded : Ts... { using Ts::apply...; };

    template <class... Args> struct ChooseImpl;

    template <std::size_t... Indices, class T, class... Types>
    struct ChooseImpl<std::index_sequence<Indices...>, T, Types...>
            : Overloaded<Function<std::integral_constant<std::size_t, Indices>, Types, T>...> {
        using Overloaded<Function<std::integral_constant<std::size_t, Indices>, Types, T>...>::apply;
    };

    template <class... Args> struct ChooseType;

    template <class T, class... Args>
    struct ChooseType<T, Args...> : ChooseImpl<std::make_index_sequence<sizeof...(Args)>, T, Args...> {
        using ChooseImpl<std::make_index_sequence<sizeof...(Args)>, T, Args...>::apply;
    };

    template <class T, class... Types> using ChoosingType = decltype(ChooseType<T, Types...>::apply(std::declval<T>()));

    template <class T, class B, typename = void>
    struct OverloadedIndex : std::integral_constant<std::size_t, variant_npos> {};

    template <class T, class... Types>
    struct OverloadedIndex<T, Pack<Types...>, std::void_t<ChoosingType<T, Types...>>> : ChoosingType<T, Types...> {};

    template <class T, class... Types> constexpr auto OverloadIndex = OverloadedIndex<T, Pack<Types...>>::value;

    template <class T> struct IsInplace : std::false_type {};

    template <class T> struct IsInplace<in_place_type_t<T>> : std::true_type {};

    template <std::size_t I> struct IsInplace<in_place_index_t<I>> : std::true_type {};

    template <typename T, typename Tuple> struct TupleCount;

    template <typename T, typename Tuple> inline constexpr size_t TupleCount_v = TupleCount<T, Tuple>::value;

    template <typename T, typename... Types>
    struct TupleCount<T, std::tuple<Types...>> : std::integral_constant<size_t, 0> {};

    template <typename T, typename First, typename... Rest>
    struct TupleCount<T, std::tuple<First, Rest...>>
            : std::integral_constant<size_t, TupleCount_v<T, std::tuple<Rest...>> + std::is_same_v<T, First>> {};

    template <typename Tp, typename... Types>
    inline constexpr bool ExactlyOnce = TupleCount_v<Tp, std::tuple<Types...>> == 1;

    template <class... Types, class T> decltype(auto) variant_cast(T &&t) {
        if constexpr (std::is_lvalue_reference_v<T>) {
            if constexpr (std::is_const_v<std::remove_reference_t<T>>)
                return *static_cast<const variant<Types...> *>(static_cast<const void *>(&t));
            else
                return *static_cast<variant<Types...> *>(static_cast<void *>(&t));
        } else
            return std::move(*static_cast<variant<Types...> *>(static_cast<void *>(&t)));
    }

    template <class T, class... Types> struct Traits {
        static constexpr bool kDefaultConstructor = (std::is_default_constructible_v<T>);
        static constexpr bool kCopyConstructor =
                (std::is_copy_constructible_v<T> && ... && std::is_copy_constructible_v<Types>);
        static constexpr bool kMoveConstructor =
                (std::is_move_constructible_v<T> && ... && std::is_move_constructible_v<Types>);

        static constexpr bool kCopyAssign =
                kCopyConstructor && (std::is_copy_assignable_v<T> && ... && std::is_copy_assignable_v<Types>);
        static constexpr bool kMoveAssign =
                kMoveConstructor && (std::is_move_assignable_v<T> && ... && std::is_move_assignable_v<Types>);

        static constexpr bool kTrivialDtor =
                (std::is_trivially_destructible_v<Types> && ... && std::is_trivially_destructible_v<T>);
        static constexpr bool kTrivialMoveCtor =
                (std::is_trivially_move_constructible_v<Types> && ... && std::is_trivially_move_constructible_v<T>);
        static constexpr bool kTrivialCopyCtor =
                (std::is_trivially_copy_constructible_v<Types> && ... && std::is_trivially_copy_constructible_v<T>);
        static constexpr bool kTrivialMoveAssign =
                kTrivialDtor && kTrivialMoveCtor &&
                (std::is_trivially_move_assignable_v<Types> && ... && std::is_trivially_move_assignable_v<T>);
        static constexpr bool kTrivialCopyAssign =
                kTrivialDtor && kTrivialCopyCtor &&
                (std::is_trivially_copy_assignable_v<Types> && ... && std::is_trivially_copy_assignable_v<T>);

        static constexpr bool kNothrowDefaultCtor = std::is_nothrow_default_constructible_v<T>;
        static constexpr bool kNothrowCopyCtor = false;
        static constexpr bool kNothrowMoveCtor =
                (std::is_nothrow_move_constructible_v<Types> && ... && std::is_nothrow_move_constructible_v<T>);
        static constexpr bool kNothrowMoveAssign =
                kNothrowMoveCtor && (std::is_nothrow_move_assignable_v<Types> && ... && std::is_nothrow_move_assignable_v<T>);
        static constexpr bool kNothrowCopyAssign = false;
    };

    template <bool IsTriviallyMovable, class... Types> struct MoveAssignBase;

    template <bool IsTriviallyDestructible, class... Types> struct VariantStorage;

    template <class... Types> struct VariantStorage<false, Types...> {
        using ValueType = typename detail::PackTypes<Types...>::type;
        using IndexType = std::size_t;
        template <std::size_t I, typename = std::enable_if_t<(I < sizeof...(Types))>>
        using ToType = variant_alternative_t<I, VariantStorage>;
        using IndexSeq = std::make_index_sequence<sizeof...(Types)>;
        ValueType value_;
        IndexType index_{};

        constexpr VariantStorage() noexcept(std::is_nothrow_default_constructible_v<ValueType>) : index_(variant_npos){};

        constexpr VariantStorage(const VariantStorage &other) = default;

        template <class T, class... Args>
        constexpr explicit VariantStorage(in_place_type_t<T>, Args &&...args)
                : VariantStorage(in_place_index<detail::OverloadIndex<T, Types...>>, std::forward<Args>(args)...) {}

        template <class T, class U, class... Args>
        constexpr explicit VariantStorage(in_place_type_t<T>, std::initializer_list<U> il, Args &&...args)
                : VariantStorage(in_place_index<detail::OverloadIndex<T, Types...>>, il, std::forward<Args>(args)...) {}

        template <std::size_t I, class... Args>
        constexpr explicit VariantStorage(in_place_index_t<I>, Args &&...args)
                : value_(in_place_index<I>, std::forward<Args>(args)...), index_(I) {}

        template <std::size_t I, class U, class... Args>
        constexpr explicit VariantStorage(in_place_index_t<I>, std::initializer_list<U> il, Args &&...args)
                : value_(in_place_index<I>, il, std::forward<Args>(args)...), index_(I) {}

        constexpr VariantStorage &operator=(VariantStorage &&) = default;

        constexpr VariantStorage &operator=(const VariantStorage &rhs) = default;

        [[nodiscard]] constexpr bool IsValid() const noexcept { return index_ != IndexType(variant_npos); }

        template <std::size_t... Indices> void reset(std::index_sequence<Indices...>) {
            ((index_ == Indices
              ? (std::destroy_at(std::addressof(get(std::integral_constant<std::size_t, Indices>{}, value_))), 0)
              : 0),
                    ...);
        }

        void reset() {
            if (!IsValid())
                return;

            reset(std::make_index_sequence<sizeof...(Types)>{});

            index_ = variant_npos;
        }

        ~VariantStorage() { reset(); }
    };

    template <class... Types> struct VariantStorage<true, Types...> {
        using ValueType = typename detail::PackTypes<Types...>::type;
        using IndexType = std::size_t;
        template <std::size_t I, typename = std::enable_if_t<(I < sizeof...(Types))>>
        using ToType = variant_alternative_t<I, VariantStorage>;
        using IndexSeq = std::make_index_sequence<sizeof...(Types)>;
        ValueType value_;
        IndexType index_{};

        constexpr VariantStorage() noexcept(std::is_nothrow_default_constructible_v<ValueType>) : index_(variant_npos) {}

        constexpr VariantStorage(const VariantStorage &other) = default;

        constexpr VariantStorage(VariantStorage &&other) = default;

        template <class T, class... Args>
        constexpr explicit VariantStorage(in_place_type_t<T>, Args &&...args)
                : VariantStorage(in_place_index<detail::OverloadIndex<T, Types...>>, std::forward<Args>(args)...) {}

        template <class T, class U, class... Args>
        constexpr explicit VariantStorage(in_place_type_t<T>, std::initializer_list<U> il, Args &&...args)
                : VariantStorage(in_place_index<detail::OverloadIndex<T, Types...>>, il, std::forward<Args>(args)...) {}

        template <std::size_t I, class... Args>
        constexpr explicit VariantStorage(in_place_index_t<I>, Args &&...args)
                : value_(in_place_index<I>, std::forward<Args>(args)...), index_(I) {}

        template <std::size_t I, class U, class... Args>
        constexpr explicit VariantStorage(in_place_index_t<I>, std::initializer_list<U> il, Args &&...args)
                : value_(in_place_index<I>, il, std::forward<Args>(args)...), index_(I) {}

        constexpr VariantStorage &operator=(VariantStorage &&rhs) = default;

        constexpr VariantStorage &operator=(const VariantStorage &rhs) = default;

        [[nodiscard]] constexpr bool IsValid() const noexcept { return index_ != IndexType(variant_npos); }

        template <std::size_t... Indices> void reset(std::index_sequence<Indices...>) {
            ((index_ == Indices
              ? (std::destroy_at(std::addressof(get(std::integral_constant<std::size_t, Indices>{}, value_))), 0)
              : 0),
                    ...);
        }

        void reset() {
            if (!IsValid())
                return;

            reset(std::make_index_sequence<sizeof...(Types)>{});

            index_ = variant_npos;
        }
    };

    template <class... Types> using VariantStorageAlias = VariantStorage<Traits<Types...>::kTrivialDtor, Types...>;

    template <class... Types> using MoveAssignBaseAlias = MoveAssignBase<Traits<Types...>::kTrivialMoveAssign, Types...>;

    template <class... Types> struct MoveAssignBase<true, Types...> : VariantStorageAlias<Types...> {
        using Base = VariantStorageAlias<Types...>;
        using Base::Base;

        constexpr MoveAssignBase() = default;
        constexpr MoveAssignBase(MoveAssignBase const &) = default;
        constexpr MoveAssignBase(MoveAssignBase &&) = default;
        constexpr MoveAssignBase &operator=(MoveAssignBase &&) = default;
        constexpr MoveAssignBase &operator=(MoveAssignBase const &) = default;
    };

    template <class... Types> struct MoveAssignBase<false, Types...> : VariantStorageAlias<Types...> {
        using Base = VariantStorageAlias<Types...>;
        using Base::Base;

        template <std::size_t... Indices>
        constexpr void move_assign(std::index_sequence<Indices...> index_seq, MoveAssignBase &&rhs) {
            if (Base::index_ != rhs.index_) {
                Base::reset();
                ((rhs.index_ == Indices ? (variant_cast<Types...>(*this).template emplace<Indices>(std::move(
                        get(std::integral_constant<std::size_t, Indices>{}, std::move(rhs.value_)))),
                        0)
                                        : 0),
                        ...);
            } else {
                ((rhs.index_ == Indices ? (get(std::integral_constant<std::size_t, Indices>{}, Base::value_) = std::move(
                        get(std::integral_constant<std::size_t, Indices>{}, std::move(rhs.value_))),
                        0)
                                        : 0),
                        ...);
            }
            Base::index_ = rhs.index_;
        }

        constexpr MoveAssignBase() = default;
        constexpr MoveAssignBase(MoveAssignBase const &) = default;
        constexpr MoveAssignBase(MoveAssignBase &&rhs) = default;
        constexpr MoveAssignBase &operator=(MoveAssignBase &&rhs) {
            if (!Base::IsValid() && !rhs.IsValid()) {
                return *this;
            } else if (!rhs.IsValid()) {
                Base::reset();
            } else {
                move_assign(std::make_index_sequence<sizeof...(Types)>{}, std::move(rhs));
            }
            return *this;
        }
        constexpr MoveAssignBase &operator=(MoveAssignBase const &) = default;
    };

    template <bool, class... Types> struct MoveCtorBase;

    template <class... Types> using MoveCtorBaseAlias = MoveCtorBase<Traits<Types...>::kTrivialMoveCtor, Types...>;

    template <class... Types> struct MoveCtorBase<true, Types...> : MoveAssignBaseAlias<Types...> {
        using Base = MoveAssignBaseAlias<Types...>;
        using Base::Base;

        constexpr MoveCtorBase() = default;
        constexpr MoveCtorBase(MoveCtorBase const &) = default;
        constexpr MoveCtorBase(MoveCtorBase &&) = default;
        constexpr MoveCtorBase &operator=(MoveCtorBase &&) = default;
        constexpr MoveCtorBase &operator=(MoveCtorBase const &) = default;
    };

    template <class... Types> struct MoveCtorBase<false, Types...> : MoveAssignBaseAlias<Types...> {
        using Base = MoveAssignBaseAlias<Types...>;
        using Base::Base;

        template <std::size_t... Indices, class T> void ConstructVariant(std::index_sequence<Indices...> ind, T &&rhs) {
            using Type = std::remove_reference_t<decltype(rhs.value_)>;

            ((std::forward<T>(rhs).index_ == Indices
              ? (::new (std::addressof(Base::value_))
                            Type(in_place_index<Indices>,
                                 get(std::integral_constant<std::size_t, Indices>{}, std::forward<T>(rhs).value_)),
                            0)
              : 0),
                    ...);
        }

        constexpr MoveCtorBase() = default;
        constexpr MoveCtorBase(MoveCtorBase const &) = default;
        constexpr MoveCtorBase(MoveCtorBase &&other) noexcept((std::is_nothrow_move_constructible_v<Types> && ...)) {
            VariantStorageAlias<Types...>::index_ = other.index_;
            ConstructVariant(typename Base::IndexSeq{}, std::move(other));
        }
        constexpr MoveCtorBase &operator=(MoveCtorBase &&) = default;
        constexpr MoveCtorBase &operator=(MoveCtorBase const &) = default;
    };

    template <bool, class... Types> struct CopyAssignBase;

    template <class... Types> using CopyAssignBaseAlias = CopyAssignBase<Traits<Types...>::kTrivialCopyAssign, Types...>;

    template <class... Types> struct CopyAssignBase<true, Types...> : MoveCtorBaseAlias<Types...> {
        using Base = MoveCtorBaseAlias<Types...>;
        using Base::Base;

        constexpr CopyAssignBase() = default;
        constexpr CopyAssignBase(CopyAssignBase const &) = default;
        constexpr CopyAssignBase(CopyAssignBase &&) = default;
        constexpr CopyAssignBase &operator=(CopyAssignBase &&) = default;
        constexpr CopyAssignBase &operator=(CopyAssignBase const &) = default;
    };

    template <class... Types> struct CopyAssignBase<false, Types...> : MoveCtorBaseAlias<Types...> {
        using Base = MoveCtorBaseAlias<Types...>;
        using VariantBase = VariantStorageAlias<Types...>;
        using Base::Base;

        template <std::size_t... Indices>
        constexpr void assign(std::index_sequence<Indices...> index_seq, const CopyAssignBase &rhs) {
            if (VariantBase::index_ != rhs.index_) {
                VariantBase::reset();
                ((rhs.index_ == Indices ? (variant_cast<Types...>(*this).template emplace<Indices>(
                        get(std::integral_constant<std::size_t, Indices>{}, rhs.value_)),
                        0)
                                        : 0),
                        ...);
            } else {
                ((rhs.index_ == Indices ? (get(std::integral_constant<std::size_t, Indices>{}, VariantBase::value_) =
                                                   get(std::integral_constant<std::size_t, Indices>{}, rhs.value_),
                        0)
                                        : 0),
                        ...);
            }
            VariantBase::index_ = rhs.index_;
        }

        constexpr CopyAssignBase() = default;
        constexpr CopyAssignBase(CopyAssignBase const &) = default;
        constexpr CopyAssignBase(CopyAssignBase &&other) = default;
        constexpr CopyAssignBase &operator=(CopyAssignBase &&) = default;
        constexpr CopyAssignBase &operator=(const CopyAssignBase &rhs) {
            if (!VariantBase::IsValid()) {
                return *this;
            }

            assign(std::make_index_sequence<sizeof...(Types)>{}, rhs);

            return *this;
        }
    };

    template <bool, class... Types> struct CopyCtorBase;

    template <class... Types> using CopyCtorBaseAlias = CopyCtorBase<Traits<Types...>::kTrivialCopyCtor, Types...>;

    template <class... Types> struct CopyCtorBase<true, Types...> : CopyAssignBaseAlias<Types...> {
        using Base = CopyAssignBaseAlias<Types...>;
        using Base::Base;

        constexpr CopyCtorBase() = default;
        constexpr CopyCtorBase(CopyCtorBase const &) = default;
        constexpr CopyCtorBase(CopyCtorBase &&) = default;
        constexpr CopyCtorBase &operator=(CopyCtorBase &&) = default;
        constexpr CopyCtorBase &operator=(CopyCtorBase const &) = default;
    };

    template <class... Types> struct CopyCtorBase<false, Types...> : CopyAssignBaseAlias<Types...> {
        template <class..., class T> friend decltype(auto) variant_cast(T &&t);
        using Base = CopyAssignBaseAlias<Types...>;
        using VariantBase = VariantStorageAlias<Types...>;
        using Base::Base;

        template <std::size_t... Indices>
        constexpr void assign(std::index_sequence<Indices...> index_seq, const CopyCtorBase &rhs) {
            if (VariantBase::index_ != rhs.index_) {
                VariantBase::reset();
                ((rhs.index_ == Indices ? (variant_cast<Types...>(*this).template emplace<Indices>(
                        get(std::integral_constant<std::size_t, Indices>{}, rhs.value_)),
                        0)
                                        : 0),
                        ...);
            } else {
                ((rhs.index_ == Indices ? (get(std::integral_constant<std::size_t, Indices>{}, VariantBase::value_) =
                                                   get(std::integral_constant<std::size_t, Indices>{}, rhs.value_),
                        0)
                                        : 0),
                        ...);
            }
            VariantBase::index_ = rhs.index_;
        }

        constexpr CopyCtorBase() = default;
        constexpr CopyCtorBase(CopyCtorBase const &rhs) { assign(std::make_index_sequence<sizeof...(Types)>{}, rhs); }
        constexpr CopyCtorBase(CopyCtorBase &&other) = default;
        constexpr CopyCtorBase &operator=(CopyCtorBase &&) = default;
        constexpr CopyCtorBase &operator=(const CopyCtorBase &rhs) = default;
    };

    template <class... Types> struct VariantBase : CopyCtorBaseAlias<Types...> {
        using Base = CopyCtorBaseAlias<Types...>;

        constexpr VariantBase() noexcept(Traits<Types...>::kNothrowDefaultCtor) : VariantBase(in_place_index<0>) {}

        template <size_t Np, typename... Args>
        constexpr explicit VariantBase(in_place_index_t<Np> i, Args &&...args) : Base(i, std::forward<Args>(args)...) {}

        VariantBase(const VariantBase &) = default;
        VariantBase(VariantBase &&) = default;
        VariantBase &operator=(const VariantBase &) = default;
        VariantBase &operator=(VariantBase &&) = default;
    };

    struct Tag {};

    template <bool> struct EnableDefaultConstructor {
        constexpr EnableDefaultConstructor() noexcept = delete;
        constexpr EnableDefaultConstructor(EnableDefaultConstructor const &) noexcept = default;
        constexpr EnableDefaultConstructor(EnableDefaultConstructor &&) noexcept = default;
        EnableDefaultConstructor &operator=(EnableDefaultConstructor const &) noexcept = default;
        EnableDefaultConstructor &operator=(EnableDefaultConstructor &&) noexcept = default;

        constexpr EnableDefaultConstructor(Tag){};
    };

    template <> struct EnableDefaultConstructor<true> {
        constexpr EnableDefaultConstructor() noexcept = default;
        constexpr EnableDefaultConstructor(EnableDefaultConstructor const &) noexcept = default;
        constexpr EnableDefaultConstructor(EnableDefaultConstructor &&) noexcept = default;
        EnableDefaultConstructor &operator=(EnableDefaultConstructor const &) noexcept = default;
        EnableDefaultConstructor &operator=(EnableDefaultConstructor &&) noexcept = default;

        constexpr EnableDefaultConstructor(Tag){};
    };

    template <bool> struct EnableCopyConstructor {
        constexpr EnableCopyConstructor() noexcept = default;
        constexpr EnableCopyConstructor(EnableCopyConstructor const &) noexcept = delete;
        constexpr EnableCopyConstructor(EnableCopyConstructor &&) noexcept = default;
        EnableCopyConstructor &operator=(EnableCopyConstructor const &) noexcept = default;
        EnableCopyConstructor &operator=(EnableCopyConstructor &&) noexcept = default;
    };

    template <> struct EnableCopyConstructor<true> {
        constexpr EnableCopyConstructor() noexcept = default;
        constexpr EnableCopyConstructor(EnableCopyConstructor const &) noexcept = default;
        constexpr EnableCopyConstructor(EnableCopyConstructor &&) noexcept = default;
        EnableCopyConstructor &operator=(EnableCopyConstructor const &) noexcept = default;
        EnableCopyConstructor &operator=(EnableCopyConstructor &&) noexcept = default;
    };

    template <bool> struct EnableMoveConstructor {
        constexpr EnableMoveConstructor() noexcept = default;
        constexpr EnableMoveConstructor(EnableMoveConstructor const &) noexcept = default;
        constexpr EnableMoveConstructor(EnableMoveConstructor &&) noexcept = delete;
        EnableMoveConstructor &operator=(EnableMoveConstructor const &) noexcept = default;
        EnableMoveConstructor &operator=(EnableMoveConstructor &&) noexcept = default;
    };

    template <> struct EnableMoveConstructor<true> {
        constexpr EnableMoveConstructor() noexcept = default;
        constexpr EnableMoveConstructor(EnableMoveConstructor const &) noexcept = default;
        constexpr EnableMoveConstructor(EnableMoveConstructor &&) noexcept = default;
        EnableMoveConstructor &operator=(EnableMoveConstructor const &) noexcept = default;
        EnableMoveConstructor &operator=(EnableMoveConstructor &&) noexcept = default;
    };

    template <bool> struct EnableCopyAssign {
        constexpr EnableCopyAssign() noexcept = default;
        constexpr EnableCopyAssign(EnableCopyAssign const &) noexcept = default;
        constexpr EnableCopyAssign(EnableCopyAssign &&) noexcept = default;
        EnableCopyAssign &operator=(EnableCopyAssign const &) noexcept = delete;
        EnableCopyAssign &operator=(EnableCopyAssign &&) noexcept = default;
    };

    template <> struct EnableCopyAssign<true> {
        constexpr EnableCopyAssign() noexcept = default;
        constexpr EnableCopyAssign(EnableCopyAssign const &) noexcept = default;
        constexpr EnableCopyAssign(EnableCopyAssign &&) noexcept = default;
        EnableCopyAssign &operator=(EnableCopyAssign const &) noexcept = default;
        EnableCopyAssign &operator=(EnableCopyAssign &&) noexcept = default;
    };

    template <bool> struct EnableMoveAssign {
        constexpr EnableMoveAssign() noexcept = default;
        constexpr EnableMoveAssign(EnableMoveAssign const &) noexcept = default;
        constexpr EnableMoveAssign(EnableMoveAssign &&) noexcept = default;
        EnableMoveAssign &operator=(EnableMoveAssign const &) noexcept = delete;
        EnableMoveAssign &operator=(EnableMoveAssign &&) noexcept = delete;
    };

    template <> struct EnableMoveAssign<true> {
        constexpr EnableMoveAssign() noexcept = default;
        constexpr EnableMoveAssign(EnableMoveAssign const &) noexcept = default;
        constexpr EnableMoveAssign(EnableMoveAssign &&) noexcept = default;
        EnableMoveAssign &operator=(EnableMoveAssign const &) noexcept = default;
        EnableMoveAssign &operator=(EnableMoveAssign &&) noexcept = default;
    };
} // namespace detail

template <std::size_t I, class... Types>
constexpr variant_alternative_t<I, variant<Types...>> &get(variant<Types...> &v);

template <class... Types>
class variant : detail::VariantBase<Types...>,
                detail::EnableDefaultConstructor<detail::Traits<Types...>::kDefaultConstructor>,
                detail::EnableCopyConstructor<detail::Traits<Types...>::kCopyConstructor>,
                detail::EnableMoveConstructor<detail::Traits<Types...>::kMoveConstructor>,
                detail::EnableCopyAssign<detail::Traits<Types...>::kCopyAssign>,
                detail::EnableMoveAssign<detail::Traits<Types...>::kMoveAssign> {
    using Base = detail::VariantBase<Types...>;
    using EnableDefaultCtor = detail::EnableDefaultConstructor<detail::Traits<Types...>::kDefaultConstructor>;
    template <std::size_t I, typename = std::enable_if_t<(I < sizeof...(Types))>>
    using ToType = variant_alternative_t<I, variant>;
    using Traits = detail::Traits<Types...>;

    template <size_t Np, typename Variant, typename... Args>
    static void ConstructByIndex(Variant &v, Args &&...args) {
        v.reset();
        try {
            v.index_ = Np;
            auto &&storage = get<Np>(v);
            ::new ((void *)std::addressof(storage)) std::remove_reference_t<decltype(storage)>(std::forward<Args>(args)...);
        } catch (...) {
            using index_type = decltype(v.index_);
            v.index_ = static_cast<index_type>(variant_npos);
            throw;
        }
    }

public:
    constexpr variant() noexcept(Traits::kNothrowDefaultCtor) = default;

    constexpr variant(const variant &other) noexcept(Traits::kNothrowCopyCtor) = default;

    constexpr variant(variant &&other) noexcept(Traits::kNothrowMoveCtor) = default;

    template <class T, typename = std::enable_if_t<(sizeof...(Types) > 0)>,
            typename = std::enable_if_t<!detail::IsInplace<T>::value>,
            std::size_t Tj_id = detail::OverloadIndex<T, Types...>, typename Tj = ToType<Tj_id>,
            typename = std::enable_if_t<detail::ExactlyOnce<Tj, Types...>>,
            typename = std::enable_if_t<std::is_constructible_v<Tj, T>>>
    constexpr variant(T &&t) noexcept(std::is_nothrow_constructible_v<Tj, T>)
            : Base(in_place_index<Tj_id>, std::forward<T>(t)), EnableDefaultCtor(detail::Tag{}) {}

    template <class T, class... Args,
            typename = std::enable_if_t<detail::ExactlyOnce<T, Types...> && std::is_constructible_v<T, Args...>>>
    constexpr explicit variant(in_place_type_t<T>, Args &&...args)
            : Base(in_place_index<detail::OverloadIndex<T, Types...>>, std::forward<Args>(args)...),
              EnableDefaultCtor(detail::Tag{}) {}

    template <class T, class U, class... Args,
            typename = std::enable_if_t<detail::ExactlyOnce<T> &&
                                        std::is_constructible_v<T, std::initializer_list<U> &, Args...>>>
    constexpr explicit variant(in_place_type_t<T>, std::initializer_list<U> il, Args &&...args)
            : Base(in_place_index<detail::OverloadIndex<T, Types...>>, il, std::forward<Args>(args)...),
              EnableDefaultCtor(detail::Tag{}) {}

    template <std::size_t I, class... Args, typename = std::enable_if_t<(I < sizeof...(Types))>, typename T = ToType<I>,
            typename = std::enable_if_t<std::is_constructible_v<T, Args...>>>
    constexpr explicit variant(in_place_index_t<I>, Args &&...args)
            : Base(in_place_index<I>, std::forward<Args>(args)...), EnableDefaultCtor(detail::Tag{}) {}

    template <std::size_t I, class U, class... Args, typename = std::enable_if_t<(I < sizeof...(Types))>,
            typename T = ToType<I>,
            typename = std::enable_if_t<std::is_constructible_v<T, std::initializer_list<U> &, Args...>>>
    constexpr explicit variant(in_place_index_t<I>, std::initializer_list<U> il, Args &&...args)
            : Base(in_place_index<I>, il, std::forward<Args>(args)...), EnableDefaultCtor(detail::Tag{}) {}

    constexpr variant &operator=(const variant &rhs) noexcept(Traits::kNothrowCopyAssign) = default;
    constexpr variant &operator=(variant &&rhs) noexcept(Traits::kNothrowMoveAssign) = default;

    template <typename T>
    std::enable_if_t<detail::ExactlyOnce<ToType<detail::OverloadIndex<T &&, Types...>>, Types...> &&
                     std::is_constructible_v<ToType<detail::OverloadIndex<T &&, Types...>>, T> &&
                     std::is_assignable_v<ToType<detail::OverloadIndex<T &&, Types...>> &, T>,
            variant &>
    operator=(T &&rhs) noexcept(std::is_nothrow_assignable_v<ToType<detail::OverloadIndex<T &&, Types...>> &, T>
                                &&std::is_nothrow_constructible_v<ToType<detail::OverloadIndex<T &&, Types...>>, T>) {
        constexpr auto index = detail::OverloadIndex<T, Types...>;
        if (this->index() == index)
            get<index>(*this) = std::forward<T>(rhs);
        else {
            using Tj = ToType<detail::OverloadIndex<T &&, Types...>>;
            if constexpr (std::is_nothrow_constructible_v<Tj, T> || !std::is_nothrow_move_constructible_v<Tj>)
                this->emplace<index>(std::forward<T>(rhs));
            else
                operator=(variant(std::forward<T>(rhs)));
        }
        return *this;
    }

    template <typename Tp, typename... Args>
    std::enable_if_t<std::is_constructible_v<Tp, Args...> && detail::ExactlyOnce<Tp, Types...>, Tp &>
    emplace(Args &&...args) {
        constexpr size_t index = detail::IndexOf_v<Tp, Types...>;
        return this->emplace<index>(std::forward<Args>(args)...);
    }

    template <typename Tp, typename Up, typename... Args>
    std::enable_if_t<
            std::is_constructible_v<Tp, std::initializer_list<Up> &, Args...> && detail::ExactlyOnce<Tp, Types...>, Tp &>
    emplace(std::initializer_list<Up> il, Args &&...args) {
        constexpr size_t index = detail::IndexOf_v<Tp, Types...>;
        return this->emplace<index>(il, std::forward<Args>(args)...);
    }

    template <size_t Np, typename... Args>
    std::enable_if_t<std::is_constructible_v<variant_alternative_t<Np, variant>, Args...>,
            variant_alternative_t<Np, variant> &>
    emplace(Args &&...args) {
        static_assert(Np < sizeof...(Types), "The index must be in [0, number of alternatives)");
        using type = variant_alternative_t<Np, variant>;
        if constexpr (std::is_scalar_v<type>) {
            const type tmp(std::forward<Args>(args)...);
            ConstructByIndex<Np>(*this, tmp);
        } else {
            ConstructByIndex<Np>(*this, std::forward<Args>(args)...);
        }
        return get<Np>(*this);
    }

    template <std::size_t... Indices> void swap(std::index_sequence<Indices...>, variant &rhs) {

        if (Base::index_ == rhs.index_) {
            using std::swap;
            ((Base::index_ == Indices ? (swap(get(std::integral_constant<std::size_t, Indices>{}, Base::value_),
                                              get(std::integral_constant<std::size_t, Indices>{}, rhs.value_)),
                    0)
                                      : 0),
                    ...);
            std::swap(Base::index_, rhs.index_);
        } else if (!rhs.IsValid()) {
            rhs = std::move(*this);
            rhs.index_ = Base::index_;
            Base::reset();
        } else if (!Base::IsValid()) {
            *this = std::move(rhs);
            rhs.reset();
        } else {
            auto tmp(std::move(rhs));
            rhs = std::move(*this);
            *this = std::move(tmp);
        }
    }

    void swap(variant &rhs) {
        if ((!Base::IsValid() && !rhs.IsValid()) || this == std::addressof(rhs)) {
            return;
        }

        swap(std::make_index_sequence<sizeof...(Types)>{}, rhs);
    }

    [[nodiscard]] constexpr bool valueless_by_exception() const noexcept { return !this->IsValid(); }
    [[nodiscard]] constexpr std::size_t index() const noexcept { return Base::index_; }

    template <std::size_t I, class... Args>
    friend constexpr variant_alternative_t<I, variant<Args...>> &get(variant<Args...> &);
};

class bad_variant_access : public std::exception {
public:
    bad_variant_access() noexcept = default;

    [[nodiscard]] const char *what() const noexcept override { return reason; }

    explicit bad_variant_access(const char *reason) noexcept : reason(reason) {}

private:
    // Must point to a string with static storage duration:
    const char *reason = "bad variant access";
};

template <class T, class... Types> constexpr bool holds_alternative(const variant<Types...> &v) noexcept {
    return detail::IndexOf_v<T, Types...> == v.index();
}

template <std::size_t I, class... Types>
constexpr variant_alternative_t<I, variant<Types...>> &get(variant<Types...> &v) {
    if (I != v.index()) {
        throw bad_variant_access("illegal get");
    }
    return detail::get(std::integral_constant<std::size_t, I>{}, v.value_);
}

template <std::size_t I, class... Types>
constexpr const variant_alternative_t<I, variant<Types...>> &get(const variant<Types...> &v) {
    if (I != v.index()) {
        throw bad_variant_access("illegal get");
    }
    return detail::get(std::integral_constant<std::size_t, I>{}, v.value_);
}

template <std::size_t I, class... Types>
constexpr const variant_alternative_t<I, variant<Types...>> &&get(const variant<Types...> &&v) {
    if (I != v.index()) {
        throw bad_variant_access("illegal get");
    }
    return detail::get(std::integral_constant<std::size_t, I>{}, std::move(v.value_));
}

template <std::size_t I, class... Types>
constexpr variant_alternative_t<I, variant<Types...>> &&get(variant<Types...> &&v) {
    if (I != v.index()) {
        throw bad_variant_access("illegal get");
    }
    return detail::get(std::integral_constant<std::size_t, I>{}, std::move(v.value_));
}

template <typename Tp, typename... Types> constexpr Tp &get(variant<Types...> &v) {
    static_assert(detail::ExactlyOnce<Tp, Types...>, "T must occur exactly once in alternatives");
    static_assert(!std::is_void_v<Tp>, "Tp must not be void");
    return get<detail::IndexOf_v<Tp, Types...>>(v);
}

template <typename Tp, typename... Types> constexpr const Tp &get(const variant<Types...> &v) {
    static_assert(detail::ExactlyOnce<Tp, Types...>, "T must occur exactly once in alternatives");
    static_assert(!std::is_void_v<Tp>, "Tp must not be void");
    return get<detail::IndexOf_v<Tp, Types...>>(v);
}

template <typename Tp, typename... Types> constexpr Tp &&get(variant<Types...> &&v) {
    static_assert(detail::ExactlyOnce<Tp, Types...>, "T must occur exactly once in alternatives");
    static_assert(!std::is_void_v<Tp>, "Tp must not be void");
    return get<detail::IndexOf_v<Tp, Types...>>(std::move(v));
}

template <typename Tp, typename... Types> constexpr const Tp &&get(const variant<Types...> &&v) {
    static_assert(detail::ExactlyOnce<Tp, Types...>, "T must occur exactly once in alternatives");
    static_assert(!std::is_void_v<Tp>, "Tp must not be void");
    return get<detail::IndexOf_v<Tp, Types...>>(std::move(v));
}

template <size_t Np, typename... Types>
constexpr std::add_pointer_t<variant_alternative_t<Np, variant<Types...>>> get_if(variant<Types...> *ptr) noexcept {
    using AlternativeType = variant_alternative_t<Np, variant<Types...>>;
    static_assert(Np < sizeof...(Types), "The index must be in [0, number of alternatives)");
    static_assert(!std::is_void_v<AlternativeType>, "_Tp must not be void");
    if (ptr && ptr->index() == Np)
        return std::addressof(get<Np>(*ptr));
    return nullptr;
}

template <size_t Np, typename... Types>
constexpr std::add_pointer_t<const variant_alternative_t<Np, variant<Types...>>>
get_if(const variant<Types...> *ptr) noexcept {
    using AlternativeType = variant_alternative_t<Np, variant<Types...>>;
    static_assert(Np < sizeof...(Types), "The index must be in [0, number of alternatives)");
    static_assert(!std::is_void_v<AlternativeType>, "_Tp must not be void");
    if (ptr && ptr->index() == Np)
        return std::addressof(get<Np>(*ptr));
    return nullptr;
}

template <typename T, typename... Types> constexpr std::add_pointer_t<T> get_if(variant<Types...> *ptr) noexcept {
    static_assert(detail::ExactlyOnce<T, Types...>, "T must occur exactly once in alternatives");
    static_assert(!std::is_void_v<T>, "T must not be void");
    return get_if<detail::IndexOf_v<T, Types...>>(ptr);
}

template <typename T, typename... Types>
constexpr std::add_pointer_t<const T> get_if(const variant<Types...> *ptr) noexcept {
    static_assert(detail::ExactlyOnce<T, Types...>, "T must occur exactly once in alternatives");
    static_assert(!std::is_void_v<T>, "T must not be void");
    return get_if<detail::IndexOf_v<T, Types...>>(ptr);
}

template <typename T> constexpr T &&at_impl(T &&elem) { return std::forward<T>(elem); }

template <typename T, typename... Is> constexpr auto &&at_impl(T &&elems, std::size_t i, Is... is) {
    return at_impl(std::forward<T>(elems)[i], is...);
}

template <typename T, typename... Is> constexpr auto &&at(T &&elems, Is... is) {
    return at_impl(std::forward<T>(elems), is...);
}

template <typename Func, typename... Variants, std::size_t... Is>
constexpr decltype(auto) make_vtable(std::index_sequence<Is...> seq) {
    struct dispatcher {
        static constexpr decltype(auto) dispatch(Func f, Variants... vs) {
            return static_cast<Func>(f)(get<Is>(static_cast<Variants>(vs))...);
        }
    };
    return &dispatcher::dispatch;

}



template<typename Dest, typename... Types>
struct make_array_elem
{
    using type = Dest;
};

template<typename... Types>
struct make_array_elem<void, Types...> : std::common_type<Types...> {
    template <typename>
    struct is_reference_wrapper : std::false_type {};

    template <typename Up>
    struct is_reference_wrapper<std::reference_wrapper<Up>> : std::true_type {};
};


template <typename Dest = void, typename... Types>
constexpr std::array<typename make_array_elem<Dest, Types...>::type, sizeof...(Types)> make_array(Types&&... t) {
    return {{ std::forward<Types>(t)... }};
}

template <typename F, typename... Vs, std::size_t... Is, std::size_t... Js, typename... Ls>
constexpr auto make_vtable(std::index_sequence<Is...>, std::index_sequence<Js...>, Ls... ls) {
    return make_array(make_vtable<F, Vs...>(std::index_sequence<Is..., Js>{}, ls...)...);
}

template <typename F, typename... Variants> constexpr auto create_vtable() {
    return make_vtable<F, Variants...>(std::index_sequence<>{},
                                       std::make_index_sequence<variant_size_v<std::decay_t<Variants>>>{}...);
}

template <typename Visitor, typename... Variants>
constexpr decltype(auto) visit(Visitor &&visitor, Variants &&...variants) {
    if ((variants.valueless_by_exception() || ...)) {
        throw bad_variant_access();
    }
    constexpr auto vtable = create_vtable<Visitor &&, Variants &&...>();
    return at(vtable, variants.index()...)(std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
}
