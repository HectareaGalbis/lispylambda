
#pragma once

#include <type_traits>

template <typename T>
struct debug { };

/// tpp
template <typename T>
using tpp = typename T::template tpp_return<>;

template <typename T>
using tpp_t = typename tpp<T>::type;

template <typename T>
extern constexpr auto tpp_v = tpp<T>::value;

template <typename T>
extern constexpr auto tpp_vt = tpp<T>::value_type;

/// returns
#define returns            \
    template <typename...> \
    using tpp_return =

/// box
template<typename T>
struct __box {
    returns T;
};

/// unbox
template <typename T>
struct __unbox {
    using type = T;
};

template<typename T>
struct __unbox<__box<T>> {
    using type = T;
};

template<typename T>
using __unbox_t = typename __unbox<T>::type;

/// quote
template <typename T>
struct q {
    returns T;
};

/// unquote
template <typename T>
using u = tpp<T>;

/// v
template <auto b>
struct v {
    using type = v;
    using value_type = decltype(b);
    static constexpr auto value = b;

    returns v;
};

/// is_value
template <typename T>
struct is_value : std::false_type { };

template <auto b>
struct is_value<v<b>> : std::true_type { };

/// Value
template <typename T>
concept Value = is_value<__unbox_t<T>>::value;

/// Arithmetic
template <typename T>
concept Arithmetic = Value<T> && std::is_arithmetic_v<typename __unbox_t<T>::value_type>;

/// Int
template <typename T>
concept Int = Value<T> && std::is_integral_v<typename __unbox_t<T>::value_type>;

/// UInt
template <typename T>
concept UInt = Value<T> && std::is_integral_v<typename __unbox_t<T>::value_type> && __unbox_t<T>::value >= 0;

/// t
template<typename T>
struct t {
    using type = t;
    using value_type = T;

    returns t;
};

/// is_type
template <typename T>
struct is_type : std::false_type { };

template <typename T>
struct is_type<t<T>> : std::true_type { };

/// Type
template <typename T>
concept Type = is_type<__unbox_t<T>>::value;

/// f
template <template <typename...> typename F, typename... AS>
struct f {
    using type = f;

    returns f;
};

/// is_function
template <typename T>
struct is_function : std::false_type { };

template <template <typename...> typename T>
struct is_function<f<T>> : std::true_type { };

/// Function
template <typename T>
concept Function = is_function<__unbox_t<T>>::value;

/// funcall and defun
template <typename F, typename... AS>
struct __funcall { };

template <template <typename...> typename F, typename... AS, typename... FS>
struct __funcall<f<F, AS...>, FS...> {
    returns tpp<tpp<F<__box<tpp<AS>>..., __box<tpp<FS>>...>>>;
};

#define defun(name, ...)                                    \
    template <__VA_ARGS__>                                  \
    struct ____##name##____;                                \
                                                            \
    template <typename... TS>                               \
    struct name {                                           \
        returns tpp<__funcall<f<____##name##____>, TS...>>; \
    };                                                      \
                                                            \
    template <__VA_ARGS__>                                  \
    struct ____##name##____

defun(funcall, Function F, typename... AS)
{
    returns __funcall<tpp<F>, tpp<AS>...>;
};

/// expand and defmacro
template <typename F, typename... AS>
struct __expand { };

template <template <typename...> typename F, typename... FS>
struct __expand<f<F>, FS...> {
    returns tpp<tpp<F<__box<FS>...>>>;
};

#define defmacro(name, ...)                                   \
    template <__VA_ARGS__>                                    \
    struct ____##name##____;                                  \
                                                              \
    template <typename... TS>                                 \
    struct name {                                             \
        template <typename... LS>                             \
        using __private_func = ____##name##____<LS...>;       \
        returns tpp<tpp<__expand<f<__private_func>, TS...>>>; \
    };                                                        \
                                                              \
    template <__VA_ARGS__>                                    \
    struct ____##name##____

/// macroexpand
template <typename T>
struct macroexpand_aux {
    using type = T;
};

template <template <typename...> typename F, typename... FS>
struct macroexpand_aux<F<FS...>> {
    using type = __expand<f<F<FS...>::template __private_func>, FS...>;
};

defun(macroexpand, typename T){
    returns typename macroexpand_aux<tpp<T>>::type;
};

/// defvar
#define defvar(name, type) \
    using name = __box<tpp<type>>;

/// deftype
#define deftype(name, cstr, ...)                    \
    template <__VA_ARGS__>                          \
    struct cstr {                                   \
        using type = cstr;                          \
        returns cstr;                               \
    };                                              \
                                                    \
    template <typename T>                           \
    struct is_##name : std::false_type { };         \
                                                    \
    template <typename T>                           \
    struct is_##name<cstr<T>> : std::true_type { }; \
                                                    \
    template <typename T>                           \
    concept name = is_##name<__unbox_t<T>>::value

/// list
template <typename... TS>
struct __l {
    using type = __l;
    using value_type = bool;
    static constexpr bool value = false;

    returns __l;
};

template <typename T, typename... TS>
struct __l<T, TS...> {
    using type = __l;

    returns __l;
};

template <typename T>
struct is_list : std::false_type { };

template <typename... T>
struct is_list<__l<T...>> : std::true_type { };

template <typename T>
concept List = is_list<__unbox_t<T>>::value;

defun(list, typename... TS)
{
    returns __l<tpp<TS>...>;
};

/// nil
using nil = __l<>;

/// False
using False = nil;

/// T
struct True {
    using type = True;
    using value_type = bool;
    static constexpr bool value = true;

    returns True;
};

/// is_bool
template <typename T>
struct is_bool : std::false_type { };

template <>
struct is_bool<False> : std::true_type { };

template <>
struct is_bool<True> : std::false_type { };

/// Bool
template <typename T>
concept Bool = is_bool<__unbox_t<T>>::value;

/// branch
template <typename C, typename T, typename E>
struct b_aux {
    using type = tpp<T>;
};

template <typename T, typename E>
struct b_aux<False, T, E> {
    using type = tpp<E>;
};

template <typename C, typename T, typename E>
struct b {
    returns typename b_aux<tpp<C>, T, E>::type;
};

/// car
template <typename L>
struct __car {
    using type = nil;
};

template <typename T, typename... TS>
struct __car<__l<T, TS...>> {
    using type = T;
};

defun(car, List L)
{
    returns typename __car<tpp<L>>::type;
};

/// cdr
template <typename L>
struct __cdr {
    using type = nil;
};

template <typename T, typename... TS>
struct __cdr<__l<T, TS...>> {
    using type = __l<TS...>;
};

defun(cdr, List L)
{
    returns typename __cdr<tpp<L>>::tyoe;
};

/// cons
template <typename T, typename L>
struct __cons {};

template <typename T, typename... TS>
struct __cons<T,__l<TS...>> {
    using type = __l<T, TS...>;
};

defun(cons, typename T, List L)
{
    returns typename __cons<tpp<T>, tpp<L>>::type;
};


/// length
template <typename L>
struct __length { };

template <typename... TS>
struct __length<__l<TS...>> {
    using type = v<sizeof...(TS)>;
};

defun(length, List L)
{
    returns typename __length<tpp<L>>::type;
};

/// null
template <typename L>
struct __nullp {
    using type = False;
};

template <>
struct __nullp<nil> {
    using type = True;
};

defun(null, typename L)
{
    returns typename __nullp<tpp<L>>::type;
};

/// cond
defmacro(cond, typename C, typename T, typename... ES)
{
    defvar(restcond, list<ES...>);
    returns
        q<b<u<C>,
            u<T>,
            u<b<null<restcond>,
                nil,
                q<cond<u<ES>...>>>>>>;
};

/// add
template<typename... A>
struct add_aux {
    using type = v<0>;
};

template<typename A, typename... AS>
struct add_aux<A,AS...> {
    using type = v<A::value + add_aux<AS...>::type::value>;
};

defun(add, Arithmetic... A)
{
    returns typename add_aux<tpp<A>...>::type;
};

/// eq
defun(eq, typename A, typename B)
{
    returns std::conditional_t<std::is_same_v<tpp<A>, tpp<B>>, True, False>;
};

/// match
struct otherwise {
    returns otherwise;
};

defmacro(match, typename O, typename V, typename T, typename... ES){
    returns
        q<cond<
            eq<u<V>, otherwise>, u<T>,
            eq<u<V>, u<O>>, u<T>,
            True, u<b<null<list<ES...>>, nil, q<match<u<O>, u<ES>...>>>>>>;
};

/// fibonacci
defun(fibonacci, UInt N)
{
    returns
        match<N,
            v<0>, v<0>,
            v<1>, v<1>,
            otherwise, add<fibonacci<add<N, v<-1>>>, fibonacci<add<N, v<-2>>>>>;
};
