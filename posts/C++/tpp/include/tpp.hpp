
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

/// t
struct t {
    using type = t;

    returns t;
};

/// f
template <template <typename...> typename F, typename... AS>
struct f {
    using type = f;

    returns f;
};

/// funcall and defun
template <typename F, typename... AS>
struct __funcall { };

template <template <typename...> typename F, typename... AS, typename... FS>
struct __funcall<f<F, AS...>, FS...> {
    returns tpp<tpp<F<__box<tpp<AS>>..., __box<tpp<FS>>...>>>;
};

#define defun(name, ...)                                \
    template <__VA_ARGS__>                              \
    struct ____##name##____;                            \
                                                        \
    template <typename... TS>                           \
    struct name {                                       \
        template <typename... LS>                       \
        using __private_func = ____##name##____<LS...>; \
        returns tpp<__funcall<f<__private_func>, TS...>>; \
    };                                                  \
                                                        \
    template <__VA_ARGS__>                              \
    struct ____##name##____

defun(funcall, typename F, typename... AS)
{
    returns __funcall<F, AS...>;
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
    using name = q<tpp<type>>;


/// list
template <typename... TS>
struct __l {
    using type = __l;

    returns __l;
};

defun(list, typename... TS)
{
    returns __l<TS...>;
};

/// nil
using nil = __l<>;

/// branch
template <typename C, typename T, typename E>
struct b_aux {
    using type = tpp<T>;
};

template <typename T, typename E>
struct b_aux<nil, T, E> {
    using type = tpp<E>;
};

template <typename T, typename E>
struct b_aux<v<false>, T, E> {
    using type = tpp<E>;
};

template <typename C, typename T, typename E>
struct b {
    returns typename b_aux<tpp<C>, T, E>::type;
};

defmacro(c, typename T) {
    returns q<u<T>>;
};

/// car
template <typename L>
struct __car { };

template <typename T, typename... TS>
struct __car<__l<T, TS...>> {
    returns T;
};

defun(car, typename L)
{
    returns __car<L>;
};

/// cdr
template <typename L>
struct __cdr { };

template <typename... TS>
struct __cdr<__l<TS...>> {
    returns __l<>;
};

template <typename T, typename... TS>
struct __cdr<__l<T, TS...>> {
    returns __l<TS...>;
};

defun(cdr, typename L)
{
    returns __cdr<L>;
};

/// length
template <typename L>
struct __length { };

template <typename... TS>
struct __length<__l<TS...>> {
    returns v<sizeof...(TS)>;
};

defun(length, typename L)
{
    returns __length<L>;
};

/// null
template <typename L>
struct __nullp { };

template <typename... TS>
struct __nullp<__l<TS...>> {
    using type = t;
};

template <typename T, typename... TS>
struct __nullp<__l<T, TS...>> {
    using type = nil;
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
    using type = A;
};

template <typename A, typename B, typename... AS>
struct add_aux<A, B, AS...> {
    using type = v<A::value + B::value>;
};

defun(add, typename... A)
{
    returns typename add_aux<tpp<A>...>::type;
};

/// eq
defun(eq, typename A, typename B)
{
    returns
        v<std::is_same_v<A, B>>;
};

/// fibonacci
defun(fibonacci, typename N)
{
    returns
        cond<eq<N, v<0>>, v<0>,
            eq<N, v<1>>, v<1>,
            v<true>, add<fibonacci<add<N, v<-1>>>,
                         fibonacci<add<N, v<-2>>>>>;
};
