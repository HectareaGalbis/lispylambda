#pragma once

#include <iostream>
#include <type_traits>

/// debug
template <typename>
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


/// evaluating
#define evaluating         \
    template <typename...> \
    using tpp_return =

/// box
template <auto b>
struct box {
    using type = box;
    using value_type = decltype(b);
    static constexpr auto value = b;

    evaluating box;
};

/// tbox
template <typename T>
struct tbox {
    using type = tbox;
    using value_type = T;

    evaluating tbox;
};

/// ttbox
template <template <typename...> typename F>
struct ttbox {
    using type = ttbox;
    template <typename... FS>
    using value_template_type = F<FS...>;

    evaluating ttbox;
};

/// variable
template<typename T>
struct variable {
    evaluating T;
};

/// function
template <template <typename...> typename F, typename... AS>
struct make_function {
    evaluating tpp<F<variable<tpp<AS>>...>>;
};

/// defun
#define defun(name, ...)                                  \
    template <__VA_ARGS__>                                \
    struct name##_impl;                                   \
                                                          \
    template <typename... TS>                             \
    struct name##_pre_impl : tpp<name##_impl<TS...>> { }; \
                                                          \
    template <typename... TS>                             \
    struct name : make_function<name##_pre_impl, TS...> { };

/// let
#define let using

/// returning
#define returning          \
    template <typename...> \
    using tpp_return =

/// collection
template<typename... TS>
struct eval_collection {};

template<template<typename...> typename F, typename... AS>
struct eval_collection<ttbox<F>,AS...> : F<AS...> {};

template <typename... TS>
struct collection {
    using type = collection;

    evaluating tpp<eval_collection<TS...>>;
};

/// quote
template <typename T>
struct quote {
    evaluating T;
};

template <template <typename...> typename F, typename... AS>
struct quote<F<AS...>> {
    evaluating collection<ttbox<F>, tpp<quote<AS>>...>;
};

/// qt
template<typename T>
using qt = quote<T>;

/// unquote
template <typename T>
struct unquote { };

/// uq
template<typename T>
using uq = unquote<T>;

/// quasiquote
template<typename T>
struct quasiquote {
    evaluating T;
};

template<typename T>
struct quasiquote<unquote<T>> {
    evaluating tpp<T>;  
};

template <template <typename...> typename F, typename... AS>
struct quasiquote<F<AS...>> {
    evaluating collection<ttbox<F>, tpp<quasiquote<AS>>...>;  
};

/// qq
template<typename T>
using qq = quasiquote<T>;

/// eval
defun(eval, typename E);

template <typename E>
struct eval_impl {
    returning tpp<E>;
};

/// macro
template <template <typename...> typename M, typename... AS>
struct macro {
    using type = macro;

    evaluating tpp<tpp<M<variable<quote<AS>>...>>>;
};

/// defmacro
#define defmacro(name, ...)                               \
    template <__VA_ARGS__>                                \
    struct name##_impl;                                   \
                                                          \
    template <typename... TS>                             \
    struct name##_pre_impl : tpp<name##_impl<TS...>> { }; \
                                                          \
    template <typename... TS>                             \
    struct name : macro<name##_pre_impl, TS...> { };

/// add
defun(add, typename... NS);

template <typename... NS>
struct add_impl {
    returning box<0>;
};

template <typename N, typename... NS>
struct add_impl<N, NS...> {
    returning box<tpp_v<N> + tpp_v<add<NS...>>>;
};

/// mult
defun(mult, typename... NS);

template <typename... NS>
struct mult_impl {
    returning box<1>;
};

template <typename N, typename... NS>
struct mult_impl<N, NS...> {
    returning box<tpp_v<N> * tpp_v<mult<NS...>>>;
};

// sub
defun(sub, typename... NS);

template <typename... NS>
struct sub_impl {
    returning box<0>;
};

template <typename N, typename... NS>
struct sub_impl<N, NS...> {
    returning box<tpp_v<N> - tpp_v<sub<NS...>>>;
};

/// quotient
defun(quotient, typename... NS);

template <typename... NS>
struct quotient_impl {
    returning box<1>;
};

template <typename N, typename... NS>
struct quotient_impl<N, NS...> {
    returning box<tpp_v<N> / tpp_v<quotient<NS...>>>;
};

/// is_zero
defun(is_zero, typename T);

template <typename T>
struct is_zero_impl {
    returning box<tpp_v<T> == 0>;
};

/// or_logic
defun(or_logic, typename... NS);

template <typename... NS>
struct or_logic_impl {
    returning box<false>;
};

template <typename N, typename... NS>
struct or_logic_impl<N, NS...> {
    returning box<tpp_v<N> || tpp_v<or_logic<NS...>>>; 
};

/// and_logic
defun(and_logic, typename... NS);

template <typename... NS>
struct and_logic_impl {
    returning box<true>;
};

template <typename N, typename... NS>
struct and_logic_impl<N, NS...> {
    returning box<tpp_v<N> && tpp_v<and_logic<NS...>>>;
};

/// not operator
defun(not_logic, typename B);

template <typename B>
struct not_logic_impl {
    returning box<!tpp_v<B>>;
};

/// add1
defun(add1, typename N);

template <typename N>
struct add1_impl {
    returning add<N, box<1>>;
};

/// sub1
defun(sub1, typename N);

template <typename N>
struct sub1_impl {
    returning sub<N, box<1>>;
};

/// eql
defun(eql, typename N, typename M);

template <typename N, typename M>
struct eql_impl {
    returning box<tpp_v<N> == tpp_v<M>>;
};

/// branch
template <typename C, typename T, typename E>
struct branch : branch<tpp<C>, T, E> { };

template <typename T, typename E>
struct branch<box<true>, T, E> : tpp<T> { };

template <typename T, typename E>
struct branch<box<false>, T, E> : tpp<E> { };

/// cond
defmacro(cond, typename... CT);

template <typename C, typename T, typename... ES>
struct cond_impl<C, T, ES...>  {
    returning qq<branch<uq<C>, uq<T>, cond<uq<ES>...>>>;
};

/// fibonacci
defun(fibonacci, typename N);

template <typename N>
struct fibonacci_impl {
    returning cond<
        eql<N, box<0>>, box<0>,
        eql<N, box<1>>, box<1>,
        box<true>,add<fibonacci<sub1<N>>,
                      fibonacci<sub<N, box<2>>>>>;
};

/// is_empty
defun(is_empty, typename C);

template <typename... TS>
struct is_empty_impl<collection<TS...>> : box<true> { };

template <typename T, typename... TS>
struct is_empty_impl<collection<T, TS...>> : box<false> { };

/// cons
defun(cons, typename V, typename C);

template <typename V, typename... CS>
struct cons_impl<V, collection<CS...>> : collection<V, CS...> { };

/// car
defun(car, typename C);

template <typename C, typename... CS>
struct car_impl<collection<C, CS...>> : C { };

/// cdr
defun(cdr, typename C);

template <typename C, typename... CS>
struct cdr_impl<collection<C, CS...>> : collection<CS...> { };

template <typename... CS>
struct cdr_impl<collection<CS...>> : collection<CS...> { };

/// caar
defun(caar, typename C);

template <typename C>
struct caar_impl : car<car<C>> { };


/// cadr
defun(cadr, typename C);

template <typename C>
struct cadr_impl : car<cdr<C>> { };

/// cdar
defun(cdar, typename C);

template <typename C>
struct cdar_impl : cdr<car<C>> { };

/// cddr
defun(cddr, typename C);

template <typename C>
struct cddr_impl : cdr<cdr<C>> { };

/// concat
defun(concat, typename C, typename D);

template <typename... CS, typename... DS>
struct concat_impl<collection<CS...>, collection<DS...>> : collection<CS..., DS...> { };

/// reverse_collection
defun(reverse_collection, typename C);

template <typename C, typename D>
struct reverse_collection_aux { };

template <typename... cs, typename... ds>
struct reverse_collection_aux<collection<cs...>, collection<ds...>>
    : collection<ds...> { };

template <typename c, typename... cs, typename... ds>
struct reverse_collection_aux<collection<c, cs...>, collection<ds...>>
    : reverse_collection_aux<collection<cs...>, collection<c, ds...>> { };

template <typename... cs>
struct reverse_collection_impl<collection<cs...>> : reverse_collection_aux<collection<cs...>, collection<>> { };

