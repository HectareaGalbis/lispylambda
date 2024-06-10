#pragma once

#include <type_traits>
#include <iostream>

/// debug
template <typename>
struct debug {};

/// eval
template<typename T>
using eval = typename T::eval_impl<>;

template<typename T>
using eval_t = typename eval<T>::type;

template<typename T>
extern constexpr auto eval_v = eval<T>::value;


/// box
template<auto b>
struct box{
  using type = decltype(b);
  static constexpr auto value = b;

  template<typename...>
  using eval_impl = box;
};


/// tbox
template<typename T>
struct tbox{
  using type = T;

  template<typename...>
  using eval_impl = tbox;
};


/// function
template<template<typename...> typename F, typename... AS>
struct make_function{
  template<typename...>
  using eval_impl = eval<F<eval<AS>...>>;
};


/// defun
#define defun(name, ...)                           \
  template<__VA_ARGS__>                            \
  struct name##_impl;                              \
                                                   \
  template<typename... TS>                         \
  using name = make_function<name##_impl, TS...>;


/// macro
template<template<typename...> typename M, typename... AS>
struct macro{
  using type = macro;

  template<typename...>
  using eval_impl = eval<eval<M<AS...>>>;
};


/// defmacro
#define defmacro(name)                          \
  template<typename... TS>                      \
  using name = macro<name##_impl, TS...>;


/// add
defun(add, typename... NS);

template<typename... NS>
struct add_impl : box<0>{};

template<typename N, typename... NS>
struct add_impl<N,NS...> : box<N::value+add_impl<NS...>::value> {};


/// mult
defun(mult, typename... NS);

template<typename... NS>
struct mult_impl : box<1> {};

template<typename N, typename... NS>
struct mult_impl<N,NS...> : box<N::value * mult_impl<NS...>::value> {};


// sub
defun(sub, typename... NS);

template<typename... NS>
struct sub_impl : box<0>{};

template<typename N, typename... NS>
struct sub_impl<N,NS...> : box<N::value-sub_impl<NS...>::value> {};


/// quotient
defun(quotient, typename... NS);

template<typename... NS>
struct quotient_impl : box<1> {};

template<typename N, typename... NS>
struct quotient_impl<N,NS...> : box<N::value / quotient_impl<NS...>::value> {};


/// is_zero
defun(is_zero, typename T);

template<typename T>
struct is_zero_impl : box<false>{}; 

template<>
struct is_zero_impl<box<0>> : box<true> {};


/// or_logic
defun(or_logic, typename... NS);

template<typename... NS>
struct or_logic_impl : box<false>{};

template<typename N, typename... NS>
struct or_logic_impl<N,NS...> : box<N::value || or_logic_impl<NS...>::value> {};


/// and_logic
defun(and_logic, typename... NS);

template<typename... NS>
struct and_logic_impl : box<true>{};

template<typename N, typename... NS>
struct and_logic_impl<N,NS...> : box<N::value && and_logic_impl<NS...>::value> {};


/// not operator
defun(not_logic, typename B);

template<typename B>
struct not_logic_impl : box<!B::value> {};


/// add1
defun(add1, typename N);

template<typename N>
struct add1_impl : add<N,box<1>> {};


/// sub1
defun(sub1, typename N);

template<typename N>
struct sub1_impl : box<N::value - 1> {};


/// eql
defun(eql, typename N, typename M);

template<typename N, typename M>
struct eql_impl : box<N::value == M::value> {};


/// branch
template<typename C, typename T, typename E>
struct branch : branch<eval<C>,T,E> {};

template<typename T, typename E>
struct branch<box<true>,T,E> : eval<T> {};

template<typename T, typename E>
struct branch<box<false>,T,E> : eval<E> {};


/// quote
template<typename T>
struct quote {
  template<typename...>
  using eval_impl = T;
};


/// cond
template<typename... CT>
struct cond_impl {};

defmacro(cond);

template<typename C, typename T, typename... ES>
struct cond_impl<C,T,ES...> : quote<branch<C,T,cond<ES...>>> {};


/// fibonacci
defun(fibonacci, typename N);

template<typename N>
struct fibonacci_impl
  : branch<eql<N,box<0>>,
           box<0>,
           branch<eql<N,box<1>>,
                  box<1>,
                  add<fibonacci<sub1<N>>, fibonacci<sub1<sub1<N>>>>>> {};

