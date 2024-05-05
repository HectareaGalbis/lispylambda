#pragma once

#include <type_traits>
#include <iostream>

/// identity
template<typename T>
struct identity{
  template<typename... TS>
  using call = T;
};


/// box
template<auto b>
struct box{
  using type = box;
  using value_type = decltype(b);
  static constexpr auto value = b; 
};

template<auto b>
using box_t = typename box<b>::type;

template<auto b>
using box_vt = typename box<b>::value_type;

template<auto b>
extern constexpr auto box_v = box<b>::value;


/// tbox
template<typename T>
struct tbox{
  using type = tbox;
  using value_type = T;
};

template<typename T>
using tbox_t = typename tbox<T>::type;

template<typename T>
using tbox_vt = typename tbox<T>::value_type;


/// curry
template<template<typename...> typename F, typename... AS>
struct curry{
  using type = curry;

  template<typename... BS>
  using call = typename F<AS...,BS...>::type;
};


/// defun
#define defun(name)                             \
  template<typename... TS>                      \
  using name = curry<name##_impl, TS...>;


/// cocurry
template<template<typename...> typename F, typename... AS>
struct cocurry{
  using type = cocurry;

  template<typename... BS>
  using call = typename F<AS...,BS...>::type;
};


/// funcall
template<typename F, typename... A>
struct funcall : F::template call<A...> {};


/// eval
template<typename T>
struct interpret : T {};

template<typename T>
struct cointerpret : T {};

template<typename T>
struct eval : interpret<typename cointerpret<T>::type> {};

template<template<typename...> typename F, typename... AS>
struct interpret<curry<F,AS...>>
  : funcall<curry<F,typename eval<AS>::type...>> {};

template<template<typename...> typename F, typename... AS>
struct cointerpret<cocurry<F,AS...>>
  : eval<typename funcall<cocurry<F,AS...>>::type> {};

template<typename T>
using eval_t = typename eval<T>::type;

template<typename T>
using eval_vt = typename eval<T>::value_type;

template<typename T>
extern constexpr auto eval_v = eval<T>::value;


/// add
template<typename... NS>
struct add_impl : box<0>{};

template<typename N, typename... NS>
struct add_impl<N,NS...> : box<N::value+add_impl<NS...>::value> {};

defun(add);
