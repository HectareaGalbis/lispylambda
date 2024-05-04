
#pragma once

#include <type_traits>
#include <iostream>


/// debug
template <typename>
struct debug {};


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


/// function
template<template<typename...> typename F>
struct function{
  using type = function;

  template<typename... A>
  using call = typename F<A...>::type;
};


/// curry
template<template<typename...> typename F, typename... AS>
struct curry{
  using type = curry;

  template<typename... BS>
  using call = typename F<AS...,BS...>::type;
};


/// canonize
#define canonize(name)                                  \
  template<typename... TS>                              \
  using name##_t = typename name<TS...> ::type;         \
                                                        \
  template<typename... TS>                              \
  static constexpr auto name##_v = name<TS...> ::value; \
                                                        \
  template<typename... TS>                              \
  using name##_vt = typename name<TS...>::value_type;   \
                                                        \
  using name##_f = function<name>;                      \
                                                        \
  template<typename... TS>                              \
  using name##_c = curry<name, TS...>;


/// identity
template<typename T>
struct identity : T {};

canonize(identity);


/// add
template<typename... NS>
struct add : box<0>{};

template<typename N, typename... NS>
struct add<N,NS...> : box<N::value+add<NS...>::value> {};

canonize(add);


/// mult
template<typename... NS>
struct mult : box<1> {};

template<typename N, typename... NS>
struct mult<N,NS...> : box<N::value * mult<NS...>::value> {};

canonize(mult);


// sub
template<typename... NS>
struct sub : box<0>{};

template<typename N, typename... NS>
struct sub<N,NS...> : box<N::value-sub<NS...>::value> {};

canonize(sub);


/// quot
template<typename... NS>
struct quot : box<1> {};

template<typename N, typename... NS>
struct quot<N,NS...> : box<N::value / quot<NS...>::value> {};

canonize(quot);


/// is_zero
template<typename T>
struct is_zero : box<false>{}; 

template<>
struct is_zero<box<0>> : box<true> {};

canonize(is_zero);


/// or_logic
template<typename... NS>
struct or_logic : box<false>{};

template<typename N, typename... NS>
struct or_logic<N,NS...> : box<N::value || or_logic<NS...>::value> {};

canonize(or_logic);


/// and_logic
template<typename... NS>
struct and_logic : box<true>{};

template<typename N, typename... NS>
struct and_logic<N,NS...> : box<N::value && and_logic<NS...>::value> {};

canonize(and_logic);


/// not operator
template<typename B>
struct not_logic : box<!B::value> {};

canonize(not_logic);


/// add1
template<typename N>
struct add1 : box<N::value + 1> {};

canonize(add1);


/// sub1
template<typename N>
struct sub1 : box<N::value - 1> {};

canonize(sub1);


/// eql
template<typename N, typename M>
struct eql : box<N::value == M::value> {};

canonize(eql);


/// collection
template<typename... TS>
struct collection{
  using type = collection;
};

template<typename... TS>
using collection_t = typename collection<TS...>::type;


/// is_empty
template<typename C>
struct is_empty {};

template<typename... TS>
struct is_empty<collection<TS...>> : box<true> {};

template<typename T, typename... TS>
struct is_empty<collection<T,TS...>> : box<false> {};

canonize(is_empty);


/// cons
template<typename V, typename C>
struct cons {};

template<typename V, typename... CS>
struct cons<V, collection<CS...>> : collection<V,CS...> {};

canonize(cons);


/// car
template<typename C>
struct car {};

template<typename C, typename... CS>
struct car<collection<C,CS...>> : C {};

canonize(car);


/// cdr
template<typename C>
struct cdr {};

template<typename C, typename... CS>
struct cdr<collection<C,CS...>> : collection<CS...> {};

template<typename... CS>
struct cdr<collection<CS...>> : collection<CS...> {};

canonize(cdr);


/// caar
template<typename C>
struct caar : car<car_t<C>> {};

canonize(caar);

/// cadr
template<typename C>
struct cadr : car<cdr_t<C>> {};

canonize(cadr);

/// cdar
template<typename C>
struct cdar : cdr<car_t<C>> {};

canonize(cdar);

/// cddr
template<typename C>
struct cddr : cdr<cdr_t<C>> {};

canonize(cddr);


/// concat
template<typename C, typename D>
struct concat {};

template<typename... CS, typename... DS>
struct concat<collection<CS...>,collection<DS...>> : collection<CS...,DS...> {};

canonize(concat);


/// reverse_collection
template<typename C, typename D>
struct reverse_collection_aux {};

template<typename... cs, typename... ds>
struct reverse_collection_aux<collection<cs...>, collection<ds...>>
	: collection<ds...> {};

template<typename c, typename... cs, typename... ds>
struct reverse_collection_aux<collection<c,cs...>, collection<ds...>>
	: reverse_collection_aux<collection<cs...>, collection<c,ds...>> {};

template<typename C>
struct reverse {};

template<typename... cs>
struct reverse<collection<cs...>> : reverse_collection_aux<collection<cs...>, collection<>> {};

canonize(reverse);


/// funcall
template<typename F, typename... A>
struct funcall : F::template call<A...> {};

canonize(funcall);


/// apply
template<typename F, typename C, typename... A>
struct apply_aux {};

template<typename F, typename... CS, typename A, typename B, typename... AS>
struct apply_aux<F,collection<CS...>,A,B,AS...> : apply_aux<F,collection<CS...,A>,B,AS...> {};

template<typename F, typename... CS, typename... AS>
struct apply_aux<F,collection<CS...>,collection<AS...>> : funcall<F,CS...,AS...> {};

template<typename F, typename... A>
struct apply : apply_aux<F,collection<>,A...> {};

canonize(apply);


/// sum
template<typename C>
struct sum : apply<add_f,C> {};

canonize(sum);


/// range
template<typename N, typename I>
struct range_aux {};

template<int k>
struct range_aux<box<k>,box<k>> : collection<> {};

template<int n, int i>
struct range_aux<box<n>, box<i>>
  : cons<box<n>, typename range_aux<box<n+1>, box<i>>::type> {};

template<typename I>
struct range : range_aux<box<0>, I> {};

canonize(range);


/// length
template<typename C>
struct length {};

template<>
struct length<collection<>> : box<0> {};

template<typename C, typename... CS>
struct length<collection<C,CS...>> : add1<typename length<collection<CS...>>::type> {};

canonize(length);


/// map_single
template<typename F, typename C>
struct map_single {};

template<typename F>
struct map_single<F, collection<>> : collection<> {};

template<typename F, typename C, typename... CS>
struct map_single<F, collection<C,CS...>>
  : cons<funcall_t<F,C>, typename map_single<F, collection<CS...>>::type> {};

canonize(map_single);


/// zip
template<bool stop, typename... CS>
struct zip_aux {};

template<typename... CS>
struct zip_aux<true, CS...> : collection<> {};

template<typename... CS>
struct zip_aux<false, CS...> 
  : cons<collection<car_t<CS>...>, 
         typename zip_aux<apply_v<and_logic_f,map_single_t<is_empty_f,map_single_t<cdr_f,collection<CS...>>>>,
                          cdr_t<CS>...>::type> {};

template<typename... CS>
struct zip : zip_aux<apply_v<and_logic_f,map_single_t<is_empty_f,collection<CS...>>>,
                     CS...> {};

canonize(zip);


/// map
template<typename F, typename C>
struct map_aux {};

template<typename F, typename... CS>
struct map_aux<F, collection<CS...>> : collection<apply_t<F,CS>...> {};

template<typename F, typename... CS>
struct map : map_aux<F, zip_t<CS...>> {};

canonize(map);


/// interpret
template<typename T>
struct interpret : T {};

template<template<typename...> typename F, typename... AS>
struct interpret<curry<F,AS...>> : funcall<curry<F,typename interpret<AS>::type...>> {};

canonize(interpret);



// /// branch
// template<typename C, typename T, typename E>
// struct branch {};

// template<typename T, typename E>
// struct branch<box<true>,T,E> : funcall<T> {};

// template<typename T, typename E>
// struct branch<box<false>,T,E> : funcall<E> {};

// canonize(branch);


// /// fibonacci
// template<typename N>
// struct fibonacci : branch<eql_t<N, box<0>>, 
//                           identity_c<box<0>>,
//                           branch_c<eql_t<N, box<1>>,
//                                    box<1>,
//                                    add_t<typename fibonacci<sub1_t<N>>::type,
//                                          typename fibonacci<sub1_t<sub1_t<N>>>::type>>> {};

// canonize(fibonacci);
