
#pragma once

#include <type_traits>
#include <iostream>


/// function
template<template<typename...> typename F>
struct function{
  using type = function;

  template<typename... A>
  using call = typename F<A...>::type;
};


/// canonicalize
#define EXPAND(X) X
#define canonize(name)                                  \
  template<typename... TS>                              \
  using name##_t = typename name<TS...> ::type;         \
                                                        \
  template<typename... TS>                              \
  static constexpr auto name##_v = name<TS...> ::value; \
                                                        \
  using name##_f = function<name>;

/// cinco
using cinco = std::integral_constant<int,5>;


/// int_constant
template<int k>
struct int_constant : std::integral_constant<int,k>{};

template<int k>
constexpr int int_constant_v = int_constant<k>::value;

template<int k>
using int_constant_t = typename int_constant<k>::type;



/// bool_constant
template<int b>
struct bool_constant : std::integral_constant<bool,b>{};

template<int b>
constexpr bool bool_constant_v = int_constant<b>::value;

template<int b>
using bool_constant_t = typename bool_constant<b>::type;


/// add
template<typename... NS>
struct add : int_constant<0>{};

template<typename N, typename... NS>
struct add<N,NS...> : int_constant<N::value+add<NS...>::value> {};

template<typename... NS>
constexpr int add_v = add<NS...>::value;

template<typename... NS>
using add_t = typename add<NS...>::type;

using add_f = function<add>;


/// mult
template<typename N, typename M>
struct mult : int_constant<N::value * M::value> {};

template<typename N, typename M>
constexpr int mult_v = mult<N,M>::value;

template<typename N, typename M>
using mult_t = typename mult<N,M>::type;


/// isZero
template<typename T>
struct isZero
	: bool_constant<false> {};

template<>
struct isZero<std::integral_constant<int,0>>
	: bool_constant<true> {};

template<typename T>
constexpr bool isZero_v = isZero<T>::value;

template<typename T>
using isZero_t = typename isZero<T>::type;


/// or operator
template<typename B, typename C>
struct or_bool : bool_constant<B::value || C::value> {};

template<typename B, typename C>
constexpr bool or_bool_v = or_bool<B,C>::value;

template<typename B, typename C>
using or_bool_t = typename or_bool<B,C>::type;


/// not operator
template<typename B>
struct not_bool : bool_constant<!B::value> {};

template<typename B>
constexpr bool not_bool_v = not_bool<B>::value;

template<typename B>
using not_bool_t = typename not_bool<B>::type;


/// add1
template<typename N>
struct add1 : int_constant<N::value + 1> {};

template<typename N>
constexpr int add1_v = add1<N>::value;

template<typename N>
using add1_t = typename add1<N>::type;

using add1_f = function<add1>;

/// sub1
template<typename N>
struct sub1 : int_constant<N::value - 1> {};

template<typename N>
constexpr int sub1_v = sub1<N>::value;

template<typename N>
using sub1_t = typename sub1<N>::type;



/// eql
template<typename N, typename M>
struct eql : bool_constant<N::value == M::value> {};

template<typename N, typename M>
constexpr bool eql_v = eql<N,M>::value;

template<typename N, typename M>
using eql_t = typename eql<N,M>::type;


/// mod operator
template<typename A, typename B>
struct mod : int_constant<A::value % B::value> {};

template<typename A, typename B>
constexpr int mod_v = mod<A,B>::value;

template<typename A, typename B>
using mod_t = typename mod<A,B>::type;


/// isDivisor
template<typename D, typename N>
struct isDivisor : isZero<mod_t<N,D>> {};

template<typename D, typename N>
constexpr bool isDivisor_v = isDivisor<D,N>::value;

template<typename D, typename N>
using isDivisor_t = typename isDivisor<D,N>::type;


/// hasDivisors
template<typename D, typename N>
struct hasDivisors_aux : or_bool<
							isDivisor_t<D,N>,
							typename hasDivisors_aux<add1_t<D>,N>::type> {};

template<typename N>
struct hasDivisors_aux<N,N> : bool_constant<false> {};

template<typename N>
struct hasDivisors : hasDivisors_aux<int_constant_t<2>,N> {};

template<typename N>
constexpr bool hasDivisors_v = hasDivisors<N>::value;

template<typename N>
using hasDivisors_t = typename hasDivisors<N>::type;


/// isPrime
template<typename N>
struct isPrime : not_bool<hasDivisors_t<N>> {};

template<typename N>
constexpr bool isPrime_v = isPrime<N>::value;

template<typename N>
using isPrime_t = typename isPrime<N>::type;


/// nextPrime
template<typename N, typename IsPrime>
struct nextPrimeAux : nextPrimeAux<add1_t<N>,isPrime_t<add1_t<N>>> {};

template<typename N>
struct nextPrimeAux<N,bool_constant_t<true>> : N {};

template<typename N>
struct nextPrime : nextPrimeAux<add1_t<N>,isPrime_t<add1_t<N>>> {};

template<typename N>
constexpr int nextPrime_v = nextPrime<N>::value;

template<typename N>
using nextPrime_t = typename nextPrime<N>::type;


// vector
struct vector{
	int x;
	int y;
};

template<int x, int y>
struct vector_constant{
	static constexpr vector value = {x,y};
	using type = vector_constant;
};

template<int x, int y>
constexpr vector vector_constant_v = vector_constant<x,y>::value;

template<int x, int y>
using vector_constant_t = typename vector_constant<x,y>::type;

std::ostream& operator<<(std::ostream& os, const vector& v){
	return os << "(" << v.x << "," << v.y << ")";
}


/// getters
template<typename P>
struct vector_x : int_constant<P::value.x> {};

template<typename P>
constexpr int vector_x_v = vector_x<P>::value;

template<typename P>
using vector_x_t = typename vector_x<P>::type;


template<typename P>
struct vector_y : int_constant<P::value.y> {};

template<typename P>
constexpr int vector_y_v = vector_y<P>::value;

template<typename P>
using vector_y_t = typename vector_y<P>::type;


/// dot_product
template<typename P, typename Q>
struct dot_product : add<
						mult_t<
							vector_x_t<P>,
							vector_x_t<Q>>,
						mult_t<
							vector_y_t<P>,
							vector_y_t<Q>>> {};

template<typename P, typename Q>
constexpr int dot_product_v = dot_product<P,Q>::value;

template<typename P, typename Q>
using dot_product_t = typename dot_product<P,Q>::type;


/// integral_array
template<typename T, T... TS>
struct integral_array{
	static constexpr T value[] = {TS...};
	using type = integral_array;
	using value_type = T;
};

template<typename T, T... TS>
constexpr T integral_array<T, TS...>::value[];

template <typename T, T... TS>
constexpr const T* integral_array_v = integral_array<T,TS...>::value;

template<typename T, T... TS>
using integral_array_t = typename integral_array<T,TS...>::type;


/// integral_array (char)
template<char... TS>
struct integral_array<char,TS...>{
	static constexpr char value[] = {TS...,'\0'};
	using type = integral_array;
	using value_type = char;
};

template <char... TS>
constexpr char integral_array<char,TS...>::value[];



/// int_array
template<int... TS>
struct int_array : integral_array<int,TS...> {};

template <int... TS>
constexpr const int* int_array_v = int_array<TS...>::value;

template<int... TS>
using int_array_t = typename int_array<TS...>::type;


/// char_array
template<char... cs>
struct char_array : integral_array<char,cs...> {};

template <char... cs>
constexpr const char* char_array_v = char_array<cs...>::value;

template<char... cs>
using char_array_t = typename char_array<cs...>::type;


/// aref
template<typename A, typename N>
struct aref
	: std::integral_constant<typename A::value_type,A::value[N::value]> {};

template<typename A, typename N>
constexpr typename A::value_type aref_v = aref<A,N>::value;

template<typename A, typename N>
using aref_t = typename aref<A,N>::type;


/// sum_array
template<typename A, typename N>
struct sum_array : add<
						aref_t<A,sub1_t<N>>,
						typename sum_array<A,sub1_t<N>>::type> {};

template<typename A>
struct sum_array<A,int_constant_t<0>> : int_constant_t<0> {};

template<typename A, typename N>
constexpr int sum_array_v = sum_array<A,N>::value;

template<typename A, typename N>
using sum_array_t = typename sum_array<A,N>::type;


/// int_collection
/// define una coleccion de enteros
template<int... ns>
struct int_collection {
	using type = int_collection;
};

template<int... ns>
using int_collection_t = typename int_collection<ns...>::type;


/// range_collection
/// devuelve una coleccion de enteros de 0 a n
template<int n, int... ns>
struct range_collection_aux : range_collection_aux<n-1,n-1,ns...> {};

template<int... ns>
struct range_collection_aux<0,ns...> : int_collection<ns...> {};

template<int n>
struct range_collection : range_collection_aux<n> {};

template<int n>
using range_collection_t = typename range_collection<n>::type;


/// aref_str
/// devuelve el caracter de un string de C++ con indice i
template<const char* str, int i>
struct aref_str{
	static constexpr char value = str[i];
};

template<const char* str, int i>
constexpr char aref_str_v = aref_str<str,i>::value;


/// length_str
/// devuelve la longitud de un string de C++
template<const char* str, int i, bool nullchar>
struct length_str_aux : length_str_aux<str,i+1,str[i]=='\0'> {};

template<const char* str, int i>
struct length_str_aux<str,i,true>{
	static constexpr int value = i;
};

template<const char* str>
struct length_str : length_str_aux<str,0,str[0]=='\0'> {};

template<const char* str>
constexpr int length_str_v = length_str<str>::value;


/// Crea un string de T++ a partir de uno de C++
template<const char* str, typename C>
struct make_string_aux {};

template<const char* str, int... ns>
struct make_string_aux<str,int_collection<ns...>> : integral_array<char,aref_str_v<str,ns>...> {};

template<const char* str>
struct make_string : make_string_aux<str,range_collection_t<length_str_v<str>>> {};

template<const char* str>
constexpr const char* make_string_v = make_string<str>::value;

template<const char* str>
using make_string_t = typename make_string<str>::type;

constexpr char holaMundo[] = "Hola mundo!";


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

template<typename T, typename... TS>
struct is_empty<collection<T,TS...>> : bool_constant<false> {};

template<typename... TS>
struct is_empty<collection<TS...>> : bool_constant<true> {};

template<typename C>
constexpr bool is_empty_v = is_empty<C>::value;

template<typename C>
using is_empty_t = typename is_empty<C>::type;


/// concatenate
template<typename C, typename D>
struct concatenate {};

template<typename... CS, typename... DS>
struct concatenate<collection<CS...>,collection<DS...>> : collection<CS...,DS...> {};

template<typename C,typename D>
using concatenate_t = typename concatenate<C,D>::type;


/// debug
template <typename>
struct debug {};


/// reverse_collection
template<typename C, typename D>
struct reverse_collection_aux {};

template<typename... cs, typename... ds>
struct reverse_collection_aux<collection<cs...>, collection<ds...>>
	: collection_t<ds...> {};

template<typename c, typename... cs, typename... ds>
struct reverse_collection_aux<collection<c,cs...>, collection<ds...>>
	: reverse_collection_aux<collection_t<cs...>, collection_t<c,ds...>> {};

template<typename C>
struct reverse_collection
	: reverse_collection_aux<C, collection_t<>> {};

template<typename C>
using reverse_collection_t = typename reverse_collection<C>::type;


/// funcall
template<typename F, typename... A>
struct funcall : F::template call<A...> {};

template<typename F, typename... A>
using funcall_t = typename funcall<F,A...>::type;

template<typename F, typename... A>
static constexpr typename funcall<F,A...>::value_type funcall_v = funcall<F,A...>::value;

using funcall_f = function<funcall>;


/// apply
template<typename F, typename C, typename... A>
struct apply_aux {};

template<typename F, typename... CS, typename A, typename B, typename... AS>
struct apply_aux<F,collection<CS...>,A,B,AS...> : apply_aux<F,collection<CS...,A>,B,AS...> {};

template<typename F, typename... CS, typename... AS>
struct apply_aux<F,collection<CS...>,collection<AS...>> : funcall<F,CS...,AS...> {};

template<typename F, typename... A>
struct apply : apply_aux<F,collection<>,A...> {};

template<typename F, typename... A>
using apply_t = typename apply<F,A...>::type;

template<typename F, typename... A>
static constexpr typename apply<F,A...>::value_type apply_v = apply<F,A...>::value;

using apply_f = function<apply>;


/// sum
template<typename C>
struct sum : apply<add_f,C> {};

template<typename C>
using sum_t = typename sum<C>::type;

template<typename C>
static constexpr int sum_v = sum<C>::value;

using sum_f = function<sum>;


/// first
template<typename C>
struct first {};

template<typename C, typename... CS>
struct first<collection<C,CS...>> : C {};

canonize(first);


/// cons
template<typename V, typename C>
struct cons {};

template<typename V, typename... CS>
struct cons<V, collection<CS...>> : collection<V,CS...> {};

canonize(cons);


/// rest
template<typename C>
struct rest {};

template<typename C, typename... CS>
struct rest<collection<C,CS...>> : collection<CS...> {};

template<typename... CS>
struct rest<collection<CS...>> : collection<CS...> {};

canonize(rest);


/// range
template<typename N, typename I>
struct range_aux {};

template<int k>
struct range_aux<int_constant<k>,int_constant<k>> : collection<> {};

template<int n, int i>
struct range_aux<int_constant<n>, int_constant<i>>
  : cons<int_constant<n>, typename range_aux<int_constant<n+1>, int_constant<i>>::type> {};

template<typename I>
struct range : range_aux<int_constant<0>, I> {};

canonize(range);


/// length
template<typename C>
struct length {};

template<>
struct length<collection<>> : int_constant<0> {};

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


/// and_bool
template<typename... BS>
struct and_bool {};

template<>
struct and_bool<> : bool_constant<true> {};

template<typename B>
struct and_bool<B> : B {};

template<bool a, bool b, typename... BS>
struct and_bool<bool_constant<a>, bool_constant<b>, BS...>
  : and_bool<bool_constant<a && b>, BS...> {};

canonize(and_bool);


/// zip_aux
template<bool stop, typename... CS>
struct zip_aux {};

template<typename... CS>
struct zip_aux<false, CS...> : collection<> {};

template<typename... CS>
struct zip_aux<true, CS...> 
  : cons<collection<first_t<CS>...>, 
         typename zip_aux<apply_v<and_bool_f,map_single_t<is_empty_f,map_single_t<rest_f,collection<CS...>>>>,
                          rest_t<CS>...>::type> {};

template<typename... CS>
struct zip : zip_aux<apply_v<and_bool_f,map_single_t<is_empty_f,CS...>>,
                     CS...>
