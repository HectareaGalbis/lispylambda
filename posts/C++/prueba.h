
#pragma once

#include <type_traits>
#include <iostream>

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
template<typename A, typename B>
struct add : int_constant<A::value+B::value>{};

template<typename A, typename B>
constexpr int add_v = add<A,B>::value;

template<typename A, typename B>
using add_t = typename add<A,B>::type;


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


/// array
template<typename T, T... TS>
struct array{
	static constexpr T value[] = {TS...};
	using type = array;
	using value_type = T;
};

template<typename T, T... TS>
using array_t = typename array<T,TS...>::type;


/// int_array
template<int... TS>
struct int_array : array<int,TS...> {};

template<int... TS>
using int_array_t = typename int_array<TS...>::type;


/// aref
template<typename A, typename N>
struct aref
	: std::integral_constant<typename A::value_type,A::value[N::value]> {};

template<typename A, typename N>
constexpr typename A::value_type aref_v = aref<A,N>::value;

template<typename A, typename N>
using aref_t = typename aref<A,N>::type;







