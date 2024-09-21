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

/// v
template <auto b>
struct v {
    using type = v;
    using value_type = decltype(b);
    static constexpr auto value = b;

    evaluating v;
};

/// t
template <typename T>
struct t {
    using type = t;
    using value_type = T;

    evaluating t;
};

/// tt
template <template <typename...> typename T>
struct tt {
    using type = tt;

    template <typename... TS>
    using value_template_type = T<TS...>;

    evaluating tt;
};

/// f
template <template <typename...> typename F, typename... AS>
struct f {
    using type = f;
    template <typename... FS>
    using value_template_type = F<AS..., FS...>;

    evaluating f;
};

/// function
template <template <typename...> typename F, typename... AS>
struct make_function {
    evaluating tpp<F<tpp<AS>...>>;
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

/// returning
#define returning          \
    template <typename...> \
    using tpp_return =

/// __l
template <typename... TS>
struct __l {
    using type = __l;

    evaluating __l;
};

/// l
defun(l, typename... TS);

template <typename... TS>
struct l_impl {
    returning __l<TS...>;
};

/// funcall
defun(funcall, typename F, typename... AS);

template <typename F, typename... AS>
struct funcall_impl {
    returning typename F::template value_template_type<AS...>;
};

/// apply
defun(apply_aux, typename F, typename C, typename... A)

    template <typename F, typename... CS, typename... AS>
    struct apply_aux_impl<F, __l<CS...>, __l<AS...>> {
    returning funcall<F, CS..., AS...>;
};

template <typename F, typename... CS, typename A, typename B, typename... AS>
struct apply_aux_impl<F, __l<CS...>, A, B, AS...> {
    returning apply_aux<F, l<CS..., A>, B, AS...>;
};

defun(apply, typename F, typename... AS);

template <typename F, typename... AS>
struct apply_impl {
    returning apply_aux<F, l<>, AS...>;
};

/// quote
template <typename T>
struct quote {
    evaluating T;
};

template <template <typename...> typename F, typename... AS>
struct quote<F<AS...>> {
    evaluating l<f<F>, tpp<quote<AS>>...>;
};

/// qt
template <typename T>
using qt = quote<T>;

/// unquote
template <typename T>
struct unquote { };

/// uq
template <typename T>
using uq = unquote<T>;

/// quasiquote
template <typename T>
struct quasiquote {
    evaluating T;
};

template <typename T>
struct quasiquote<unquote<T>> {
    evaluating tpp<T>;
};

template <template <typename...> typename F, typename... AS>
struct quasiquote<F<AS...>> {
    evaluating l<f<F>, tpp<quasiquote<AS>>...>;
};

/// qq
template <typename T>
using qq = quasiquote<T>;

/// macro
template <template <typename...> typename M, typename... AS>
struct macro {
    using type = macro;

    evaluating tpp<tpp<M<quote<AS>...>>>;
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
    returning v<0>;
};

template <typename N, typename... NS>
struct add_impl<N, NS...> {
    returning v<N::value + tpp_v<add<NS...>>>;
};

/// mult
defun(mult, typename... NS);

template <typename... NS>
struct mult_impl {
    returning v<1>;
};

template <typename N, typename... NS>
struct mult_impl<N, NS...> {
    returning v<N::value * tpp_v<mult<NS...>>>;
};

// sub
defun(sub, typename... NS);

template <typename... NS>
struct sub_impl {
    returning v<0>;
};

template <typename N, typename... NS>
struct sub_impl<N, NS...> {
    returning v<N::value - tpp_v<sub<NS...>>>;
};

/// quotient
defun(quotient, typename... NS);

template <typename... NS>
struct quotient_impl {
    returning v<1>;
};

template <typename N, typename... NS>
struct quotient_impl<N, NS...> {
    returning v<N::value / tpp_v<quotient<NS...>>>;
};

/// is_zero
defun(is_zero, typename T);

template <typename T>
struct is_zero_impl {
    returning v<T::value == 0>;
};

/// or_logic
defun(or_logic, typename... NS);

template <typename... NS>
struct or_logic_impl {
    returning v<false>;
};

template <typename N, typename... NS>
struct or_logic_impl<N, NS...> {
    returning v<N::value || tpp_v<or_logic<NS...>>>;
};

/// and_logic
defun(and_logic, typename... NS);

template <typename... NS>
struct and_logic_impl {
    returning v<true>;
};

template <typename N, typename... NS>
struct and_logic_impl<N, NS...> {
    returning v<N::value && tpp_v<and_logic<NS...>>>;
};

/// not operator
defun(not_logic, typename B);

template <typename B>
struct not_logic_impl {
    returning v<!B::value>;
};

/// add1
defun(add1, typename N);

template <typename N>
struct add1_impl {
    returning add<N, v<1>>;
};

/// sub1
defun(sub1, typename N);

template <typename N>
struct sub1_impl {
    returning sub<N, v<1>>;
};

/// eql
defun(eql, typename N, typename M);

template <typename N, typename M>
struct eql_impl {
    returning v<N::value == M::value>;
};

/// lt
defun(lt, typename N, typename M);

template <typename N, typename M>
struct lt_impl {
    returning v<(N::value < M::value)>;
};

/// gt
defun(gt, typename N, typename M);

template <typename N, typename M>
struct gt_impl {
    returning v<(N::value > M::value)>;
};

/// le
defun(le, typename N, typename M);

template <typename N, typename M>
struct le_impl {
    returning v<N::value <= M::value>;
};

/// ge
defun(ge, typename N, typename M);

template <typename N, typename M>
struct ge_impl {
    returning v<N::value >= M::value>;
};

/// plusp
defun(plusp, typename N);

template <typename N>
struct plusp_impl {
    returning gt<N, v<0>>;
};

/// branch
template <typename C, typename T, typename E>
struct branch : branch<tpp<C>, T, E> { };

template <typename T, typename E>
struct branch<v<true>, T, E> : tpp<T> { };

template <typename T, typename E>
struct branch<v<false>, T, E> : tpp<E> { };

/// cond
defmacro(cond, typename... CT);

template <typename C, typename T, typename... ES>
struct cond_impl<C, T, ES...> {
    returning qq<branch<uq<C>, uq<T>, cond<uq<ES>...>>>;
};

/// fibonacci
defun(fibonacci, typename N);

// template <typename N>
// struct fibonacci_impl {
//     returning cond<
//         eql<N, v<0>>, v<0>,
//         eql<N, v<1>>, v<1>,
//         v<true>,add<fibonacci<sub1<N>>,
//                       fibonacci<sub<N, v<2>>>>>;
// };

template <typename N>
struct fibonacci_impl {
    returning
        branch<eql<N, v<0>>,
            v<0>,
            branch<eql<N, v<1>>,
                v<1>,
                add<fibonacci<sub1<N>>, fibonacci<sub1<sub1<N>>>>>>;
};

/// is_empty
defun(is_empty, typename C);

template <typename... TS>
struct is_empty_impl<__l<TS...>> {
    returning v<true>;
};

template <typename T, typename... TS>
struct is_empty_impl<__l<T, TS...>> {
    returning v<false>;
};

/// cons
defun(cons, typename V, typename C);

template <typename V, typename... CS>
struct cons_impl<V, __l<CS...>> {
    returning l<V, CS...>;
};

/// listp
defun(listp, typename T);

template<typename T>
struct listp_impl{
    returning v<false>;
};

template<typename... TS>
struct listp_impl<__l<TS...>>{
    returning v<true>;
};

/// car
defun(car, typename C);

template <typename C, typename... CS>
struct car_impl<__l<C, CS...>> {
    returning C;
};

/// cdr
defun(cdr, typename C);

template <typename C, typename... CS>
struct cdr_impl<__l<C, CS...>> {
    returning l<CS...>;
};

template <typename... CS>
struct cdr_impl<__l<CS...>> {
    returning l<CS...>;
};

/// caar
defun(caar, typename C);

template <typename C>
struct caar_impl {
    returning car<car<C>>;
};

/// cadr
defun(cadr, typename C);

template <typename C>
struct cadr_impl {
    returning car<cdr<C>>;
};

/// cdar
defun(cdar, typename C);

template <typename C>
struct cdar_impl {
    returning cdr<car<C>>;
};

/// cddr
defun(cddr, typename C);

template <typename C>
struct cddr_impl {
    returning cdr<cdr<C>>;
};

/// concat
defun(concat, typename C, typename D);

template <typename... CS, typename... DS>
struct concat_impl<__l<CS...>, __l<DS...>> {
    returning l<CS..., DS...>;
};

/// reverse
defun(reverse_aux, typename C, typename D);

template <typename... cs, typename... ds>
struct reverse_aux_impl<__l<cs...>, __l<ds...>> {
    returning l<ds...>;
};

template <typename c, typename... cs, typename... ds>
struct reverse_aux_impl<__l<c, cs...>, __l<ds...>> {
    returning reverse_aux<l<cs...>, l<c, ds...>>;
};

defun(reverse, typename C);

template <typename... cs>
struct reverse_impl<l<cs...>> {
    returning reverse_aux<l<cs...>, l<>>;
};

/// map_single
defun(map_single, typename F, typename C);

template <typename F, typename... CS>
struct map_single_impl<F, __l<CS...>> {
    returning l<funcall<F, CS>...>;
};

/// map
defun(map, typename F, typename... CS);

template <typename F, typename... CS>
struct map_impl {
    returning
        branch<
            // Si alguna lista es vacía
            apply<f<or_logic>, map_single<f<is_empty>, l<CS...>>>,
            // Devolvemos una lista vacía
            l<>,
            // En otro caso, aplicamos F a los primeros elementos y realizamos
            // la llamada recursiva quitando un elemento de cada lista
            cons<apply<F, map_single<f<car>, l<CS...>>>,
                apply<f<map>, F, map_single<f<cdr>, l<CS...>>>>>;
};

/// zip
defun(zip, typename... CS);

template <typename... CS>
struct zip_impl {
    returning map<f<l>, CS...>;
};

/// eval
defun(eval, typename T);

template <typename T>
struct eval_impl<t<T>> {
    returning T;
};

/// atom
defun(atom, typename T);

template <typename T>
struct atom_impl<t<T>> {
    returning v<std::is_same_v<T, tpp_t<T>>>;
};

/// simple_split
defun(simple_split, typename T);

template <template <typename...> typename T, typename... S>
struct simple_split_impl<t<T<S...>>> {
    returning l<tt<T>, t<S>...>;
};

/// split
defun(split, typename T);

template <typename T>
struct split_impl<t<T>> {

    using splitted_T = simple_split<t<T>>;

    returning branch<
        atom<t<T>>,
        t<T>,
        cons<car<splitted_T>, map<f<split>, cdr<splitted_T>>>>;
};

/// simple_join
defun(simple_join, typename T);

template <template <typename...> typename T, typename... S>
struct simple_join_impl<__l<tt<T>, t<S>...>> {
    returning t<T<S...>>;
};

/// join
defun(join, typename T);

template <typename T>
struct join_impl {
    returning branch<
        listp<T>,
        simple_join<cons<car<T>, map<f<join>, cdr<T>>>>,
        T>;
};

/// q
template <typename T>
struct q {
    evaluating T;
};
