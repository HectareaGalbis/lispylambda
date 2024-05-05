#pragma once

#include <type_traits>
#include <iostream>


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

