#pragma once
#include "remove_cvref.h"
#include <boost/preprocessor/facilities/overload.hpp>

#define modified(obj, ...) ([&] { auto _=tc::make_copy(obj); {__VA_ARGS__;} return _; }())
