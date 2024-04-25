
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "reference_or_value.h"
#include "../tuple.h"
#include <utility>

static_assert(std::is_trivially_copyable<tc::reference_or_value<int>>::value);
static_assert(std::is_trivially_copy_assignable<tc::reference_or_value<int>>::value);
static_assert(std::is_trivially_move_assignable<tc::reference_or_value<int>>::value);

static_assert(!std::is_trivially_copy_assignable<std::pair<int&, int&>>::value);
static_assert(!std::is_trivially_move_assignable<std::pair<int&, int&>>::value);
static_assert(!std::is_trivially_copyable<tc::reference_or_value<std::pair<int&, int&>>>::value);
static_assert(!std::is_trivially_copy_assignable<tc::reference_or_value<std::pair<int&, int&>>>::value);
static_assert(!std::is_trivially_move_assignable<tc::reference_or_value<std::pair<int&, int&>>>::value);

static_assert(std::is_trivially_copyable<tc::reference_or_value<int&>>::value);
static_assert(std::is_trivially_copy_assignable<tc::reference_or_value<int&>>::value);
static_assert(std::is_trivially_move_assignable<tc::reference_or_value<int&>>::value);

static_assert(std::is_trivially_copyable<tc::reference_or_value<tc::tuple<int, double>>>::value);
static_assert(std::is_trivially_copy_assignable<tc::reference_or_value<tc::tuple<int, double>>>::value);
static_assert(std::is_trivially_move_assignable<tc::reference_or_value<tc::tuple<int, double>>>::value);
