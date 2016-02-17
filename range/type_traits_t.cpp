//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------


#include "type_traits.h"

static_assert( tc::is_safely_convertible<int, double>::value, "" );

static_assert( std::is_convertible<double, int>::value, "" );
static_assert( !tc::is_safely_convertible<double, int>::value, "" );

static_assert( std::is_convertible<float, int>::value, "" );
static_assert( !tc::is_safely_convertible<float, int>::value, "" );

static_assert( std::is_convertible<int, unsigned int>::value, "" );
static_assert( !tc::is_safely_convertible<int, unsigned int>::value, "" );

static_assert( std::is_convertible<unsigned int, int>::value, "" );
static_assert( !tc::is_safely_convertible<unsigned int, int>::value, "" );

static_assert(std::is_convertible<int*, bool>::value, "");
static_assert(!tc::is_safely_convertible<int*, bool>::value, "");

// scoped enum (enum class)
enum class TEnumClass { a, b, c };
enum TEnum { x, y, z };
static_assert( !tc::is_safely_convertible<int, TEnumClass>::value, "" );
static_assert( !tc::is_safely_convertible<TEnumClass, int>::value, "" );

// unscoped enum (primitive enum)
enum TPrimitiveEnum { a, b, c };
static_assert( !tc::is_safely_convertible<int, TPrimitiveEnum>::value, "" );
static_assert( std::is_convertible<TPrimitiveEnum, std::underlying_type_t<TPrimitiveEnum> >::value, "" );
static_assert( !tc::is_safely_convertible<TPrimitiveEnum, std::underlying_type_t<TPrimitiveEnum>>::value, "" );

enum TPrimitiveEnum2 { l, m };
static_assert( !tc::is_safely_convertible<TPrimitiveEnum, TPrimitiveEnum2>::value, "" );

struct SBase {};
struct SDerived final : SBase {};
static_assert(std::is_convertible<SDerived, SBase>::value, "");
static_assert(!tc::is_safely_convertible<SDerived, SBase>::value, "");

struct SToInt final {
	operator int() const noexcept;
};

static_assert(!std::is_convertible<SBase, int>::value, "");
static_assert(std::is_convertible<SToInt, int>::value, "");
static_assert(tc::is_safely_convertible<SToInt, int>::value, "");

static_assert(std::is_convertible<SToInt, int const&>::value, "");
static_assert(!tc::is_safely_convertible<SToInt, int const&>::value, "");
