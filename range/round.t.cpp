//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
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

#include "range.t.h"
#include "round.h"

UNITTESTDEF( rounding_cast ) {
	static_assert(std::is_same<decltype(tc::rounding_cast<int>(1, tc::roundFLOOR)), int&&>::value, "tc::rounding_cast<T> doesn't return T");
	static_assert(std::is_same<decltype(tc::rounding_cast<int>(1.0, tc::roundFLOOR)), int>::value, "tc::rounding_cast<T> doesn't return T");
	_ASSERTEQUAL(tc::rounding_cast<int>(1.0, tc::roundFLOOR), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1.0, tc::roundCEIL), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1.0, tc::roundNEAREST), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1.5, tc::roundFLOOR), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1.5, tc::roundCEIL), 2);
	_ASSERTEQUAL(tc::rounding_cast<int>(1.5, tc::roundNEAREST), 2);
	_ASSERTEQUAL(tc::rounding_cast<int>(1, tc::roundFLOOR), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1, tc::roundCEIL), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1, tc::roundNEAREST), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1.5), 2);
}
