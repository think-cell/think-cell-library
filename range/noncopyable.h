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

#pragma once

namespace tc {
	namespace noncopyable_adl_barrier
	{
		// derived classes can be moved, but not copied
		struct noncopyable {
		protected:
			noncopyable() noexcept {}

			noncopyable(noncopyable const&) = delete;
			noncopyable& operator=(noncopyable const&) = delete;
			noncopyable(noncopyable &&) noexcept = default;
			noncopyable& operator=(noncopyable &&) & noexcept = default;
		};
	}
	using noncopyable_adl_barrier::noncopyable;

	namespace nonmovable_adl_barrier {
		struct nonmovable {
		protected:
			nonmovable() noexcept {}
			~nonmovable() = default;

			nonmovable(nonmovable const&) = delete;
			nonmovable& operator=(nonmovable const&) = delete;
			nonmovable(nonmovable &&) = delete;
			nonmovable& operator=(nonmovable &&) = delete;
		};
	}
	using nonmovable_adl_barrier::nonmovable;
}
