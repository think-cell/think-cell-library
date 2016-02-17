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
	namespace empty_chain_impl {
		// consider a CRTP such as implements_compare etc.
		//
		//		template <typename Derived, typename Chain=empty_chain>
		//		struct SomeCRTP : Chain {};
		//
		//		class A : SomeCRTP<A> {}; // implicitly derives from empty_chain
		//		class B : SomeCRTP<B> { A myA; }; // implicitly derives from empty_chain
		//
		// then, C++ does NOT allow empty base class optimization, because the compiler has to ensure
		//		std::addressof( base_cast<empty_chain>(instance_of_B) ) != std::addressof( base_cast<empty_chain>(instance_of_B.myA) )
		//
		// therefore, force different types for each instance of empty_chain, i.e.,
		//
		//		template <typename Derived, typename Chain=empty_chain<Derived>>
		//		struct SomeCRTP : Chain {};

		template <typename T>
		struct empty_chain {};
	}
	using empty_chain_impl::empty_chain;
}