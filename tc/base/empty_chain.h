
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "fundamental.h"

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
		struct TC_EMPTY_BASES empty_chain {};
	}
	using empty_chain_impl::empty_chain;
}
