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

#include "renew.h"

#include <type_traits>
#include <memory>

namespace tc {
	struct aggregate_tag final {}; // tag to distinguish constructors that aggregate their single argument from templated copy constructors

	namespace reference_or_value_adl_barrier {
		template< typename T >
		struct reference_or_value final {
			static_assert( !std::is_reference<T>::value, "" );

			using value_type = std::remove_cv_t<T>;
			using reference = value_type&;
			using const_reference = value_type const&;

			reference_or_value() noexcept {} // m_t may be default-constructible
		
			template< typename Rhs >
			reference_or_value( Rhs&& rhs, aggregate_tag ) noexcept
			: m_t( std::forward<Rhs>(rhs) )
			{}

			template< typename Rhs >
			void aggregate(Rhs&& rhs) noexcept {
				m_t=std::forward<Rhs>(rhs);
			}

			reference best_access() const noexcept {
				// When declaring m_t non-mutable and using const_cast here be undefined behavior in code like:
				//	reference_or_value<T> _const_ foo;
				//	foo.best_access();
				// ?
				return m_t;
			}
			value_type* operator->() noexcept {
				return std::addressof(m_t);
			}
			value_type const* operator->() const noexcept {
				return std::addressof(m_t);
			}
			reference operator*() & noexcept {
				return m_t;
			}
			const_reference operator*() const & noexcept {
				return m_t;
			}
			value_type&& operator*() && noexcept {
				return tc_move(m_t);
			}
			value_type const&& operator*() const && noexcept = delete;

		private:
			// Store mutable T to return mutable reference from best_access().
			// best_access() is used to create an index object from an iterator
			// to m_t. add_index_interface<T>::index needs a mutable iterator.
			// This is because of a deficiency in the STL: If there was a way to
			// get a mutable reference from a const_iterator *and* a mutable T,
			// the index type could always take the const_iterator instead.
			value_type mutable m_t;
		};

		template< typename T >
		struct reference_or_value<T&> final {
		private:
			T* m_pt;

		public:
			using reference = T&;
			using const_reference = reference;

			reference_or_value(T& t, aggregate_tag) noexcept
			:	m_pt(std::addressof(t))
			{}
			void aggregate(T& t) noexcept {
				m_pt=std::addressof(t);
			}
			reference best_access() const noexcept {
				return *m_pt;
			}
			T * operator->() const noexcept {
				return m_pt;
			}
			reference operator*() const noexcept {
				return *m_pt;
			}
		};
	}
	using reference_or_value_adl_barrier::reference_or_value;
}
