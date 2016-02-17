#pragma once

#include "conversion_traits.h"
#include "renew.h"

#include <type_traits>
#include <memory>

namespace tc {
	struct aggregate_tag {}; // tag to distinguish constructors that aggregate their single argument from templated copy constructors

	template< typename T >
	struct reference_or_value {
		static_assert( !std::is_reference<T>::value, "" );

		using value_type = std::remove_cv_t<T>;
		using reference = value_type&;
		using const_reference = value_type const&;

		reference_or_value() {} // m_t may be default-constructible
		
		template< typename Rhs >
		reference_or_value( Rhs&& rhs, aggregate_tag )
		: m_t( std::forward<Rhs>(rhs) )
		{}
		
		reference best_access() const {
			// When declaring m_t non-mutable and using const_cast here be undefined behavior in code like:
			//	reference_or_value<T> _const_ foo;
			//	foo.best_access();
			// ?
			return m_t;
		}
		value_type* operator->() {
			return std::addressof(m_t);
		}
		value_type const* operator->() const {
			return std::addressof(m_t);
		}
		reference operator*() {
			return m_t;
		}
		const_reference operator*() const {
			return m_t;
		}

		/*
			Last access could be replaced by
			T&& operator*() && {
				return tc_move(m_t);
			}
			as soon as the compiler allows ref-qualifiers
		*/
		value_type&& last_access() {
			return tc_move(m_t);
		}

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
	struct reference_or_value<T&> {
	private:
		T* m_pt;

	public:
		using reference = T&;
		using const_reference = reference;

		reference_or_value(T& t, aggregate_tag)
		:	m_pt(std::addressof(t))
		{}
		reference best_access() const {
			return *m_pt;
		}
		T * operator->() const {
			return m_pt;
		}
		reference operator*() const {
			return *m_pt;
		}
		reference last_access() {
			return *m_pt;
		}
	};

}
