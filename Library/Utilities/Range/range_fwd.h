#pragma once

#include <type_traits>

#define RANGE_PROPOSAL_NAMESPACE tc   // Todo: make this work with different names

namespace RANGE_PROPOSAL_NAMESPACE {
	template< typename T >
	struct is_range;
	
	template<typename Rng>
	struct is_range_with_iterators;
	
	template<typename It>
	struct const_iterator_;

	template<typename It, typename ConstIt>
	struct iterator_base;

	namespace sub_range_impl {
		template< typename Rng >
		struct sub_range;
	}
	using sub_range_impl::sub_range;

	namespace transform_adaptor_impl {
		template< typename Func, typename Rng, bool HasIterator=is_range_with_iterators< Rng >::value >
		struct transform_adaptor;
	}
	using transform_adaptor_impl::transform_adaptor;

	template< typename Rng, typename Enable=void >
	struct make_sub_range_result;

	template<typename T>
	bool bool_cast(T const& t){
		static_assert( std::is_pointer<T>::value || std::is_class<T>::value || std::is_same<T,bool>::value, "");
		return static_cast<bool>( VERIFYINITIALIZED(t) );
	}

	struct bool_context {
		template< typename T >
		bool_context(T const& t)
			: m_b(tc::bool_cast(t))
		{}
		operator bool() const { return m_b; }
	private:
		bool const m_b;
	};
}
