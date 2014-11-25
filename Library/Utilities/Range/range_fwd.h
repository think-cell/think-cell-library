#pragma once

#define RANGE_PROPOSAL_NAMESPACE tc   // Todo: make this work with different names

namespace RANGE_PROPOSAL_NAMESPACE {
	template< typename T >
	struct is_range;
	
	template<typename Rng>
	struct is_range_with_iterators;
	
	template<typename It>
	class const_iterator_;

	template<typename It, typename ConstIt>
	struct iterator_base;

	namespace sub_range_impl {
		template< typename Rng >
		class sub_range;
	}
	using sub_range_impl::sub_range;

	namespace transform_adaptor_impl {
		template< typename Func, typename Rng, bool HasIterator=is_range_with_iterators< Rng >::value >
		class transform_adaptor;
	}
	using transform_adaptor_impl::transform_adaptor;

	template< typename Rng, typename Enable=void >
	struct make_sub_range_result;
}
