#pragma once

#include "break_or_continue.h"
#include "range_defines.h"
#include "meta.h"

#include <boost/range/iterator.hpp>
#include <boost/noncopyable.hpp>

#include <functional>

namespace RANGE_PROPOSAL_NAMESPACE{

	//-------------------------------------------------------------------------------------------------------------------------
	// equal - check whether two ranges are equal - overloaded for combinations of generator and iterator based ranges

	namespace detail {

		template<typename Rng, typename Pred>
		struct is_equal_elem : boost::noncopyable {
			is_equal_elem(Rng const& rng, Pred&& pred_) : it(boost::begin(rng)), end(boost::end(rng)), pred(std::forward<Pred>(pred_)), equal(true) {}

			template<typename Elem>
			break_or_continue operator()(Elem const& elem) {
				if (it == end || !boost::implicit_cast<bool>(pred(elem, *it))) { equal = false; return break_; }
				++it;
				return continue_;
			}

			bool result() const { return equal && it == end; }

			private:
				typedef typename boost::range_iterator<Rng const>::type iterator_type;

				iterator_type it;
				iterator_type end;
				typename std::decay<Pred>::type pred;
				bool equal;
		};
	}

	template<typename LRng, typename RRng, typename Pred>
		typename std::enable_if< is_range_with_iterators< RRng >::value,
	bool >::type equal(LRng const& lrng, RRng const& rrng, Pred&& pred) {

		detail::is_equal_elem<RRng, Pred> equal(rrng, std::forward<Pred>(pred));
		ensure_index_range(lrng)(std::ref(equal));

		return equal.result();
	}

	// forward to the symetric case above
	template<typename LRng, typename RRng, typename Pred>
		typename std::enable_if< !is_range_with_iterators< RRng >::value && is_range_with_iterators< LRng >::value,
	bool >::type equal(LRng const& lrng, RRng const& rrng, Pred pred) {
		return RANGE_PROPOSAL_NAMESPACE::equal(rrng, lrng, std::bind(pred, std::placeholders::_2, std::placeholders::_1)); 
	}

	// is_arithmetic helpful for generic programming
	// only do if semantics are clear-cut
	template<typename T, typename Pred>
		typename std::enable_if< std::is_arithmetic< T >::value,
	bool >::type equal(T const& lhs, T const& rhs, Pred pred) {
		return pred(lhs,rhs);
	}

		/*
	// rule out the case we cannot handle (this would need coroutines to work)
	template<typename LRng, typename RRng, typename Pred>
	STATIC_ASSERT_OVERLOAD_NOT_SELECTED ( equal, 
											!is_range_with_iterators< LRng >::value && !is_range_with_iterators< RRng >::value,
											 "cannot traverse two generator ranges simultaneously!",
											 bool, LRng const&, RRng const&, Pred&&
										)
										*/
	// forward the non predicate version
	template<typename LRng, typename RRng>
	bool equal(LRng const& lrng, RRng const& rrng) {
		return RANGE_PROPOSAL_NAMESPACE::equal(lrng, rrng, fn_equal());
	}

}

