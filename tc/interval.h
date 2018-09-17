
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "container.h" 
#include "generic_macros.h"
#include "type_traits.h"
#include "counting_range.h"
#include "interval_types.h"
#include "dense_map.h"
#include "binary_operators.h"
#include "tag_type.h"
#include "round.h"
#include "partition_iterator.h"
#include "algorithm.h"

#include <chrono>
#include <set>

DEFINE_FN( length );
DEFINE_FN( inclusive_include );

namespace tc {
	// least and greatest are the values least/greatest with respect to operator< of a given type.
	// std::numeric_limits<T>::lowest/max are required to be finite, so they have a different semantics.

	template<typename T,typename Enable=void>
	struct compare_traits;

	// It is expected that the semantic of comparisons between objects is not influenced by cvref/volatile qualifiers.
	// (e.g. compare_traits<T>::least() should be <= than any T, but also any T const&, T&, etc..)
	template<typename T> struct compare_traits<T, std::enable_if_t<!tc::is_decayed<T>::value>>
		: compare_traits<tc::decay_t<T>>
	{};

	template<typename T>
	struct compare_traits<T,std::enable_if_t< tc::contiguous_enum<T>::value>> {
		static constexpr T least() noexcept {
			return tc::contiguous_enum<T>::begin();
		}
		static constexpr T greatest() noexcept {
			return tc::contiguous_enum<T>::end();
		}
	};

	template<typename T>
	struct compare_traits<T,std::enable_if_t< std::is_integral<T>::value >> {
		static constexpr T least() noexcept {
			return std::numeric_limits<T>::lowest();
		}
		static constexpr T greatest() noexcept {
			return std::numeric_limits<T>::max();
		}
	};

	template<typename T>
	struct compare_traits<T,std::enable_if_t< std::is_floating_point<T>::value >> {
		static constexpr T least() noexcept {
			return -std::numeric_limits<T>::infinity();
		}
		static constexpr T greatest() noexcept {
			return std::numeric_limits<T>::infinity();
		}
	};

	template<typename Clock, typename Duration>
	struct compare_traits<std::chrono::time_point<Clock,Duration>> {
		static constexpr std::chrono::time_point<Clock,Duration> least() noexcept {
			return std::chrono::time_point<Clock,Duration>::min();
		}
		static constexpr std::chrono::time_point<Clock,Duration> greatest() noexcept {
			return std::chrono::time_point<Clock,Duration>::max();
		}
	};

	namespace interval_adl {
		template< typename T > struct interval;
	}
	using interval_adl::interval;

	namespace no_adl {
		struct use_set_impl_tag;
		struct use_vector_impl_tag;
	}
	using no_adl::use_set_impl_tag;
	using no_adl::use_vector_impl_tag;

	namespace interval_set_adl {
		template<typename T, typename TInterval=tc::interval<T>, typename SetOrVectorImpl=use_set_impl_tag> struct interval_set;
	}
	using interval_set_adl::interval_set;

	namespace no_adl {
		template< typename T >
		struct difference {
			using type = decltype(std::declval<T>() - std::declval<T>());
		};

		TC_HAS_EXPR(minus, (T), std::declval<T>()-std::declval<T>())
	}

	namespace interval_adl {
		template<typename T>
		struct interval : tc::dense_map<tc::lohi, T>, 
#ifdef TC_PRIVATE
			PersistentTypeT<
#endif
			tc::arithmetic< tc::orable< tc::andable< > > 
#ifdef TC_PRIVATE
			>
#endif
		> {
		private:
#ifdef TC_PRIVATE
			using TPersistentBase = PersistentTypeT<
				tc::arithmetic< tc::orable< tc::andable< > > >
			>;
#endif
			using newT = tc::decay_t<T>;
			using difference_type = typename std::conditional_t<
				tc::no_adl::has_minus<T>::value
				,/*?*/tc::no_adl::difference<T const&> //Evaluation of ::type must be delayed, because it only compiles if the predicate is true
				,/*:*/boost::mpl::identity<void>
			>::type;

		public:
			using base_ = tc::dense_map<tc::lohi, T>;

			TC_DENSE_MAP_SUPPORT(interval)

			template< typename Func > 
			interval< tc::decayed_result_of_t< Func&(T) > > transform(Func func) const& MAYTHROW {
				// AssertSameOrder( *this, intvl ); see comments for AssertSameOrder(...) below
				return {func((*this)[tc::lo]), func((*this)[tc::hi])};
			}

			template< typename Func > 
			interval< tc::decayed_result_of_t< Func&(T) > > transform_sort(Func func) const& MAYTHROW {
		//		_ASSERT( !((*this)[tc::hi] < (*this)[tc::lo]) ); // see comments for AssertSameOrder(...) below
				return make_interval_sort( func((*this)[tc::lo]), func((*this)[tc::hi]) );
			}

			template< typename Func >
			auto for_each(Func func) & MAYTHROW {
				RETURN_IF_BREAK( tc::continue_if_not_break(func, (*this)[tc::lo]) );
				return tc::continue_if_not_break(func, (*this)[tc::hi]);
			}

		private:
			template< typename TPos, typename TExtent >
			static constexpr interval<T> from_extent_impl( TPos&& pos, TExtent&& extent, tc::lohi lohi ) noexcept {
				switch_no_default(lohi) {
					case tc::lo: {
						auto end = pos + std::forward<TExtent>(extent);
						static_assert(tc::is_safely_convertible<decltype(end)&&, T>::value, "Cannot initialize an interval with an unsafe conversion");
						return interval<T>(std::forward<TPos>(pos), tc_move(end));
					}
					case tc::hi: {
						auto begin = pos - std::forward<TExtent>(extent);
						static_assert(tc::is_safely_convertible<decltype(begin)&&, T>::value, "Cannot initialize an interval with an unsafe conversion");
						return interval<T>(tc_move(begin), std::forward<TPos>(pos));
					}
				}
			}

			template< bool bGeneralized, typename TPos, typename TExtent >
			static interval<T> centered_interval_impl(TPos&& pos, TExtent&& extent) noexcept {
				static_assert(std::is_same<tc::decay_t<TExtent>, difference_type>::value, "Ambiguous interpretation: convert the base position or the provided extent, so that their types match");
				// make use of the precondition center(a + c, b + c) = c + center(a, b)
				auto begin = std::forward<TPos>(pos) - tc::internal_lower_half<bGeneralized>(extent);
				auto end = begin + std::forward<TExtent>(extent);
				static_assert( tc::is_safely_convertible<decltype(begin)&&, T>::value );
				static_assert( tc::is_safely_convertible<decltype(end)&&, T>::value );
				return interval<T>( tc_move(begin), tc_move(end) );
			}

		public:
			template< typename TPos, typename TExtent >
			static constexpr interval<T> from_extent( TPos&& pos, TExtent&& extent, tc::lohi lohi ) noexcept
			{
				return from_extent_impl(std::forward<TPos>(pos), std::forward<TExtent>(extent), lohi);
			}

			template< bool bGeneralized, typename TPos, typename TExtent >
			static interval<T> centered_interval(TPos&& pos, TExtent&& extent) noexcept 
			{
				return centered_interval_impl<bGeneralized>(std::forward<TPos>(pos), std::forward<TExtent>(extent));
			}

			template< typename TPos, typename TExtent >
			static interval<T> from_extent( TPos&& pos, TExtent&& extent, EAlign ealign ) noexcept
			{
				switch_no_default(ealign) {
					case ealignLOW: {
						return from_extent_impl(std::forward<TPos>(pos), std::forward<TExtent>(extent), tc::lo);
					}
					case ealignHIGH: {
						return from_extent_impl(std::forward<TPos>(pos), std::forward<TExtent>(extent), tc::hi);
					}
					case ealignCENTER: {
						return centered_interval_impl</*bGeneralized*/false>(std::forward<TPos>(pos), std::forward<TExtent>(extent));
					}
				}
			}

			template< typename TExtent >
			void expand(TExtent&& extent) & noexcept {
				(*this)[tc::lo] -= extent;
				(*this)[tc::hi] += std::forward<TExtent>(extent);
			}

			template< typename TExtent >
			void expand_to(TExtent&& extent) {
				*this = interval<newT>::template centered_interval</*bGeneralized*/true>(tc::internal_center</*bGeneralized*/true>((*this)[tc::lo], (*this)[tc::hi]), std::forward<TExtent>(extent));
			}

			void expand_to_integer() & noexcept {
				static_assert(std::is_floating_point<T>::value);
				// Rounding an empty interval may make it non-empty, which might not be intended.
				_ASSERT(!empty());
				(*this)[tc::lo] = std::floor((*this)[tc::lo]);
				(*this)[tc::hi] = std::ceil((*this)[tc::hi]);
			}

			template <typename TTarget>
			interval<TTarget> expanding_cast() const& noexcept;

			template <typename TTarget>
			interval<TTarget> inclusive_expanding_cast() const& noexcept;

			template< typename TExtent >
			void expand_length(TExtent&& extent) & noexcept {
				expand_to(length() + std::forward<TExtent>(extent));
			}

			template< typename TExtent >
			void ensure_length(TExtent&& extent) & noexcept {
				if (length() < extent) {
					expand_to(std::forward<TExtent>(extent));
				}
			}

			bool empty() const& noexcept {
				return !( (*this)[tc::lo] < (*this)[tc::hi] );
			}

			bool inclusive_empty() const& noexcept {
				return (*this)[tc::hi] < (*this)[tc::lo];
			}

			difference_type distance(T const& t) const& noexcept {
				_ASSERT( !inclusive_empty() );
				if( t < (*this)[tc::lo] ) {
					return (*this)[tc::lo] - t;
				} else if( (*this)[tc::hi] < t ) {
					return t - (*this)[tc::hi];
				} else {
					return 0;
				}
			}

			template<typename T2>
			tc::common_reference_t<T2&&, T const&> clamp_inclusive(T2&& t) const& noexcept {
				_ASSERT( !inclusive_empty() );
				// static_cast necessary because T& does not implicitly convert to common_reference_t<T&, T&&> == T const&&
				using result_t=tc::common_reference_t<T2&&, T const&>;
				if( t<(*this)[tc::lo] ) {
					return static_cast<result_t>( (*this)[tc::lo] );
				} else if( (*this)[tc::hi]<t ) {
					return static_cast<result_t>( (*this)[tc::hi] );
				} else {
					return static_cast<result_t>(std::forward<T2>(t));
				}
			}

			template<typename T2>
			tc::common_reference_t<T2&&, T&&> clamp_inclusive(T2&& t) && noexcept {
				_ASSERT( !inclusive_empty() );
				// static_cast necessary because T& does not implicitly convert to common_reference_t<T&, T&&> == T const&&
				using result_t=tc::common_reference_t<T2&&, T&&>;
				if( t<(*this)[tc::lo] ) {
					return static_cast<result_t>( tc_move_always(*this)[tc::lo] );
				} else if( (*this)[tc::hi]<t ) {
					return static_cast<result_t>( tc_move_always(*this)[tc::hi] );
				} else {
					return static_cast<result_t>(std::forward<T2>(t));
				}
			}

			template<typename T2>
			tc::common_reference_t<T2&&, T const&&> clamp_inclusive( T2&& t ) const&& noexcept=delete;

			template<typename T2>
			tc::common_reference_t<tc::decay_t<T>, T2&&> clamp_exclusive(T2&& t) const& noexcept {
#ifdef TC_PRIVATE
				static_assert(
					!tc::is_floating_point_or_chrono_like <tc::decay_t<T2>>::value,
					"For floating-point-like types, use clamp_inclusive and clearly state what you want"
				);
#endif
				if( t<(*this)[tc::lo] ) {
					return (*this)[tc::lo];
				} else {
					return tc::min(modified((*this)[tc::hi], --_), std::forward<T2>(t));
				}
			}

			template<typename S>
			T const& closest_boundary( S const& s ) const& noexcept {
				_ASSERT( !inclusive_empty() );
				return (*this)[tc::not_if(s < center(), tc::hi)];
			}

			template<typename S>
			T const& farthest_boundary( S const& s ) const& noexcept {
				_ASSERT( !inclusive_empty() );
				return (*this)[tc::not_if(s < center(), tc::lo)];
			}

			newT center() const& noexcept {
				return tc::center((*this)[tc::lo], (*this)[tc::hi]);
			}

			decltype(auto) length() const& noexcept {
				// overflows with EmptyInterval()
#pragma warning (suppress : 4552) // '-': operator has no effect; expected operator with side-effect
				return (*this)[tc::hi] - (*this)[tc::lo];
			}

			difference_type saturated_length() const& noexcept {
				// This should also work for unsigned integers (returning length clamped to [0..max]), but this probably wouldn't be an intended behavior
				static_assert(tc::is_actual_integer<T>::value && std::is_signed<T>::value);
				return (*this)[tc::lo] < 0
					? (*this)[tc::hi] < std::numeric_limits<T>::max() + (*this)[tc::lo] ? (*this)[tc::hi] - (*this)[tc::lo] : std::numeric_limits<T>::max()
					: std::numeric_limits<T>::lowest() + (*this)[tc::lo] < (*this)[tc::hi] ? (*this)[tc::hi] - (*this)[tc::lo] : std::numeric_limits<T>::lowest();
			}

			//returns a value X such that modified(intvl, _.ensure_length(this->length())).contains(*this + X)
			difference_type OffsetToFit(interval<T> const& intvl) const& noexcept {
				_ASSERT( !empty() );
				_ASSERT( !(intvl[tc::hi]<intvl[tc::lo]) ); // intvl.begin==intvl.end is treated like intvl very small

				if( (*this)[tc::lo] < intvl[tc::lo] ) {
					auto t=intvl[tc::lo]-(*this)[tc::lo];
					if( intvl[tc::hi] < (*this)[tc::hi]+t ) {
						// if *this does not fit into intvl, center *this on intvl
						t=tc::internal_center</*bGeneralized*/true>(intvl[tc::lo],intvl[tc::hi])-tc::internal_center</*bGeneralized*/true>((*this)[tc::lo],(*this)[tc::hi]);
					}
					return t;
				} else if( intvl[tc::hi] < (*this)[tc::hi] ) {
					auto t=intvl[tc::hi]-(*this)[tc::hi];
					if( (*this)[tc::lo]+t < intvl[tc::lo] ) {
						// if *this does not fit into intvl, center *this on intvl
						t=tc::internal_center</*bGeneralized*/true>(intvl[tc::lo],intvl[tc::hi])-tc::internal_center</*bGeneralized*/true>((*this)[tc::lo],(*this)[tc::hi]);
					}
					return t;
				} else {
					return tc::explicit_cast<difference_type>(0);
				}
			}

			newT Val(EAlign ealign) const& noexcept {
				switch_no_default(ealign) {
					case ealignLOW:
						return (*this)[tc::lo];
					case ealignHIGH:
						return (*this)[tc::hi];
					case ealignCENTER:
						return center();
				}
			}

			template<typename SetOrVectorImpl>
			bool contains( interval_set<T, interval<T>, SetOrVectorImpl> const& intvlset ) const& noexcept {
				return tc::empty(intvlset) || contains( intvlset.bound_interval() );
			}

			template<typename U>
			bool contains( interval<U> const& intvl ) const& noexcept {
				return intvl.empty() || !( intvl[tc::lo]<(*this)[tc::lo] || (*this)[tc::hi]<intvl[tc::hi] );
			}

			template<typename S> 
			bool contains(S const& t) const& noexcept {
				return !(t<(*this)[tc::lo]) && t<(*this)[tc::hi];
			}

			template<typename S> 
			bool inclusive_contains(S const& t) const& noexcept {
				return !(t<(*this)[tc::lo] || (*this)[tc::hi]<t);
			}

			template<typename SetOrVectorImpl>
			bool intersects( interval_set<T, interval<T>, SetOrVectorImpl> const& intvlset ) const& noexcept {
				auto itinterval=intvlset.upper_bound(*this);
				return (itinterval!=tc::end(intvlset) && intersects(*itinterval)) 
					|| (itinterval!=tc::begin(intvlset) && intersects(*(--itinterval)));
			}

			template<typename S> 
			bool intersects(interval<S> const& intvl) const& noexcept {
				bool const bLhsEndFirst= (*this)[tc::hi]<intvl[tc::hi];
				if( (*this)[tc::lo]<intvl[tc::lo] ) {
					return ( bLhsEndFirst ? intvl[tc::lo] < (*this)[tc::hi] : intvl[tc::lo] < intvl[tc::hi] );
				} else {
					return ( bLhsEndFirst ? (*this)[tc::lo] < (*this)[tc::hi] : (*this)[tc::lo] < intvl[tc::hi] );
				}
			}

			interval<T>& operator&=(interval<T> const& intvl) & noexcept {
				tc::assign_max( (*this)[tc::lo], intvl[tc::lo] );
				tc::assign_min( (*this)[tc::hi], intvl[tc::hi] );
				return *this;
			}

			void CanonicalEmpty() & noexcept {
				if (empty()) {
					*this = EmptyInterval();
				}
			}

			void inclusive_include(newT const& t) & noexcept {
				_ASSERT( EmptyInterval()==*this || !inclusive_empty() );
				tc::assign_min( (*this)[tc::lo], t );
				tc::assign_max( (*this)[tc::hi], t );
			}

			interval<T>& operator|=(interval<T> const& intvl) & noexcept {
				_ASSERTDEBUG( EmptyInterval()==*this || !inclusive_empty() );
				_ASSERTDEBUG( EmptyInterval()==intvl || !intvl.inclusive_empty() );
				tc::assign_min( (*this)[tc::lo], intvl[tc::lo] );
				tc::assign_max( (*this)[tc::hi], intvl[tc::hi] );
				return *this;
			}
			template<typename S, std::enable_if_t<tc::generic_operator_helper::has_compound_addable<T&, S const&>::value>* = nullptr>
			interval<T>& operator+=(S const& t) & noexcept {
				_ASSERTDEBUG(boost::implicit_cast<void const*>(std::addressof((*this)[tc::lo])) != boost::implicit_cast<void const*>(std::addressof(t)));
				(*this)[tc::lo]+=t;
				(*this)[tc::hi]+=t;
				return *this;
			}

			template<typename S, std::enable_if_t<tc::generic_operator_helper::has_compound_subtractable<T&, S const&>::value>* = nullptr>
			interval<T>& operator-=(S const& t) & noexcept {
				_ASSERTDEBUG(boost::implicit_cast<void const*>(std::addressof((*this)[tc::lo])) != boost::implicit_cast<void const*>(std::addressof(t)));
				(*this)[tc::lo]-=t;
				(*this)[tc::hi]-=t;
				return *this;
			}

			template< typename S, std::enable_if_t<std::is_arithmetic<S>::value && tc::generic_operator_helper::has_compound_multipliable<T&, S const&>::value>* = nullptr>
			interval<T>& operator*=(S const& t) & noexcept {
				_ASSERTDEBUG(boost::implicit_cast<void const*>(std::addressof((*this)[tc::lo])) != boost::implicit_cast<void const*>(std::addressof(t)));
				(*this)[tc::lo]*=t;
				(*this)[tc::hi]*=t;
				if (t < 0) {
					boost::swap( (*this)[tc::lo], (*this)[tc::hi] );
				}
				return *this;
			}

			template< typename S, std::enable_if_t<std::is_arithmetic<S>::value && tc::generic_operator_helper::has_compound_dividable<T&, S const&>::value>* = nullptr>
			interval<T>& operator/=(S const& t) & noexcept {
				_ASSERT(0 < t); // What is the meaning of negative divisor? Swap (*this)[tc::lo]/(*this)[tc::hi]?
				(*this)[tc::lo]=tc::scale_div((*this)[tc::lo],t);
				(*this)[tc::hi]=tc::scale_div((*this)[tc::hi],t);
				return *this;
			}

			interval<newT> operator-() const& noexcept {
				interval<newT> copy=*this;
				copy.negate();
				return copy;
			}

			void negate() & noexcept {
				-tc::inplace((*this)[tc::lo]);
				-tc::inplace((*this)[tc::hi]);
				boost::swap((*this)[tc::lo],(*this)[tc::hi]);
			}

			auto range() const& noexcept {
				return tc::make_counting_range( (*this)[tc::lo], (*this)[tc::hi] );
			}

			static interval<newT> EmptyInterval() noexcept { // neutral element for operator|
				return interval<newT>( tc::compare_traits<newT>::greatest(), tc::compare_traits<newT>::least() );
			}

			static interval<newT> FullInterval() noexcept { // neutral element for operator&
				return interval<newT>( tc::compare_traits<newT>::least(), tc::compare_traits<newT>::greatest() );
			}

#ifdef TC_PRIVATE
			void append_to(tc::SReportStream& rs) const& noexcept {
				tc::append(rs, "interval(", (*this)[tc::lo], ";", (*this)[tc::hi], ")");
			}

			PERSISTSTRUCTDECL(interval, TPersistentBase);
#endif
		};

		template<typename T>
		template<typename TTarget>
		interval<TTarget> interval<T>::inclusive_expanding_cast() const& noexcept {
			_ASSERT(!inclusive_empty());
			return {
				tc::rounding_cast<TTarget>( (*this)[tc::lo], tc::roundFLOOR ),
				tc::rounding_cast<TTarget>( (*this)[tc::hi], tc::roundCEIL )
			};
		}

		template<typename T>
		template<typename TTarget>
		interval<TTarget> interval<T>::expanding_cast() const& noexcept {
			_ASSERT(!empty()); // Rounding an empty interval may make it non-empty, which might not be intended.
			return inclusive_expanding_cast<TTarget>();
		}

#ifdef TC_PRIVATE
		DOSAVESTARTTMPL(interval, ((typename)(T)) )
			SAVEMEMBERTAG( (*this)[tc::lo], XMLSTR("begin") )
			SAVEMEMBERTAG( (*this)[tc::hi], XMLSTR("end") )
		DOSAVEEND
		LOADCHILDSTARTBEGINTMPL(interval, ((typename)(T)) )
			LOADMEMBERTAG( (*this)[tc::lo], XMLSTR("begin") )
			LOADMEMBERTAG( (*this)[tc::hi], XMLSTR("end") )
		LOADCHILDSTARTEND
#endif
	}

	template<typename T>
	struct decay< tc::interval<T> > {
		using type = tc::interval< tc::decay_t<T> >;
	};

// In addition to intervals where the values of .begin and .end represent definite geometric positions in a 1-dimenional, ordered universe modelled by T, we use
// interval<T> for ordered pairs of T where values form abstract positions in a more general sense, e.g., interval< Ref<CGridline> >.
//
// For intervals of the second kind, T::operator< might not exist at all, or - as in Ref<CGridline> - may implement a different ordering than the one we mean.
// One must not perform any operation involving operator< on such intervals. This includes algebraic operators, tests for emptiness and intersection, but also
// AssertSameOrder, which was formerly used for checking the result of interval<T>::transform. We clearly want to use interval<T>::transform for intervals of the
// second kind, so I took it out. -- Valentin
//
// Alternative paths (discussion with Volker, Vladimir, Edgar):
//
// - "use a std::pair for intervals of the second type"
//   BAD IDEA, obscures semantic, we cleary want some parts of the interval interface, e.g., transform.
//
// - "intervals of non-fundamental T are of second type"
//   COUNTEREXAMPLE: gridvalue.
//
// - "intervals of fundamental T are of first type". (path taken by Volker for former implementation of AssertSameOrder, see below)
//   COUNTEREXAMPLE: ForEachPentagonSpan, [tc::lo] and [tc::hi] are gridline anchor indices.
//
// - "customize interval with a predicate for less"
//   Blows up the type for something that was never needed until today (with exception of AssertSameOrder). In addition, the order may be incomputable
//   from the values of T alone.
//
// - "provide a second interval type implementing the non-arithmetic parts of the interval interface only"
//   Sensible, could be considered later.
//		- Make sure intervals of the first kind can be passed as intervals of the second kind (could use inheritance).
//		- In generic code, can we always tell which type of interval interface should be used (in particular, when passing intervals to some facility specified by user)?

//template<typename T1, typename T2>
//std::enable_if_t< std::is_arithmetic<T1>::value && std::is_arithmetic<T2>::value >
//AssertSameOrder(interval<T1> const& intvl1, interval<T2> const& intvl2) {
//	_ASSERTEQUAL( tc::empty(intvl1), tc::empty(intvl2) );
//}
//
//template<typename T1, typename T2>
//std::enable_if_t<!( std::is_arithmetic<T1>::value && std::is_arithmetic<T2>::value )>
//AssertSameOrder(interval<T1> const& intvl1, interval<T2> const& intvl2) {
//	/* cannot evaluate order */;
//}

	template<typename T, std::enable_if_t<!std::is_floating_point<T>::value>* = nullptr>
	T nextafter(T const& t) noexcept {
		_ASSERT(t != std::numeric_limits<T>::max());
		return boost::next(t);
	}

	template<typename T, std::enable_if_t<std::is_floating_point<T>::value>* = nullptr>
	T nextafter(T const& t) noexcept {
		return std::nextafter(t, std::numeric_limits<T>::infinity());
	}

	// tc::common_type_t is already decayed
	template<typename T1, typename T2>
	constexpr auto make_interval(T1&& begin, T2&& end) noexcept return_ctor(
		interval< tc::common_type_t< T1 BOOST_PP_COMMA() T2 > >, (std::forward<T1>(begin), std::forward<T2>(end))
	)

	template<typename T1, typename T2>
	interval< tc::common_type_t< T1, T2 > > make_interval_sort(T1&& t1, T2&& t2) noexcept {
		tc::assert_not_isnan(t1);
		tc::assert_not_isnan(t2);
		if( t2 < t1 ) {
			return interval< tc::common_type_t< T1, T2 > >(std::forward<T2>(t2), std::forward<T1>(t1));
		} else {
			return interval< tc::common_type_t< T1, T2 > >(std::forward<T1>(t1), std::forward<T2>(t2));
		}
	}

	template<typename T, typename Extent>
	auto make_interval(T&& pos, Extent&& extent, EAlign ealign) noexcept
		return_decltype( interval< tc::decay_t<T> >::from_extent(std::forward<T>(pos), std::forward<Extent>(extent), ealign) )
	
	template<typename T, typename Extent>
	constexpr auto make_interval(T&& pos, Extent&& extent, tc::lohi lohi) noexcept
		return_decltype( interval< tc::decay_t<T> >::from_extent(std::forward<T>(pos), std::forward<Extent>(extent), lohi) )

	template<typename T, typename Extent>
	auto make_centered_interval(T&& pos, Extent&& extent) noexcept
		return_decltype(interval< tc::decay_t<T> >::template centered_interval</*bGeneralized*/false>(std::forward<T>(pos), std::forward<Extent>(extent) ) )

	template<typename Func>
	auto make_interval_func(Func func) noexcept
		return_decltype(make_interval(func(tc::lo), func(tc::hi)))

	template<typename T>
	auto make_singleton_interval(T&& value) noexcept {
		auto valueNext = tc::nextafter(value);
		return interval<tc::decay_t<T>>(std::forward<T>(value), tc_move(valueNext));
	}

	template<typename T>
	auto make_empty_interval(T&& value) noexcept
		return_decltype(make_interval(std::forward<T>(value), tc::decay_copy(value)))

	template< typename T, typename Rng >
	interval<T> bound_interval( Rng const& rngintvl ) noexcept {
		return tc::accumulate(rngintvl, interval<T>::EmptyInterval(), fn_assign_bit_or());
	}

	template<typename Rng>
	auto minmax_interval(Rng&& rng) noexcept {
		auto pairit = std::minmax_element(tc::begin(rng), tc::end(rng));
		return tc::make_interval(*pairit.first, *pairit.second);
	}

	template<typename T>
	bool empty(interval<T> const&) noexcept = delete;

	template< typename Rng, typename T >
	auto slice_by_interval(Rng&& rng, tc::interval<T> const& intvl) noexcept return_decltype(
		tc::slice(std::forward<Rng>(rng),
			tc::begin_next(rng, tc::explicit_cast<typename boost::range_size<std::remove_reference_t<Rng>>::type>(intvl[tc::lo])),
			tc::begin_next(rng, tc::explicit_cast<typename boost::range_size<std::remove_reference_t<Rng>>::type>(intvl[tc::hi]))
		)
	)

	namespace no_adl {
		template<typename TInterval>
		struct less_begin final {
			bool operator()(TInterval const& _Left, TInterval const& _Right) const& noexcept {
				return _Left[tc::lo]<_Right[tc::lo];
			}
		};

		DEFINE_TAG_TYPE(use_set_impl_tag)
		DEFINE_TAG_TYPE(use_vector_impl_tag)

		// deprecated, had disadvantage that you can only sort by one criterion, not different criterions in different parts of the code
		template<typename T, typename _Pr>
		struct vector_as_set : tc::vector< T > {
			using iterator = typename boost::range_iterator<tc::vector<T>>::type;
			using const_iterator = typename boost::range_iterator<tc::vector<T> const>::type;
			using base_ = tc::vector< T >;
	
			iterator lower_bound(T const& t) & noexcept {
				return tc::lower_bound<tc::return_border>(*this, t, _Pr());
			}

			const_iterator lower_bound(T const& t) const& noexcept {
				return tc::lower_bound<tc::return_border>(*this, t, _Pr());
			}

			iterator upper_bound(T const& t) & noexcept {
				return tc::upper_bound<tc::return_border>(*this, t, _Pr());
			}

			const_iterator upper_bound(T const& t) const& noexcept {
				return tc::upper_bound<tc::return_border>(*this, t, _Pr());
			}

			template< typename A0 >
			iterator emplace_hint(iterator& itWhere, A0&& a0) & noexcept {
				iterator it = base_::emplace(itWhere, std::forward<A0>(a0) );
				itWhere = it;
				++itWhere;
				return it;
			}

			template< typename A0, typename A1 >
			iterator emplace_hint(iterator& itWhere, A0&& a0, A1&& a1) & noexcept {
				iterator it = tc::vector< T >::emplace(itWhere, std::forward<A0>(a0), std::forward<A1>(a1) );
				itWhere = it;
				++itWhere;
				return it;
			}
		};
	}

	namespace interval_set_adl {
		template<typename T, typename TInterval, typename SetOrVectorImpl>
		struct interval_set : private
			tc::setlike< tc::equality_comparable<interval_set<T, TInterval, SetOrVectorImpl>> >
		{
		private:
			using Cont=std::conditional_t<
				std::is_same< SetOrVectorImpl, use_set_impl_tag >::value,
				std::set< TInterval, tc::no_adl::less_begin< TInterval > >,
				tc::no_adl::vector_as_set< TInterval, tc::no_adl::less_begin< TInterval > >
			>;
			Cont m_cont;

		public:
			using const_iterator = typename boost::range_iterator<Cont const>::type;
	
			interval_set() noexcept
			{}

			explicit interval_set(TInterval const& intvl) noexcept {
				*this |= intvl;
			}

			template<typename Iterator>
			interval_set( Iterator itBegin, Iterator itEnd ) noexcept 
				: m_cont( itBegin, itEnd )
			{}

			const_iterator begin() const& noexcept {
				return tc::begin(m_cont);
			}

			const_iterator end() const& noexcept {
				return tc::end(m_cont);
			}

			bool empty() const& noexcept {
				return tc::empty(m_cont);
			}

			TInterval bound_interval() const& noexcept {
				_ASSERT(!empty());
				return TInterval(tc_front(*this)[tc::lo], tc_back(*this)[tc::hi]);
			}

			interval_set& operator|=(TInterval const& interval) & noexcept {
				if (!interval.empty()) {
					auto itinterval = m_cont.upper_bound(interval);

					typename boost::range_iterator<Cont>::type itintervalAdd;

					if (itinterval != tc::begin(m_cont)) {
						itintervalAdd = itinterval;
						--itintervalAdd;
						if ((*itintervalAdd)[tc::hi] < interval[tc::lo]) {
							itintervalAdd = m_cont.emplace_hint(itinterval, interval);
						} else if (!((*itintervalAdd)[tc::hi] < interval[tc::hi])) {
							return *this;
						}
					} else {
						itintervalAdd = m_cont.emplace_hint(itinterval, interval);
					}

					for (; itinterval != tc::end(m_cont) && !(interval[tc::hi] < (*itinterval)[tc::hi]); ) {
						itinterval = m_cont.erase(itinterval);
					}

					if (itinterval != tc::end(m_cont) && !(interval[tc::hi] < (*itinterval)[tc::lo])) {
						tc::as_mutable(*itintervalAdd)[tc::hi] = (*itinterval)[tc::hi];
						m_cont.erase(itinterval);
					} else {
						tc::as_mutable(*itintervalAdd)[tc::hi] = interval[tc::hi];
					}
				}
				return *this;
			}

			interval_set& operator-=(TInterval const& interval) & noexcept {
				if (!interval.empty()) {
					auto itinterval = m_cont.lower_bound(interval);

					if (itinterval != tc::begin(m_cont)) {
						auto itintervalOverlap = itinterval;
						--itintervalOverlap;
						if (interval[tc::lo] < (*itintervalOverlap)[tc::hi]) {
							if (interval[tc::hi] < (*itintervalOverlap)[tc::hi]) {
								TInterval intervalRemainder(interval[tc::hi], (*itintervalOverlap)[tc::hi]);
								tc::as_mutable(*itintervalOverlap)[tc::hi] = interval[tc::lo];
								m_cont.emplace_hint(itinterval, intervalRemainder);
								return *this;
							} else {
								tc::as_mutable(*itintervalOverlap)[tc::hi] = interval[tc::lo];
							}
						}
					}

					while (itinterval != tc::end(m_cont) && !(interval[tc::hi] < (*itinterval)[tc::hi])) {
						itinterval = m_cont.erase(itinterval);
					}

					if (itinterval != tc::end(m_cont) && (*itinterval)[tc::lo] < interval[tc::hi]) {
						TInterval intervalInsert(interval[tc::hi], (*itinterval)[tc::hi]);
						_ASSERT(!intervalInsert.empty());
						itinterval = m_cont.erase(itinterval);
						m_cont.emplace_hint(itinterval, intervalInsert);
					}
				}
				return *this;
			}

			interval_set& operator-=(interval_set const& intvlset) & noexcept {
				tc::for_each(intvlset.m_cont, [this]( TInterval const& intvl ) noexcept {
					*this-=intvl;
				});
				return *this;
			}

			void erase(T const& t) & noexcept {
				erase(TInterval(t, boost::next(t)));
			}

			bool intersects(TInterval const& intvl) const& noexcept {
				const_iterator it=upper_bound(intvl);
				return (it!=end() && it->intersects(intvl))
				|| (it!=begin() && (--it)->intersects(intvl));
			}

			bool contains(TInterval const& interval) const& noexcept {
				if (interval.empty()) {
					return true;
				}

				auto itinterval = m_cont.upper_bound(interval);
				if(itinterval==tc::begin(m_cont)) {
					return false;
				}
				--itinterval;

				return !((*itinterval)[tc::hi]<interval[tc::hi]);
			}

			bool contains(T const& t) const& noexcept {
				if (empty()) {
					return false;
				}
				auto itinterval = m_cont.upper_bound( TInterval(t,t) );
				if( itinterval == tc::begin(m_cont) ) return false;
				--itinterval;
				return t < (*itinterval)[tc::hi];
			}

			interval_set& operator&=(TInterval const& interval) & noexcept {
				// special case not for correctness, but only to avoid superfluous insert below
				if (interval.empty()) {
					tc::cont_clear(m_cont);
					return *this;
				}

				//erase -inf to interval[tc::lo]
				auto itinterval = m_cont.lower_bound(interval);
				if( itinterval!=tc::begin(m_cont) ) {
					auto itintervalPartial=itinterval;
					--itintervalPartial;
					if( interval[tc::lo] < (*itintervalPartial)[tc::hi] ) {
						itinterval=tc::cont_must_emplace_before( m_cont, itinterval, interval[tc::lo], (*itintervalPartial)[tc::hi] );
					}
				}
				m_cont.erase( tc::begin(m_cont), itinterval );

				//erase interval[tc::hi] to inf
				itinterval = m_cont.lower_bound(TInterval( interval[tc::hi], interval[tc::hi] ) );
				if( itinterval!=tc::begin(m_cont) ) {
					auto itintervalPartial=itinterval;
					--itintervalPartial;
					if( interval[tc::hi] < (*itintervalPartial)[tc::hi] ) {
						tc::as_mutable(*itintervalPartial)[tc::hi]=interval[tc::hi];
					}
				}
				tc::take_inplace( m_cont, itinterval );
				return *this;
			}

			const_iterator interval_below( T const& t ) const& noexcept {
				_ASSERT( !tc::empty(m_cont) );

				auto itinterval = m_cont.upper_bound( TInterval(t,t) );

				_ASSERT( itinterval!=tc::begin(m_cont) ); // there must be interval space at or below t

				--itinterval;
				return itinterval;
			}


			// closest_below must exist
			T const& closest_below( T const& t) const& noexcept {
				_ASSERT( !tc::empty(m_cont) );

				const_iterator itinterval = interval_below(t);

				if( t < (*itinterval)[tc::hi]) {
					return t;
				} else {
					return (*itinterval)[tc::hi];
				}
			}

			const_iterator interval_above( T const& t ) const& noexcept {
				_ASSERT( !tc::empty(m_cont) );

				auto itintervalSup = m_cont.upper_bound( TInterval(t,t) );
		
				if( itintervalSup==tc::begin(m_cont) ) {
					return itintervalSup;
				} else {
					auto itintervalInf = itintervalSup;
					--itintervalInf;
					if( t<(*itintervalInf)[tc::hi] ) {
						return itintervalInf;
					} else {
						_ASSERT( itintervalSup!=tc::end(m_cont) ); // there must be interval space at or above t
						return itintervalSup;
					}
				}
			}

			// closest_above must exist
			T const& closest_above( T const& t) const& noexcept {
				_ASSERT( !tc::empty(m_cont) );

				auto itinterval = m_cont.upper_bound( TInterval(t,t) );

				if( itinterval != tc::begin(m_cont) && t < (*boost::prior(itinterval))[tc::hi] ) {
					return t;
				} else {
					return (*itinterval)[tc::lo];
				}
			}

			T const& closest_missing_above( T const& t) const& noexcept {
				auto itinterval = m_cont.upper_bound( TInterval(t,t) );

				if( itinterval != tc::begin(m_cont) ) {
					--itinterval;
					if( t < (*itinterval)[tc::hi] ) {
						return (*itinterval)[tc::hi];
					}
				}
				return t;
			}

			T const& closest_missing_below( T const& t) const& noexcept {
				auto itinterval = m_cont.upper_bound( TInterval(t,t) );

				if( itinterval != tc::begin(m_cont) ) {
					--itinterval;
					if( t < (*itinterval)[tc::hi] ) {
						return (*itinterval)[tc::lo];
					}
				}
				return t;
			}

			T const& closest_missing(T const& t, tc::lohi lohi) const& noexcept {
				if( tc::lo==lohi ) {
					return closest_missing_below(t);
				} else {
					return closest_missing_above(t);
				}
			}

			// returns end() if empty()
			const_iterator closest_interval( T const& t ) const& noexcept {
				auto itintervalSup = m_cont.upper_bound( TInterval(t,t) );

				// below first one or empty
				if( itintervalSup == tc::begin(m_cont) ) return itintervalSup;

				auto itintervalInf=itintervalSup;
				--itintervalInf;
				if( itintervalSup==tc::end(m_cont) || t-(*itintervalInf)[tc::hi] < (*itintervalSup)[tc::lo]-t ) {
					return itintervalInf;
				} else {
					return itintervalSup;
				}
			}

			T const& closest( T const& t) const& noexcept {
				_ASSERT( !tc::empty(m_cont) );

				auto itintervalSup = m_cont.upper_bound( TInterval(t,t) );

				// below first one
				if( itintervalSup == tc::begin(m_cont) ) return (*itintervalSup)[tc::lo];

				auto itintervalInf=itintervalSup;
				--itintervalInf;
				if( t < (*itintervalInf)[tc::hi]) {
					return t;
				} else if( itintervalSup==tc::end(m_cont) || t-(*itintervalInf)[tc::hi] < (*itintervalSup)[tc::lo]-t ) {
					return (*itintervalInf)[tc::hi];
				} else {
					return (*itintervalSup)[tc::lo];
				}
			}

			T strict_closest(T const& t) const& noexcept {
				static_assert(std::is_integral<T>::value);
				_ASSERT( !tc::empty(m_cont) );

				auto itintervalSup = m_cont.upper_bound( TInterval(t,t) );
				if( itintervalSup == tc::begin(m_cont) ) return (*itintervalSup)[tc::lo];

				T const& tInfEnd = (*boost::prior(itintervalSup))[tc::hi];
				if( t < tInfEnd ) {
					return t;
				} else {
					T tInfLast=tInfEnd;
					--tInfLast;
					if( itintervalSup == tc::end(m_cont) || t - tInfLast < (*itintervalSup)[tc::lo] - t ) {
						return tInfLast;
					} else {
						return (*itintervalSup)[tc::lo];
					}
				}
			}

			template<typename OtherSetOrVectorImpl>
			bool contains( interval_set<T, TInterval, OtherSetOrVectorImpl> const& intvlset ) const& noexcept {
				if( tc::empty(intvlset) ) return true;
				if( empty() ) return false;

				auto itintervalA = tc::begin(m_cont);
				auto itintervalB = tc::begin(intvlset.m_cont);
				for(;;) {
					if( (*itintervalA)[tc::hi]<(*itintervalB)[tc::hi] ) {
						++itintervalA;
						if( itintervalA==tc::end(m_cont) ) return false;
					} else {
						if( (*itintervalB)[tc::lo]<(*itintervalA)[tc::lo] ) return false;
						++itintervalB;
						if( itintervalB==tc::end(intvlset.m_cont) ) return true;
					}
				}
			}

			friend bool operator==( interval_set<T, TInterval, SetOrVectorImpl> const& lhs, interval_set<T, TInterval, SetOrVectorImpl> const& rhs ) noexcept {
				return tc::equal(lhs,rhs);
			}

			interval_set operator~() const& noexcept {
				interval_set intvlset;
				if(empty()) {
					tc::cont_must_emplace( intvlset.m_cont, TInterval::FullInterval() );
				} else {
					const_iterator itintervalPrevious=begin();
					const_iterator itinterval=itintervalPrevious;
					++itinterval;

					TInterval intvl( tc::compare_traits<T>::least(), (*itintervalPrevious)[tc::lo] );
					if(!intvl.empty()) tc::cont_must_emplace_before( intvlset.m_cont, tc::end(intvlset.m_cont), intvl );
			
					for(; itinterval!=end(); ++itinterval) {
						intvl=TInterval( (*itintervalPrevious)[tc::hi], (*itinterval)[tc::lo] );
						_ASSERT(!intvl.empty());
						tc::cont_must_emplace_before( intvlset.m_cont, tc::end(intvlset.m_cont), intvl );
				
						itintervalPrevious=itinterval;
					}

					intvl=TInterval( (*itintervalPrevious)[tc::hi], tc::compare_traits<T>::greatest() );
					if(!intvl.empty()) tc::cont_must_emplace_before( intvlset.m_cont, tc::end(intvlset.m_cont), intvl );
				}
				return intvlset;
			}

			interval_set& operator|=( interval_set<T, TInterval, SetOrVectorImpl> const& intvlset) & noexcept {
				Cont cont;

				auto itintervalA = tc::begin(m_cont);
				auto itintervalB = tc::begin(intvlset.m_cont);

				for(;;) {
					if( itintervalA==end() ) {
						tc::append(cont, tc::drop(intvlset.m_cont, itintervalB));
						break;
					} else if( itintervalB==tc::end(intvlset) ) {
						tc::append(cont, tc::drop(m_cont, itintervalA));
						break;
					}

					T tIntervalBegin=tc::min( (*itintervalA)[tc::lo], (*itintervalB)[tc::lo] );
					for(;;) {
						if( (*itintervalA)[tc::hi] < (*itintervalB)[tc::lo] ) goto end_from_A;
						if( (*itintervalB)[tc::hi] < (*itintervalA)[tc::lo] ) goto end_from_B;

						if( (*itintervalA)[tc::hi] < (*itintervalB)[tc::hi] ) {
							++itintervalA;
							if( itintervalA==end() ) goto end_from_B;
						} else {
							++itintervalB;
							if( itintervalB==tc::end(intvlset) ) goto end_from_A;
						}
					}

		end_from_A:
					tc::cont_must_emplace_before( cont, tc::end(cont), tc::make_interval( tIntervalBegin, (*itintervalA)[tc::hi] ) );
					++itintervalA;
					continue;
		end_from_B:
					tc::cont_must_emplace_before( cont, tc::end(cont), tc::make_interval( tIntervalBegin, (*itintervalB)[tc::hi] ) );
					++itintervalB;
					continue;
				}
				m_cont.swap( cont );
				return *this;
			}

			auto all_intervals() const& noexcept -> Cont const& {
				return m_cont;
			}

			template<typename OtherSetOrVectorImpl, typename Func>
			auto for_each_intersecting_interval(interval_set<T, TInterval, OtherSetOrVectorImpl> const& intvlset, Func func) const& MAYTHROW  -> tc::common_type_t<
				decltype(tc::continue_if_not_break(func, *tc::begin(m_cont), *tc::begin(intvlset.m_cont), std::declval<TInterval&>())),
				INTEGRAL_CONSTANT(tc::continue_)
			> {
				if(!empty() && !intvlset.empty()) {
					const_iterator itintervalA = tc::begin(m_cont);
					const_iterator itintervalB = tc::begin(intvlset.m_cont);
					if((*itintervalA)[tc::lo]<(*itintervalB)[tc::lo]) {
						itintervalA=m_cont.upper_bound( *itintervalB );
						--itintervalA;
					} else {
						itintervalB=intvlset.m_cont.upper_bound( *itintervalA );
						--itintervalB;
					}

					while(itintervalA!=tc::end(m_cont) && itintervalB!=tc::end(intvlset.m_cont)) {
						TInterval intvl=*itintervalA & *itintervalB;
						if( !intvl.empty() ) {
							RETURN_IF_BREAK( tc::continue_if_not_break( func, *itintervalA, *itintervalB, intvl ));
						}
						if((*itintervalA)[tc::hi] < (*itintervalB)[tc::hi]) {
							++itintervalA;
						} else {
							++itintervalB;
						}
					}
				}
				return INTEGRAL_CONSTANT(tc::continue_)();
			}

			template<typename OtherSetOrVectorImpl>
			bool intersects(interval_set<T, TInterval, OtherSetOrVectorImpl> const& intvlset ) const& noexcept {
				bool bIntersects = false;
				for_each_intersecting_interval(intvlset, 
					[&]( TInterval const&, TInterval const&, TInterval const&) noexcept {
						_ASSERT(!bIntersects);
						bIntersects=true;
						return INTEGRAL_CONSTANT(tc::break_)();
					});
				return bIntersects;
			}

			template<typename OtherSetOrVectorImpl>
			interval_set<T, TInterval, SetOrVectorImpl>& operator&=( interval_set<T, TInterval, OtherSetOrVectorImpl> const& intvlset) & noexcept {
				Cont contIntersection;
				for_each_intersecting_interval(intvlset, 
					[&]( TInterval const&, TInterval const&, TInterval const& intvl ) noexcept {
						tc::cont_must_emplace_before( contIntersection, tc::end(contIntersection),intvl);
					});
				m_cont=tc_move(contIntersection);
				return *this;
			}

			friend void swap( interval_set& lhs, interval_set& rhs ) noexcept {
				boost::swap( lhs.m_cont, rhs.m_cont );
			}

			friend bool operator==(  interval_set<T, TInterval, SetOrVectorImpl> const& lhs, TInterval const& rhs ) noexcept {
				if( rhs.empty() ) {
					return lhs.empty();
				} else {
					return tc::contains_single( lhs.m_cont, rhs );
				}
			}

			// DEPRECATED
			using iterator = typename boost::range_iterator<Cont>::type;

			const_iterator lower_bound( TInterval const& intvl ) const& noexcept {
				return m_cont.lower_bound( intvl );
			}

			const_iterator upper_bound( TInterval const& intvl ) const& noexcept {
				return m_cont.upper_bound( intvl );
			}

			iterator begin() & noexcept {
				return tc::begin(m_cont);
			}

			iterator end() & noexcept {
				return tc::end(m_cont);
			}

			std::size_t interval_count() const& noexcept {
				return tc::size(m_cont);
			}

			T accumulated_length() const& noexcept {
				return tc::accumulate( tc::transform( m_cont, mem_fn_length() ), boost::implicit_cast<T>(0), fn_assign_plus() );
			}

#ifdef TC_PRIVATE
			void append_to(tc::SReportStream& rs) const& noexcept {
				tc::append(rs, "interval_set(", interval_count(), ")(");
				int i = 0;
				for( auto it=tc::begin(*this); i < 10 && it!=tc::end(*this); ++it ) {
					if( 0 < i ) tc::append(rs, ", ");
					tc::append(rs, *it);
					++i;
				}
				if( i < interval_count() ) tc::append(rs, ", ...");
				tc::append(rs, ")");
			}
#endif
		};
	}
} // namespace tc
