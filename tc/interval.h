
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "base/generic_macros.h"
#include "base/type_traits.h"
#include "base/tag_type.h"
#include "base/integer.h"
#include "algorithm/binary_operators.h"
#include "algorithm/round.h"
#include "algorithm/algorithm.h"
#include "container/container.h" 
#include "range/iota_range.h"
#include "interval_types.h"
#include "dense_map.h"

#ifdef TC_PRIVATE
#include "Library/Persistence/PersistentType.h"
#include "Library/Persistence/PersistentStruct.h"
#endif

#include <chrono>
#include <set>

namespace tc {
	template<typename T>
	constexpr tc::interval<T> all_values_interval;

	// It is expected that the semantic of comparisons between objects is not influenced by cvref/volatile qualifiers.
	template<typename T> requires (!tc::decayed<T>)
	constexpr tc::interval<tc::decay_t<T>> all_values_interval<T> = all_values_interval<tc::decay_t<T>>;

	template<typename T> requires tc::contiguous_enum<T>::value
	constexpr tc::interval<T> all_values_interval<T> = tc::interval<T>(tc::contiguous_enum<T>::begin(), tc::contiguous_enum<T>::end());

	template<std::integral T>
	constexpr tc::interval<T> all_values_interval<T> = tc::interval<T>(std::numeric_limits<T>::lowest(), std::numeric_limits<T>::max());

	template<std::floating_point T>
	constexpr tc::interval<T> all_values_interval<T> = tc::interval<T>(-std::numeric_limits<T>::infinity(), std::numeric_limits<T>::infinity());

	template<typename Clock, typename Duration>
	constexpr tc::interval<std::chrono::time_point<Clock, Duration>> all_values_interval<std::chrono::time_point<Clock, Duration>> = tc::interval<std::chrono::time_point<Clock, Duration>>(std::chrono::time_point<Clock, Duration>::min(), std::chrono::time_point<Clock,Duration>::max());

#ifdef TC_PRIVATE
	template< typename T >
	concept chrono_like =
		tc::instance<T, std::chrono::time_point> ||
		tc::instance<T, std::chrono::duration>;

	template< typename T >
	concept floating_point_or_chrono_like =
		tc::floating_point_like<T> ||
		chrono_like<T>;
#endif

	namespace no_adl {
		struct use_set_impl_tag_t;
		struct use_vector_impl_tag_t;
	}
	using no_adl::use_set_impl_tag_t;
	using no_adl::use_vector_impl_tag_t;

	namespace interval_set_adl {
		template<typename T, typename TInterval=tc::interval<T>, typename SetOrVectorImpl=use_set_impl_tag_t> struct interval_set;
	}
	using interval_set_adl::interval_set;

	namespace no_adl {
		template< typename T >
		struct difference {
			using type = decltype(std::declval<T>() - std::declval<T>());
		};
	}
	TC_HAS_EXPR(minus, (T), std::declval<T>()-std::declval<T>())

	// TODO: consider using "nextup" and "nextdown" which are single argument C functions
	// (but only for floats) standardized in IEEE 754-2008 revision and proposed in C TS 18661-1,2

	template<typename T>
	constexpr void nextafter_inplace(T& t) noexcept {
		if constexpr( std::floating_point<T> ) {
			t = std::nextafter(t, std::numeric_limits<T>::infinity());
		} else {
			++t;
		}
	}

	template<typename T>
	constexpr void nextbefore_inplace(T& t) noexcept {
		if constexpr( std::floating_point<T> ) {
			t = std::nextafter(t, -std::numeric_limits<T>::infinity());
		} else {
			--t;
		}
	} 

	template<typename T1, typename T2>
	[[nodiscard]] constexpr interval<tc::common_type_t<T1, T2>> make_interval_sort(T1&& t1, T2&& t2) noexcept;

	namespace interval_detail::no_adl {
		struct swap_if_negative_scalar final : tc::no_prepost_scalar_operation {
			template<typename T, typename Scalar>
			static void post(tc::interval<T>& intvl, Scalar const& scalar) {
				if( scalar < tc::explicit_cast<Scalar>(0) ) intvl.swap();
			}
		};
	}

	namespace interval_adl {
		template<typename T>
		struct interval
			: tc::dense_map<tc::lohi, T>
			,
#ifdef TC_PRIVATE
			  PersistentTypeT<
#endif
				tc::scalar_multipliable<
					tc::scalar_dividable<
						tc::scalar_addable<tc::scalar_subtractable<>>,
						/*nTransformDepth*/0,
						interval_detail::no_adl::swap_if_negative_scalar
					>,
					/*nTransformDepth*/0,
					interval_detail::no_adl::swap_if_negative_scalar
				>
#ifdef TC_PRIVATE
			> 
#endif
		{
		private:
			using newT = tc::decay_t<T>;
			using difference_type = typename std::conditional_t<
				tc::has_minus<T>
				,/*?*/tc::no_adl::difference<T const&> //Evaluation of ::type must be delayed, because it only compiles if the predicate is true
				,/*:*/boost::mpl::identity<void>
			>::type;

		public:
			using base_ = typename interval::dense_map;

			TC_DENSE_MAP_SUPPORT(interval)

			template< typename Func > 
			[[nodiscard]] constexpr auto transform_sort(Func func) const& MAYTHROW {
				// _ASSERT( !((*this)[tc::hi] < (*this)[tc::lo]) ); // see comments for AssertSameOrder(...) below
				return tc::make_interval_sort( func((*this)[tc::lo]), func((*this)[tc::hi]) );
			}

		private:
			template< typename TPos, typename TExtent >
			static constexpr interval<T> from_extent_impl( TPos&& pos, TExtent&& extent, tc::lohi lohi ) noexcept {
				switch_no_default(lohi) {
					case tc::lo: {
						auto end = pos + std::forward<TExtent>(extent);
						static_assert(tc::safely_convertible_to<decltype(end)&&, T>, "Cannot initialize an interval with an unsafe conversion");
						return interval<T>(std::forward<TPos>(pos), tc_move(end));
					}
					case tc::hi: {
						auto begin = pos - std::forward<TExtent>(extent);
						static_assert(tc::safely_convertible_to<decltype(begin)&&, T>, "Cannot initialize an interval with an unsafe conversion");
						return interval<T>(tc_move(begin), std::forward<TPos>(pos));
					}
				}
			}

			template< bool bGeneralized, typename TPos, typename TExtent >
			static constexpr interval<T> centered_interval_impl(TPos&& pos, TExtent&& extent) noexcept {
				STATICASSERTSAME(tc::decay_t<TExtent>, difference_type, "Ambiguous interpretation: convert the base position or the provided extent, so that their types match");
				// make use of the precondition center(a + c, b + c) = c + center(a, b)
				auto begin = std::forward<TPos>(pos) - tc::internal_lower_half<bGeneralized>(extent);
				auto end = begin + std::forward<TExtent>(extent);
				static_assert( tc::safely_convertible_to<decltype(begin)&&, T> );
				static_assert( tc::safely_convertible_to<decltype(end)&&, T> );
				return interval<T>( tc_move(begin), tc_move(end) );
			}

		public:
			template< typename TPos, typename TExtent >
			[[nodiscard]] static constexpr interval<T> from_extent( TPos&& pos, TExtent&& extent, tc::lohi lohi ) noexcept
			{
				return from_extent_impl(std::forward<TPos>(pos), std::forward<TExtent>(extent), lohi);
			}

			template< bool bGeneralized, typename TPos, typename TExtent >
			[[nodiscard]] static constexpr interval<T> centered_interval(TPos&& pos, TExtent&& extent) noexcept 
			{
				return centered_interval_impl<bGeneralized>(std::forward<TPos>(pos), std::forward<TExtent>(extent));
			}

			template< typename TPos, typename TExtent >
			[[nodiscard]] static constexpr interval<T> from_extent( TPos&& pos, TExtent&& extent, EAlign ealign ) noexcept
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
			constexpr void expand(TExtent&& extent) & noexcept {
				(*this)[tc::lo] -= extent;
				(*this)[tc::hi] += std::forward<TExtent>(extent);
			}

			template< typename TExtent >
			constexpr void expand_to(TExtent&& extent) {
				*this = interval<newT>::template centered_interval</*bGeneralized*/true>(tc::internal_midpoint</*bGeneralized*/true>((*this)[tc::lo], (*this)[tc::hi]), std::forward<TExtent>(extent));
			}

			constexpr void expand_to_integer() & noexcept {
				static_assert(std::floating_point<T>);
				// Rounding an empty interval may make it non-empty, which might not be intended.
				_ASSERTE(!empty());
				(*this)[tc::lo] = std::floor((*this)[tc::lo]);
				(*this)[tc::hi] = std::ceil((*this)[tc::hi]);
			}

			template <typename TTarget>
			[[nodiscard]] interval<TTarget> expanding_cast() const& noexcept;

			template <typename TTarget>
			[[nodiscard]] interval<TTarget> expanding_cast_inclusive() const& noexcept;

			template< typename TExtent >
			constexpr void expand_length(TExtent&& extent) & noexcept {
				expand_to(length() + std::forward<TExtent>(extent));
			}

			template< typename TExtent >
			constexpr void ensure_length(TExtent&& extent) & noexcept {
				if (length() < extent) {
					expand_to(std::forward<TExtent>(extent));
				}
			}

			constexpr void swap() & noexcept {
				tc::swap((*this)[tc::lo], (*this)[tc::hi]);
			}

			[[nodiscard]] constexpr bool empty() const& noexcept {
				return !( (*this)[tc::lo] < (*this)[tc::hi] );
			}

			[[nodiscard]] constexpr bool empty_inclusive() const& noexcept {
				return (*this)[tc::hi] < (*this)[tc::lo];
			}

			[[nodiscard]] constexpr difference_type distance(T const& t) const& noexcept {
				_ASSERTE( !empty_inclusive() );
				if( t < (*this)[tc::lo] ) {
					return (*this)[tc::lo] - t;
				} else if( (*this)[tc::hi] < t ) {
					return t - (*this)[tc::hi];
				} else {
					return 0;
				}
			}

		private:
			template<typename Self, typename S, typename R = tc::common_reference_xvalue_as_ref_t<tc::apply_cvref_t<T, Self&&>, S&&>>
			static R clamp_inclusive_(Self&& self, S&& s) noexcept {
				_ASSERT( !self.empty_inclusive() );
				if( s<self[tc::lo] ) {
					return static_cast<R>(tc_move_if_owned(self)[tc::lo]); // static_cast to convert from const& to const&&.
				} else if( self[tc::hi]<s ) {
					return static_cast<R>(tc_move_if_owned(self)[tc::hi]);
				} else {
					return static_cast<R>(tc_move_if_owned(s));
				}
			}

			template<typename Self, typename S>
			static tc::apply_cvref_t<T, Self&&> closest_boundary_(Self&& self, S&& s) noexcept {
				_ASSERTE( !self.empty_inclusive() );
				return tc_move_if_owned(self)[tc::not_if(s < self.midpoint(), tc::hi)];
			}

			template<typename Self, typename S>
			static tc::apply_cvref_t<T, Self&&> farthest_boundary_(Self&& self, S&& s) noexcept {
				_ASSERTE( !self.empty_inclusive() );
				return tc_move_if_owned(self)[tc::not_if(s < self.midpoint(), tc::lo)];
			}

			template<typename Self, typename S>
			static tc::apply_cvref_t<T, Self&&> opposite_boundary_(Self&& self, S&& s) noexcept {
				return tc_move_if_owned(self)[tc::not_if(self[tc::lo] == s || (_ASSERTE(self[tc::hi] == s), false), tc::lo)];
			}

		public:
			RVALUE_THIS_OVERLOAD_MOVABLE_MUTABLE_REF(clamp_inclusive)
			RVALUE_THIS_OVERLOAD_MOVABLE_MUTABLE_REF(closest_boundary)
			RVALUE_THIS_OVERLOAD_MOVABLE_MUTABLE_REF(farthest_boundary)
			RVALUE_THIS_OVERLOAD_MOVABLE_MUTABLE_REF(opposite_boundary)

			template<typename T2>
			[[nodiscard]] constexpr tc::common_type_t<T, T2&&> clamp_exclusive(T2&& t) const& noexcept {
				static_assert(
#ifdef TC_PRIVATE
					!tc::floating_point_or_chrono_like<tc::decay_t<decltype(t)>>,
#else
					!tc::floating_point_like<tc::decay_t<decltype(t)>>,
#endif
					"For floating-point-like types, use clamp_inclusive and clearly state what you want"
				);
				if( t<(*this)[tc::lo] ) {
					return (*this)[tc::lo];
				} else {
					return tc::min(tc_modified((*this)[tc::hi], --_), std::forward<T2>(t));
				}
			}

			[[nodiscard]] constexpr newT midpoint() const& noexcept {
				return tc::midpoint((*this)[tc::lo], (*this)[tc::hi]);
			}

			[[nodiscard]] constexpr newT midpoint_rounded() const& noexcept {
				return tc::internal_midpoint</*bGeneralized*/true>((*this)[tc::lo], (*this)[tc::hi]);
			}

			[[nodiscard]] constexpr decltype(auto) length() const& noexcept {
				// overflows with EmptyInterval()
MODIFY_WARNINGS(((suppress)(4552))) // '-': operator has no effect; expected operator with side-effect
				return (*this)[tc::hi] - (*this)[tc::lo];
			}

			[[nodiscard]] constexpr difference_type saturated_length() const& noexcept {
				// This should also work for unsigned integers (returning length clamped to [0..max]), but this probably wouldn't be an intended behavior
				static_assert(tc::actual_integer<T> && std::is_signed<T>::value);
				return (*this)[tc::lo] < 0
					? (*this)[tc::hi] < std::numeric_limits<T>::max() + (*this)[tc::lo] ? (*this)[tc::hi] - (*this)[tc::lo] : std::numeric_limits<T>::max()
					: std::numeric_limits<T>::lowest() + (*this)[tc::lo] < (*this)[tc::hi] ? (*this)[tc::hi] - (*this)[tc::lo] : std::numeric_limits<T>::lowest();
			}

			//returns a value X such that tc_modified(intvl, _.ensure_length(this->length())).contains(*this + X)
			[[nodiscard]] constexpr difference_type OffsetToFit(interval<T> const& intvl) const& noexcept {
				_ASSERTE( !empty_inclusive() ); // intvl[tc::lo]==intvl[tc::hi] is treated like intvl very small
				if( (*this)[tc::lo] < intvl[tc::lo] ) {
					auto t=intvl[tc::lo]-(*this)[tc::lo];
					if( intvl[tc::hi] < (*this)[tc::hi]+t ) {
						// if *this does not fit into intvl, center *this on intvl
						t=tc::internal_midpoint</*bGeneralized*/true>(intvl[tc::lo],intvl[tc::hi])-tc::internal_midpoint</*bGeneralized*/true>((*this)[tc::lo],(*this)[tc::hi]);
					}
					return t;
				} else if( intvl[tc::hi] < (*this)[tc::hi] ) {
					auto t=intvl[tc::hi]-(*this)[tc::hi];
					if( (*this)[tc::lo]+t < intvl[tc::lo] ) {
						// if *this does not fit into intvl, center *this on intvl
						t=tc::internal_midpoint</*bGeneralized*/true>(intvl[tc::lo],intvl[tc::hi])-tc::internal_midpoint</*bGeneralized*/true>((*this)[tc::lo],(*this)[tc::hi]);
					}
					return t;
				} else {
					tc_return_cast( 0 );
				}
			}

			constexpr void FitInsideOf(interval<T> const& intvl) & noexcept {
				tc_auto_cref(tLength, length());
				if(!(tLength < intvl.length())) {
					*this = intvl;
				} else if((*this)[tc::lo] < intvl[tc::lo]) {
					(*this)[tc::hi]=intvl[tc::lo]+tLength;
					(*this)[tc::lo]=intvl[tc::lo];
					_ASSERTE(!(intvl[tc::hi]<(*this)[tc::hi]));
				} else if(intvl[tc::hi] < (*this)[tc::hi]) {
					(*this)[tc::lo]=intvl[tc::hi]-tLength;
					(*this)[tc::hi]=intvl[tc::hi];
					_ASSERTE(!((*this)[tc::lo]<intvl[tc::lo]));
				}
			}

			tc::dense_map<tc::lohi, T> margin_to_outer(interval<T> const& intvlOuter) const& noexcept {
				return tc::dense_map<tc::lohi, T>(
					(*this)[tc::lo]-intvlOuter[tc::lo],
					intvlOuter[tc::hi]-(*this)[tc::hi]
				);
			}

			[[nodiscard]] constexpr newT Val(EAlign ealign) const& noexcept {
				switch_no_default(ealign) {
					case ealignLOW:
						return (*this)[tc::lo];
					case ealignHIGH:
						return (*this)[tc::hi];
					case ealignCENTER:
						return midpoint();
				}
			}

			template<typename SetOrVectorImpl>
			[[nodiscard]] bool contains( interval_set<T, interval<T>, SetOrVectorImpl> const& intvlset ) const& noexcept {
				return tc::empty(intvlset) || contains( intvlset.bound_interval() );
			}

			template<typename U>
			[[nodiscard]] constexpr bool naive_contains( interval<U> const& intvl ) const& noexcept {
				// no special treatment for empty or inverse intervals
				return !( intvl[tc::lo]<(*this)[tc::lo] || (*this)[tc::hi]<intvl[tc::hi] );
			}

			template<typename U>
			[[nodiscard]] constexpr bool contains( interval<U> const& intvl ) const& noexcept {
				return intvl.empty() || naive_contains(intvl);
			}

			template<typename S> 
			[[nodiscard]] constexpr bool contains(S const& t) const& noexcept {
				return !(t<(*this)[tc::lo]) && t<(*this)[tc::hi];
			}

			template<typename S> 
			[[nodiscard]] constexpr bool contains_inclusive(S const& t) const& noexcept {
				return !(t<(*this)[tc::lo] || (*this)[tc::hi]<t);
			}

			template<typename S>
			[[nodiscard]] constexpr bool contains_exclusive(S const& t) const& noexcept {
				return (*this)[tc::lo] < t && t < (*this)[tc::hi];
			}

			template<typename SetOrVectorImpl>
			[[nodiscard]] bool intersects( interval_set<T, interval<T>, SetOrVectorImpl> const& intvlset ) const& noexcept {
				auto itinterval=intvlset.upper_bound(*this);
				return (itinterval!=tc::end(intvlset) && intersects(*itinterval)) 
					|| (itinterval!=tc::begin(intvlset) && intersects(*(--itinterval)));
			}

			template<typename S> 
			[[nodiscard]] constexpr bool intersects(interval<S> const& intvl) const& noexcept {
				bool const bLhsEndFirst= (*this)[tc::hi]<intvl[tc::hi];
				if( (*this)[tc::lo]<intvl[tc::lo] ) {
					return ( bLhsEndFirst ? intvl[tc::lo] < (*this)[tc::hi] : intvl[tc::lo] < intvl[tc::hi] );
				} else {
					return ( bLhsEndFirst ? (*this)[tc::lo] < (*this)[tc::hi] : (*this)[tc::lo] < intvl[tc::hi] );
				}
			}

			template<typename S> 
			[[nodiscard]] constexpr bool naive_intersects(interval<S> const& intvl) const& noexcept {
				// no special treatment for empty or inverse intervals
				return (*this)[tc::lo] < intvl[tc::hi] && intvl[tc::lo] < (*this)[tc::hi];
			}

			template<typename S>
			[[nodiscard]] constexpr bool intersects_inclusive(interval<S> const& intvl) const& noexcept {
				// no special treatment for empty or inverse intervals
				return !(intvl[tc::hi]<(*this)[tc::lo]) && !((*this)[tc::hi]<intvl[tc::lo]);
			}

			template<tc::instance_or_derived<tc::interval> Rhs>
			constexpr interval<T>& operator&=(Rhs&& rhs) & noexcept {
				static_assert(
					std::is_same<newT, tc::range_value_t<Rhs>>::value,
					"Are you sure you want to intersect intervals of different types? "
					"Consider tc::all_values_interval<T> != tc::all_values_interval<U>. "
					"Use implicit_cast to enable the operation."
				);
				tc::assign_max( (*this)[tc::lo], std::forward<Rhs>(rhs)[tc::lo] );
				tc::assign_min( (*this)[tc::hi], std::forward<Rhs>(rhs)[tc::hi] );
				return *this;
			}

			constexpr void CanonicalEmpty() & noexcept {
				if (empty()) {
					*this = EmptyInterval();
				}
			}

			template<typename U>
			constexpr void include(U&& u) & noexcept {
				static_assert( tc::safely_convertible_to<U&&, newT> );
				_ASSERTE( EmptyInterval()==*this || !empty() );
				newT t = std::forward<U>(u);
				tc::assign_min( (*this)[tc::lo], t );
				tc::nextafter_inplace(t);
				tc::assign_max( (*this)[tc::hi], tc_move(t) );
			}

			template<typename U>
			constexpr void include_inclusive(U&& u) & noexcept {
				_ASSERTE( EmptyInterval()==*this || !empty_inclusive() );
				tc::assign_min( (*this)[tc::lo], u );
				tc::assign_max( (*this)[tc::hi], std::forward<U>(u) );
			}

			template<tc::instance_or_derived<tc::interval> Rhs>
			constexpr interval<T>& operator|=(Rhs&& rhs) & noexcept {
				static_assert(
					std::is_same<newT, tc::range_value_t<Rhs>>::value,
					"Are you sure you want to take the union of intervals of different types? "
					"Consider interval<T>::EmptyInterval() != interval<U>::EmptyInterval(). "
					"Use implicit_cast to enable the operation."
				);
				_ASSERTDEBUG( EmptyInterval()==*this || !empty_inclusive() );
				_ASSERTDEBUG( rhs.EmptyInterval()==rhs || !rhs.empty_inclusive() );
				tc::assign_min( (*this)[tc::lo], std::forward<Rhs>(rhs)[tc::lo] );
				tc::assign_max( (*this)[tc::hi], std::forward<Rhs>(rhs)[tc::hi] );
				return *this;
			}
			[[nodiscard]] constexpr interval<newT> operator-() const& noexcept {
				interval<newT> copy=*this;
				copy.negate();
				return copy;
			}

			constexpr void negate() & noexcept {
				-tc::inplace((*this)[tc::lo]);
				-tc::inplace((*this)[tc::hi]);
				swap();
			}

			[[nodiscard]] constexpr auto range() const& noexcept {
				// tc::iota does not support end < begin. So for such empty ranges, we need to return tc::iota(begin, begin)
				return tc::iota( (*this)[tc::lo], tc::max((*this)[tc::lo], (*this)[tc::hi]) );
			}

			[[nodiscard]] static constexpr interval<newT> EmptyInterval() noexcept { // neutral element for operator|
				return interval<newT>( tc::all_values_interval<newT>[tc::hi], tc::all_values_interval<newT>[tc::lo]  );
			}

#ifdef TC_PRIVATE
		private:
			void LoadTypeImpl(CXmlReader& loadhandler) & THROW(ExLoadFail);
			friend void LoadType_impl(interval& intvl, CXmlReader& loadhandler) THROW(ExLoadFail) {
				intvl.LoadTypeImpl(loadhandler); // THROW(ExLoadFail)
			}
			LOADMETHODDECL( typename interval::PersistentType )
			SAVEMETHODDECL;
#endif
		};

		TC_DENSE_MAP_DEDUCTION_GUIDES(interval)

		template< typename Arg, typename Arg2> requires std::is_same<tc::decay_t<Arg>, tc::decay_t<Arg2>>::value
		interval(tc::lohi lohi, Arg&&, Arg2&&) -> interval<tc::decay_t<Arg>>;

		template<typename T>
		template<typename TTarget>
		interval<TTarget> interval<T>::expanding_cast_inclusive() const& noexcept {
			_ASSERT(!empty_inclusive());
			return {
				tc::rounding_cast<TTarget>( (*this)[tc::lo], tc::roundFLOOR ),
				tc::rounding_cast<TTarget>( (*this)[tc::hi], tc::roundCEIL )
			};
		}

		template<typename T>
		template<typename TTarget>
		interval<TTarget> interval<T>::expanding_cast() const& noexcept {
			_ASSERT(!empty()); // Rounding an empty interval may make it non-empty, which might not be intended.
			return expanding_cast_inclusive<TTarget>();
		}

		template<typename T>
		[[nodiscard]] constexpr tc::enumset<T> operator~(tc::interval<T> const& intvl) noexcept {
			return ~tc::enumset<T>(intvl);
		}
	}

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
//   BAD IDEA, obscures semantic, we clearly want some parts of the interval interface, e.g., transform.
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

	// tc::common_type_t is already decayed
	template<typename T1, typename T2>
	[[nodiscard]] constexpr auto make_interval(T1&& begin, T2&& end) return_ctor_MAYTHROW( // MAYTHROW if the conversion from T1 or T2 to their common_type may throw. (or their copy constructor, if they are the same type)
		TC_FWD(interval<tc::common_type_t<T1, T2>>), (std::forward<T1>(begin), std::forward<T2>(end))
	)

	template<typename T1, typename T2>
	[[nodiscard]] constexpr interval< tc::common_type_t< T1, T2 > > make_interval_sort(T1&& t1, T2&& t2) noexcept {
		tc::assert_not_isnan(t1);
		tc::assert_not_isnan(t2);
		if( t2 < t1 ) {
			return interval< tc::common_type_t< T1, T2 > >(std::forward<T2>(t2), std::forward<T1>(t1));
		} else {
			return interval< tc::common_type_t< T1, T2 > >(std::forward<T1>(t1), std::forward<T2>(t2));
		}
	}

	template<typename T, typename Extent>
	[[nodiscard]] constexpr auto make_interval(T&& pos, Extent&& extent, EAlign ealign)
		return_decltype_noexcept( interval< tc::decay_t<T> >::from_extent(std::forward<T>(pos), std::forward<Extent>(extent), ealign) )
	
	template<typename T, typename Extent>
	[[nodiscard]] constexpr auto make_interval(T&& pos, Extent&& extent, tc::lohi lohi)
		return_decltype_noexcept( interval< tc::decay_t<T> >::from_extent(std::forward<T>(pos), std::forward<Extent>(extent), lohi) )

	template<typename Rng> requires tc::is_iota_range<Rng>::value
	[[nodiscard]] constexpr auto make_interval(Rng const& rng) return_decltype_MAYTHROW(
		tc::make_interval(*tc::begin(rng), *tc::end(rng))
	)

	template<typename T, typename Extent>
	[[nodiscard]] constexpr auto make_centered_interval(T&& pos, Extent&& extent)
		return_decltype_noexcept(interval< tc::decay_t<T> >::template centered_interval</*bGeneralized*/false>(std::forward<T>(pos), std::forward<Extent>(extent) ) )

	template<typename Func>
	[[nodiscard]] constexpr auto make_interval_func(Func func)
		return_decltype_MAYTHROW(tc::make_interval(func(tc::lo), func(tc::hi)))

	template<typename T>
	[[nodiscard]] constexpr auto make_singleton_interval(T&& value) noexcept {
		return interval<tc::decay_t<T>>(std::forward<T>(value), tc_modified(value, tc::nextafter_inplace(_)));
	}

	template<typename T>
	[[nodiscard]] constexpr auto make_empty_interval(T&& value)
		return_decltype_MAYTHROW(tc::make_interval(std::forward<T>(value), tc::decay_copy(value)))

	namespace interval_adl {
		template<tc::instance_or_derived<tc::interval> Lhs, tc::instance_or_derived<tc::interval> Rhs>
		[[nodiscard]] constexpr auto operator&(Lhs&& lhs, Rhs&& rhs) noexcept {
			static_assert(
				std::is_same<tc::range_value_t<Lhs>, tc::range_value_t<Rhs>>::value,
				"Are you sure you want to intersect intervals of different types? "
				"Consider tc::all_values_interval<T> != tc::all_values_interval<U>. "
				"Use implicit_cast to enable the operation."
			);
			return tc::make_interval(
				tc::max(std::forward<Lhs>(lhs)[tc::lo], std::forward<Rhs>(rhs)[tc::lo]),
				tc::min(std::forward<Lhs>(lhs)[tc::hi], std::forward<Rhs>(rhs)[tc::hi])
			);
		}

		template<tc::instance_or_derived<tc::interval> Lhs, tc::instance_or_derived<tc::interval> Rhs>
		[[nodiscard]] constexpr auto operator|(Lhs&& lhs, Rhs&& rhs) noexcept {
			static_assert(
				std::is_same<tc::range_value_t<Lhs>, tc::range_value_t<Rhs>>::value,
				"Are you sure you want to take the union of intervals of different types? "
				"Consider interval<T>::EmptyInterval() != interval<U>::EmptyInterval(). "
				"Use implicit_cast to enable the operation."
			);
			_ASSERTE( tc::interval<tc::range_value_t<Lhs>>::EmptyInterval()==lhs || !lhs.empty_inclusive() );
			_ASSERTE( tc::interval<tc::range_value_t<Rhs>>::EmptyInterval()==rhs || !rhs.empty_inclusive() );
			return tc::make_interval(
				tc::min(std::forward<Lhs>(lhs)[tc::lo], std::forward<Rhs>(rhs)[tc::lo]),
				tc::max(std::forward<Lhs>(lhs)[tc::hi], std::forward<Rhs>(rhs)[tc::hi])
			);
		}
	}

	template< typename T, typename Rng >
	[[nodiscard]] constexpr interval<T> bound_interval( Rng const& rngintvl ) noexcept {
		return tc::accumulate(rngintvl, interval<T>::EmptyInterval(), tc::fn_assign_bit_or());
	}

	template<typename Rng, typename Pred>
	[[nodiscard]] constexpr auto minmax_interval(Rng&& rng, Pred const& pred) noexcept {
		std::optional<tc::interval<tc::range_value_t<Rng>>> ointvlMinMax;
		tc::for_each(std::forward<Rng>(rng), [&](auto&& t) noexcept {
			if(!ointvlMinMax) {
				ointvlMinMax.emplace(t, t);
			} else if(!tc::assign_better(pred, (*ointvlMinMax)[tc::lo], tc_move_if_owned(t))) {
				tc::assign_better(std::not_fn(pred), (*ointvlMinMax)[tc::hi], tc_move_if_owned(t));
			}
		});
		return *VERIFY(tc_move(ointvlMinMax));
	}

	template<typename Rng>
	[[nodiscard]] constexpr auto minmax_interval(Rng&& rng) noexcept {
		return tc::minmax_interval(std::forward<Rng>(rng), fn_less());
	}

	template<typename Rng>
	[[nodiscard]] auto minmax_interval_elements(Rng&& rng) noexcept {
		auto pairit = tc::minmax_element(rng);
		return tc::make_interval(pairit.first, pairit.second);
	}

	template<typename T>
	bool empty(interval<T> const&) noexcept = delete;

	template< typename Rng, typename T >
	[[nodiscard]] constexpr auto slice_by_interval(Rng&& rng, tc::interval<T> const& intvl) return_decltype_MAYTHROW(
		tc::slice(std::forward<Rng>(rng),
			tc::begin_next<tc::return_border>(rng, tc::explicit_cast<typename boost::range_size<std::remove_reference_t<Rng>>::type>(intvl[tc::lo])),
			tc::begin_next<tc::return_border>(rng, tc::explicit_cast<typename boost::range_size<std::remove_reference_t<Rng>>::type>(intvl[tc::hi]))
		)
	)

	namespace no_adl {
		template<typename Src, typename Dst = Src>
		struct [[nodiscard]] linear_interval_transform final {
		private:
			tc::interval<Src> m_intvlsrc;
			tc::interval<Dst> m_intvldst;

		public:
			template <typename OtherSrc, typename OtherDst>
			friend struct linear_interval_transform;
			template <typename OtherSrc, typename OtherDst, typename Shift>
			friend struct flip_and_shift_transform;

			template <typename OtherSrc, typename OtherDst>
			explicit linear_interval_transform(linear_interval_transform<OtherSrc,OtherDst> const& func) noexcept
				: tc_member_init_cast( m_intvlsrc, func.m_intvlsrc )
				, tc_member_init_cast( m_intvldst, func.m_intvldst )
			{}

			linear_interval_transform(tc::interval<Src> const& intvlsrc, tc::interval<Dst> const& intvldst) noexcept
				: m_intvlsrc(intvlsrc)
				, m_intvldst(intvldst)
			{
				if( m_intvlsrc.empty_inclusive() ) {
					m_intvlsrc.swap();
					m_intvldst.swap();
				}
				_ASSERT( m_intvlsrc[tc::lo] != m_intvlsrc[tc::hi] || m_intvldst[tc::lo] == m_intvldst[tc::hi] );
				if constexpr( std::floating_point<Src> ) {
					_ASSERT( std::isfinite(m_intvlsrc.length()) );
				}
				if constexpr( std::floating_point<Dst> ) {
					_ASSERT( std::isfinite(m_intvldst.length()) );
				}
			}

			/* Desired properties of f = interval_transform(src, dst):
				exactness: f(s[lohi]) == d[lohi]
				monotonicity: cmp(f(t0),f(t1)) * cmp(t0,t1) * cmp(src[lo],src[hi]) * cmp(dst[lo],dst[hi]) >= 0, where cmp is an arithmetic three-way comparison function
				determinacy: !isnan(f(t)) || dst[lo]==dst[hi] && !isfinite(t)
				boundedness: !src.contains_inclusive(t) || isfinite(f(t))
				consistency: !src.contains_inclusive(t) || dst.contains_inclusive(f(t))
			*/
			[[nodiscard]] Dst operator()(Src const& src) const& noexcept {
				if constexpr( tc::floating_point_like<Dst> ) {
					auto const sign = tc::not_if(m_intvlsrc.midpoint() < src, tc::sign::pos); // Because midpoint() is rounded down, src has to be strictly greater than it before we use hi interval end (important for integral Src).
					return tc::best(
						tc::directed(tc::fn_less(), tc::negate_if(m_intvldst.empty_inclusive(), sign)),
						m_intvldst[tc::lo*sign] + length_transform()(src - m_intvlsrc[tc::lo*sign]),
						m_intvldst.midpoint()
					);
				} else {
					return m_intvldst[tc::lo] + length_transform()(src - m_intvlsrc[tc::lo]);
				}
			}

			auto length_transform() const& noexcept {
				return [srcLength = m_intvlsrc.length(), dstLength = m_intvldst.length()](Src const& src) noexcept -> Dst {
					if( 0 == srcLength ) {
						// The inclusive source interval is a single point. The operations is well-defined ("0*0/0") if,
						//  * 0 == m_intvldst.length() guaranteed by constructor, and
						_ASSERTEQUAL( src, 0 );
						return 0;
					} else {
						return tc::scale_muldiv(dstLength, src, srcLength);
					}
				};
			}
			
			[[nodiscard]] linear_interval_transform<Dst, Src> inverted() const& noexcept {
				return {m_intvldst, m_intvlsrc};
			}
		};

		template<typename Src, typename Dst>
		linear_interval_transform(tc::interval<Src> const&, tc::interval<Dst> const&) -> linear_interval_transform<Src, Dst>;
	}
	using no_adl::linear_interval_transform;

	namespace no_adl {
		template<typename Src, typename Dst = Src, typename Shift = tc::common_type_t<Src, Dst>>
		struct flip_and_shift_transform {
		private:
			tc::sign m_sign;
			Shift m_shift;

		public:
			flip_and_shift_transform(tc::sign sign, Shift shift) noexcept
				: m_sign(sign)
				, m_shift(shift)
			{}

			[[nodiscard]] Dst operator()(Src const& src) const& noexcept {
				return src*m_sign + m_shift;
			}

			[[nodiscard]] flip_and_shift_transform<Dst, Src, Shift> inverted() const& noexcept {
				return {m_sign, -m_shift*m_sign};
			}

			template<typename Dst2>
			[[nodiscard]] linear_interval_transform<Src, Dst2> transform_output(linear_interval_transform<Dst, Dst2> const& func) const& noexcept {
				// tc::chained(func, *this) without intermediate rounding to Dst
				return {func.m_intvlsrc.transform(inverted()), func.m_intvldst};
			}

			template<typename Dst2, typename Shift2>
			[[nodiscard]] flip_and_shift_transform<Src, Dst2, tc::decay_t<decltype(std::declval<Shift>() + std::declval<Shift2>())>> transform_output(flip_and_shift_transform<Dst, Dst2, Shift2> const& func) const& noexcept {
				// tc::chained(func, *this) without intermediate rounding to Dst
				return {m_sign*func.m_sign, m_shift*func.m_sign + func.m_shift};
			}
		};
		template<typename Shift>
		flip_and_shift_transform(tc::sign sign, Shift shift) -> flip_and_shift_transform<Shift>;
	}
	using no_adl::flip_and_shift_transform;

	namespace no_adl {
		template<typename T, typename TInterval>
		struct less_begin final {
			bool operator()(TInterval const& intvlLeft, TInterval const& intvlRight) const& noexcept {
				return intvlLeft[tc::lo] < intvlRight[tc::lo];
			}

			using is_transparent = void;
			bool operator()(TInterval const& intvlLeft, T const& tRight) const& noexcept {
				return intvlLeft[tc::lo] < tRight;
			}
			bool operator()(T const& tLeft, TInterval const& intvlRight) const& noexcept {
				return tLeft < intvlRight[tc::lo];
			}
		};	

		// deprecated, had disadvantage that you can only sort by one criterion, not different criterions in different parts of the code
		template<typename T, typename _Pr>
		struct vector_as_set : tc::vector< T > {
			using iterator = tc::iterator_t<tc::vector<T>>;
			using const_iterator = tc::iterator_t<tc::vector<T> const>;
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
		struct interval_set :
			tc::setlike<>
		{
		private:
			using Cont=std::conditional_t<
				std::is_same< SetOrVectorImpl, use_set_impl_tag_t >::value,
				tc::set< TInterval, tc::no_adl::less_begin< T, TInterval > >,
				tc::no_adl::vector_as_set< TInterval, tc::no_adl::less_begin< T, TInterval > >
			>;
			Cont m_cont;

		public:
			using const_iterator = tc::iterator_t<Cont const>;
	
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
				return TInterval(tc::front(*this)[tc::lo], tc::back(*this)[tc::hi]);
			}

			interval_set& operator|=(TInterval const& interval) & noexcept {
				if (!interval.empty()) {
					auto itinterval = m_cont.upper_bound(interval);

					tc::iterator_t<Cont> itintervalAdd;

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
				erase(TInterval(t, tc_modified(t, ++_)));
			}

			bool intersects(TInterval const& intvl) const& noexcept {
				const_iterator it=upper_bound(intvl);
				return (it!=end() && it->intersects(intvl))
				|| (it!=begin() && (--it)->intersects(intvl));
			}

			bool contains(TInterval const& intvl) const& noexcept {
				return intvl.empty() || tc::and_then(
					upper_bound<tc::return_element_before_or_null>(intvl),
					[&](auto const& intvlInsideSet) noexcept {
						return !(intvlInsideSet[tc::hi]<intvl[tc::hi]);
					}
				);
			}

			bool contains(T const& t) const& noexcept {
				return tc::and_then(
					upper_bound<tc::return_element_before_or_null>(t),
					[&](auto const& intvlInsideSet) noexcept {
						return t<intvlInsideSet[tc::hi];
					}
				);
			}

			void erase_to(T const& t) & noexcept {
				auto itinterval = m_cont.lower_bound(t);
				if( itinterval!=tc::begin(m_cont) ) {
					auto itintervalPartial=itinterval;
					--itintervalPartial;
					if( t < (*itintervalPartial)[tc::hi] ) {
						itinterval=tc::cont_must_emplace_before( m_cont, itinterval, t, (*itintervalPartial)[tc::hi] );
					}
				}
				m_cont.erase( tc::begin(m_cont), itinterval );
			}

			void erase_from(T const& t) & noexcept {
				auto itinterval = m_cont.lower_bound(t);
				if( itinterval!=tc::begin(m_cont) ) {
					auto itintervalPartial=itinterval;
					--itintervalPartial;
					if( t < (*itintervalPartial)[tc::hi] ) {
						tc::as_mutable(*itintervalPartial)[tc::hi]=t;
					}
				}
				tc::take_inplace( m_cont, itinterval );
			}

			interval_set& operator&=(TInterval const& interval) & noexcept {
				// special case not for correctness, but only to avoid superfluous insert below
				if (interval.empty()) {
					m_cont.clear();
					return *this;
				}

				erase_to(interval[tc::lo]);
				erase_from(interval[tc::hi]);
				return *this;
			}

			const_iterator interval_below( T const& t ) const& noexcept {
				return upper_bound<tc::return_element_before>(t);
			}

			// closest_below must exist
			T const& closest_below( T const& t) const& noexcept {
				return tc::min(t, (*interval_below(t))[tc::hi]);
			}

			const_iterator interval_above( T const& t ) const& noexcept {
				_ASSERT( !tc::empty(m_cont) );

				auto itintervalSup = m_cont.upper_bound(t);
		
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

				auto itinterval = m_cont.upper_bound(t);

				if( itinterval != tc::begin(m_cont) && t < (*tc_modified(itinterval, --_))[tc::hi] ) {
					return t;
				} else {
					return (*itinterval)[tc::lo];
				}
			}

			T const& closest_missing_above( T const& t) const& noexcept {
				return closest_missing(t, tc::hi);
			}

			T const& closest_missing_below( T const& t) const& noexcept {
				return closest_missing(t, tc::lo);
			}

			T const& closest_missing(T const& t, tc::lohi lohi) const& noexcept {
				if (tc_auto_cref(itinterval, upper_bound<tc::return_element_before_or_null>(t))) {
					if( t < (*itinterval)[tc::hi] ) {
						return (*itinterval)[lohi];
					}
				}
				return t;
			}

			T closest_missing_above(T t, T const& tLength) const& noexcept {
				auto itinterval = upper_bound<tc::return_element_before_or_null>(t);
				if (itinterval) {
					t = tc::max(t, (*itinterval)[tc::hi]);
					++itinterval;
				} else {
					itinterval = tc::begin(m_cont);
				}
				while(itinterval != tc::end(m_cont) && (*itinterval)[tc::lo] < t + tLength) {
					t = (*itinterval)[tc::hi];
					++itinterval;
				}
				return t;
			}

			// returns end() if empty()
			const_iterator closest_interval( T const& t ) const& noexcept {
				auto itintervalSup = m_cont.upper_bound(t);

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

				auto itintervalSup = m_cont.upper_bound(t);

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
				static_assert(std::integral<T>);
				_ASSERT( !tc::empty(m_cont) );

				auto itintervalSup = m_cont.upper_bound(t);
				if( itintervalSup == tc::begin(m_cont) ) return (*itintervalSup)[tc::lo];

				T const& tInfEnd = (*tc_modified(itintervalSup, --_))[tc::hi];
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
					tc::cont_must_emplace( intvlset.m_cont, tc::all_values_interval<T> );
				} else {
					const_iterator itintervalPrevious=begin();
					const_iterator itinterval=itintervalPrevious;
					++itinterval;

					TInterval intvl( tc::all_values_interval<T>[tc::lo], (*itintervalPrevious)[tc::lo] );
					if(!intvl.empty()) tc::cont_must_emplace_before( intvlset.m_cont, tc::end(intvlset.m_cont), intvl );
			
					for(; itinterval!=end(); ++itinterval) {
						intvl=TInterval( (*itintervalPrevious)[tc::hi], (*itinterval)[tc::lo] );
						_ASSERT(!intvl.empty());
						tc::cont_must_emplace_before( intvlset.m_cont, tc::end(intvlset.m_cont), intvl );
				
						itintervalPrevious=itinterval;
					}

					intvl=TInterval( (*itintervalPrevious)[tc::hi], tc::all_values_interval<T>[tc::hi] );
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
				tc::constant<tc::continue_>
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
							tc_yield( func, *itintervalA, *itintervalB, intvl);
						}
						if((*itintervalA)[tc::hi] < (*itintervalB)[tc::hi]) {
							++itintervalA;
						} else {
							++itintervalB;
						}
					}
				}
				return tc::constant<tc::continue_>();
			}

			template<typename OtherSetOrVectorImpl>
			bool intersects(interval_set<T, TInterval, OtherSetOrVectorImpl> const& intvlset ) const& noexcept {
				bool bIntersects = false;
				for_each_intersecting_interval(intvlset,
					[&]( tc::unused, tc::unused, tc::unused) noexcept {
						VERIFY(tc::change(bIntersects, true));
						return tc::constant<tc::break_>();
					}
				);
				return bIntersects;
			}

			template<typename OtherSetOrVectorImpl>
			interval_set<T, TInterval, SetOrVectorImpl>& operator&=( interval_set<T, TInterval, OtherSetOrVectorImpl> const& intvlset) & noexcept {
				Cont contIntersection;
				for_each_intersecting_interval(intvlset, 
					[&]( tc::unused, tc::unused, TInterval const& intvl ) noexcept {
						tc::cont_must_emplace_before( contIntersection, tc::end(contIntersection),intvl);
					});
				m_cont=tc_move(contIntersection);
				return *this;
			}

			friend void swap( interval_set& lhs, interval_set& rhs ) noexcept {
				tc::swap( lhs.m_cont, rhs.m_cont );
			}

			friend bool operator==(  interval_set<T, TInterval, SetOrVectorImpl> const& lhs, TInterval const& rhs ) noexcept {
				if( rhs.empty() ) {
					return lhs.empty();
				} else {
					return tc::contains_single( lhs.m_cont, rhs );
				}
			}

			// DEPRECATED
			using iterator = tc::iterator_t<Cont>;

			const_iterator lower_bound( TInterval const& intvl ) const& noexcept {
				return m_cont.lower_bound( intvl );
			}

			template <typename RangeReturn = tc::return_border, typename K>
			decltype(auto) upper_bound(K const& k) const& noexcept {
				static_assert( RangeReturn::allowed_if_always_has_border );
				return RangeReturn::pack_border(m_cont.upper_bound(k), *this);
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
				return tc::accumulate( tc::transform( m_cont, tc_mem_fn(.length) ), tc::implicit_cast<T>(0), tc::fn_assign_plus() );
			}
		};
	}

	template<typename ResultWrapper, typename RngIntvl>
	[[nodiscard]] auto ordered_overlapping_intervals_impl(ResultWrapper resultwrapper, RngIntvl&& rngintvl) noexcept {
		return [rngintvl=tc::make_reference_or_value(tc_move_if_owned(rngintvl)), resultwrapper = tc_move(resultwrapper)](auto sink) noexcept {
			if(!tc::empty(*rngintvl)) {
				_ASSERTDEBUG(tc::is_sorted(*rngintvl, tc::projected(tc::fn_less(), [](auto const& intvl) noexcept { return intvl[tc::lo]; })));
				auto itintvlOverlapBegin=tc::begin(*rngintvl);
				auto itintvlLastOverlap=itintvlOverlapBegin;
				tc_return_if_break(tc::for_each(tc::make_range_of_iterators(tc::begin_next<tc::return_drop>(*rngintvl)), [&](auto const& itintvlCurr) noexcept {
					tc_auto_cref(intvlLastOverlap, *itintvlLastOverlap);
					tc_auto_cref(intvlCurr, *itintvlCurr);
					if(intvlLastOverlap[tc::hi] < intvlCurr[tc::lo]) {
						tc_yield(sink, resultwrapper(*rngintvl, itintvlOverlapBegin, itintvlCurr, itintvlLastOverlap));
						itintvlOverlapBegin=itintvlCurr;
						itintvlLastOverlap=itintvlCurr;
					} else if(intvlLastOverlap[tc::hi] < intvlCurr[tc::hi]) {
						itintvlLastOverlap=itintvlCurr;
					}
					return tc::continue_;
				}));
				tc_yield(sink, resultwrapper(*rngintvl, itintvlOverlapBegin, tc::end(*rngintvl), itintvlLastOverlap));
			}
			return tc::continue_;
		};
	}

	template<typename RngIntvl>
	[[nodiscard]] auto ordered_overlapping_intervals(RngIntvl&& rngintvl) noexcept {
		return ordered_overlapping_intervals_impl(
			[](auto const& rngintvl, auto const& itBegin, auto const& itEnd, tc::unused /*itMax*/) noexcept {
				return tc::slice(rngintvl, itBegin, itEnd);
			},
			std::forward<RngIntvl>(rngintvl)
		);
	}

	template<typename RngIntvl, typename Project>
	auto ordered_concatenated_intervals(RngIntvl&& rngintvl, Project project) noexcept {
		return ordered_overlapping_intervals_impl(
			[](tc::unused /*rng*/, auto const& itBegin, tc::unused /*itEnd*/, auto const& itMax) noexcept {
				return tc::make_interval((*itBegin.element_base())[tc::lo], (*itMax.element_base())[tc::hi]);
			},
			tc::transform(
				std::forward<RngIntvl>(rngintvl),
				[project = tc_move(project)](auto const& intvl) noexcept {
					return intvl.transform(project);
				}
			)
		);
	}
} // namespace tc
