
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "break_or_continue.h"
#include "meta.h"
#include "size.h"
#include "type_traits.h"

#include "reference_or_value.h"
#include "static_polymorphism.h"

#pragma warning( push )
#pragma warning( disable: 4018 )
#include <boost/range/reference.hpp>
#include <boost/range/difference_type.hpp>
#include <boost/range/category.hpp>
#pragma warning( pop )

#include <boost/range/iterator_range.hpp>

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#else
#pragma warning( push )
#pragma warning( disable: 4244 )
#endif
#include <boost/iterator/counting_iterator.hpp>
#ifdef __clang__
#pragma clang diagnostic pop
#else
#pragma warning( pop )
#endif

#include <boost/mpl/has_xxx.hpp>

#include <type_traits>

namespace boost {
namespace iterators {
	template <
		typename Incrementable
		, typename CategoryOrTraversal
		, typename Difference
	>
	class counting_iterator;
}
}

namespace tc {
	namespace iterator {
		template<typename It> It middle_point(It const&, It const&) noexcept;
	}

	namespace no_adl {
		template< typename It, typename ConstIt=It >
		struct iterator_base {
			using iterator = It;
			using const_iterator = ConstIt;
			using index = It;

	/*		template< typename OtherIt, typename OtherConstIt >
			explicit iterator_base( iterator_base<OtherIt,OtherConstIt> const&, std::enable_if_t<
				std::is_convertible<OtherIt,It>::value && std::is_convertible<OtherConstIt,ConstIt>::value
			, unused_arg > =unused_arg() ) {};
			template< typename OtherIt, typename OtherConstIt, std::enable_if_t<
				std::is_convertible<OtherIt,It>::value && std::is_convertible<OtherConstIt,ConstIt>::value
			>* = nullptr> iterator_base& operator=( iterator_base<OtherIt,OtherConstIt> const& ) & noexcept { return *this; }
	*/
			typename std::iterator_traits<iterator>::reference dereference_index(index const& idx) & noexcept {
				return *idx;
			}

			// We cannot require that dereference_index(...) const returns the same type as const_iterator::operator*()
			// because we use the stronger iterator as index for mutable legacy ranges, which returns reference, with is not required
			// to be at all related to const_reference.
			// Simply casting iterator to const_iterator and dereferencing does not work either because references do not outlive
			// their iterators.
			// TODO: It is probably most sensible to impose the same requirement to this return value as to const_reference,
			// namely that it is convertible to value_type. A proxy encapsulating const_iterator, which we can create from iterator, would be a fine implementation then.
			std::conditional_t<
				std::is_convertible<
					typename std::iterator_traits<iterator>::reference,
					typename std::iterator_traits<const_iterator>::reference
				>::value,
				typename std::iterator_traits<const_iterator>::reference,
				typename std::iterator_traits<iterator>::value_type
			> constexpr dereference_index(index const& idx) const& noexcept {
				return *idx;
			}

			constexpr bool equal_index(index const& idxLhs, index const& idxRhs) const& noexcept {
				return idxLhs==idxRhs;
			}

			constexpr void increment_index(index& idx) const& noexcept {
				++idx;
			}

			constexpr void decrement_index(index& idx) const& noexcept {
				--idx;
			}

			constexpr void advance_index(index& idx, typename std::iterator_traits<iterator>::difference_type d) const& noexcept {
				idx+=d;
			}

			constexpr typename std::iterator_traits<iterator>::difference_type distance_to_index(index const& idxLhs, index const& idxRhs) const& noexcept {
				return idxRhs-idxLhs;
			}

			constexpr void middle_point( index & idxBegin, index const& idxEnd ) const& noexcept {
				idxBegin=tc::iterator::middle_point( idxBegin, idxEnd );
			}

			constexpr iterator make_iterator( index idx ) & noexcept {
				return idx;
			}

			constexpr const_iterator make_iterator( index idx ) const& noexcept {
				return idx;
			}
		};
	}
	using no_adl::iterator_base;

	BOOST_MPL_HAS_XXX_TRAIT_DEF(index)

	////////////////////////////////////
	// adding index to legacy ranges

	namespace range_generator_from_index_impl {
		struct empty_chain {};

		template<
			typename Derived,
			typename Chain=empty_chain
		>
		struct range_generator_from_index : Chain {
		private:
			using this_type = range_generator_from_index;
		public:
			STATIC_VIRTUAL_CONSTEXPR(begin_index)
			STATIC_VIRTUAL_CONSTEXPR(at_end_index)

			template< typename Func >
			auto operator()(Func func) /* no & */ MAYTHROW {
				return [&]() MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, this->dereference_index(begin_index()))), INTEGRAL_CONSTANT(tc::continue_)> {
					for( auto idx=begin_index();
						!at_end_index(idx);
						this->increment_index(idx)
					) {
						RETURN_IF_BREAK( tc::continue_if_not_break( func, this->dereference_index(idx) ) );
					}
					return INTEGRAL_CONSTANT(tc::continue_)();
				}();
			}

			template< typename Func >
			auto operator()(Func func) const /* no & */ MAYTHROW {
				return [&]() MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, this->dereference_index(begin_index()))), INTEGRAL_CONSTANT(tc::continue_)> {
					for( auto idx=begin_index();
						!at_end_index(idx);
						this->increment_index(idx)
					) {
						RETURN_IF_BREAK( tc::continue_if_not_break( func, this->dereference_index(idx) ) );
					}
					return INTEGRAL_CONSTANT(tc::continue_)();
				}();
			}
		};
	}
	using range_generator_from_index_impl::range_generator_from_index;

	namespace no_adl {
		template<typename Func, typename Rng, typename Enable=void>
		struct has_mem_fn_chunk final: std::false_type {};

		template<typename Func, typename Rng>
		struct has_mem_fn_chunk<Func, Rng, tc::void_t<decltype(std::declval<Func>().chunk(std::declval<Rng>()))>> final: std::true_type {};
	}
	using no_adl::has_mem_fn_chunk;

	namespace void_generator_type_check_impl {
		template<typename Func>
		struct ensure_non_breaking_functor final: tc::decay_t<Func> {
			using base_ = tc::decay_t<Func>;
			
			constexpr explicit ensure_non_breaking_functor(Func&& func) noexcept: base_(std::forward<Func>(func)) {}

			template<typename... Args>
			constexpr decltype(auto) operator()(Args&&... args) & MAYTHROW {
				static_assert(
					!std::is_same<
						decltype(tc::base_cast<base_>(*this)(std::forward<Args>(args)...)),
						break_or_continue
					>::value &&
					!std::is_same<
						decltype(tc::base_cast<base_>(*this)(std::forward<Args>(args)...)),
						INTEGRAL_CONSTANT(tc::break_)
					>::value,
					"Functor may return break_, but range does not support it."
				);
				return tc::base_cast<base_>(*this)(std::forward<Args>(args)...); // MAYTHROW
			}

			template<typename Rng, std::enable_if_t<tc::has_mem_fn_chunk<base_&, Rng>::value>* = nullptr>
			constexpr decltype(auto) chunk(Rng&& rng) & MAYTHROW {
				static_assert(
					!std::is_same<
						decltype(tc::base_cast<base_>(*this).chunk(std::forward<Rng>(rng))),
						break_or_continue
					>::value &&
					!std::is_same<
						decltype(tc::base_cast<base_>(*this).chunk(std::forward<Rng>(rng))),
						INTEGRAL_CONSTANT(tc::break_)
					>::value,
					"Functor may return break_, but range does not support it."
				);
				return tc::base_cast<base_>(*this).chunk(std::forward<Rng>(rng)); // MAYTHROW
			}
		};
		template<typename Func>
		struct ensure_always_breaking_functor final: tc::decay_t<Func> {
			using base_ = tc::decay_t<Func>;

			constexpr explicit ensure_always_breaking_functor(Func&& func) noexcept: base_(std::forward<Func>(func)) {}

			template<typename... Args>
			constexpr decltype(auto) operator()(Args&&... args) & MAYTHROW {
				static_assert(
					std::is_same<
						decltype(tc::base_cast<base_>(*this)(std::forward<Args>(args)...)),
						INTEGRAL_CONSTANT(tc::break_)
					>::value
				);
				return tc::base_cast<base_>(*this)(std::forward<Args>(args)...); // MAYTHROW
			}
			
			template<typename Rng, std::enable_if_t<tc::has_mem_fn_chunk<base_&, Rng>::value>* = nullptr>
			constexpr decltype(auto) chunk(Rng&& rng) & MAYTHROW {
				static_assert(
					std::is_same<
						decltype(tc::base_cast<base_>(*this).chunk(std::forward<Rng>(rng))),
						INTEGRAL_CONSTANT(tc::break_)
					>::value
				);
				return tc::base_cast<base_>(*this).chunk(std::forward<Rng>(rng)); // MAYTHROW
			}
		};
	}

	template<typename Func, std::enable_if_t<tc::is_instance<void_generator_type_check_impl::ensure_non_breaking_functor, std::remove_reference_t<Func>>::value>* = nullptr>
	constexpr decltype(auto) make_ensure_non_breaking_functor(Func&& func) noexcept {
		return std::forward<Func>(func);
	}

	template<typename Func,std::enable_if_t<!tc::is_instance<void_generator_type_check_impl::ensure_non_breaking_functor, std::remove_reference_t<Func>>::value>* = nullptr>
	constexpr auto make_ensure_non_breaking_functor(Func&& func) noexcept {
		return void_generator_type_check_impl::ensure_non_breaking_functor<Func>(std::forward<Func>(func));
	}

	template<typename Func, std::enable_if_t<tc::is_instance<void_generator_type_check_impl::ensure_always_breaking_functor, std::remove_reference_t<Func>>::value>* = nullptr>
	constexpr decltype(auto) make_ensure_always_breaking_functor(Func&& func) noexcept {
		return std::forward<Func>(func);
	}

	template<typename Func,std::enable_if_t<!tc::is_instance<void_generator_type_check_impl::ensure_always_breaking_functor, std::remove_reference_t<Func>>::value>* = nullptr>
	constexpr auto make_ensure_always_breaking_functor(Func&& func) noexcept {
		return void_generator_type_check_impl::ensure_always_breaking_functor<Func>(std::forward<Func>(func));
	}

	namespace no_adl {
		template<typename Rng, typename Enable=void >
		struct index final {
			static_assert( !std::is_reference<Rng>::value );
			using type=typename boost::range_iterator<Rng>::type;
		};

		template<typename Rng>
		struct index<Rng, std::enable_if_t< has_index< Rng >::value > > final  {
			static_assert( !std::is_reference<Rng>::value );
			using type=typename Rng::index;
		};

		template<typename Rng>
		using index_t=typename index<Rng>::type;
	}
	using no_adl::index_t;

	namespace begin_index_impl {
		template<typename Rng, std::enable_if_t< !has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
		constexpr decltype(auto) begin_index(Rng&& rng) MAYTHROW {
			return tc::begin(std::forward<Rng>(rng));
		}

		template<typename Rng, std::enable_if_t< has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
		constexpr decltype(auto) begin_index(Rng&& rng) MAYTHROW {
			return std::forward<Rng>(rng).begin_index();
		}
	}

	template<typename Rng>
	constexpr decltype(auto) begin_index(tc::reference_or_value<Rng> const& rng) MAYTHROW {
		return begin_index_impl::begin_index(rng.best_access());
	}

	namespace end_index_impl {
		template<typename Rng, std::enable_if_t< !has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
		constexpr decltype(auto) end_index(Rng&& rng) MAYTHROW {
			return tc::end(std::forward<Rng>(rng));
		}

		template<typename Rng, std::enable_if_t< has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
		constexpr decltype(auto) end_index(Rng&& rng) MAYTHROW {
			return std::forward<Rng>(rng).end_index();
		}
	}

	template<typename Rng>
	constexpr decltype(auto) end_index(tc::reference_or_value<Rng> const& rng) MAYTHROW {
		return end_index_impl::end_index(rng.best_access());
	}

	template<typename Rng, typename It, std::enable_if_t< !has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
	constexpr bool at_end_index(Rng const& rng, It const& it) MAYTHROW {
		return it==tc::end(rng);
	}

	template<typename Rng, typename Index, std::enable_if_t< has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
	constexpr bool at_end_index(Rng const& rng, Index const& idx) MAYTHROW {
		return rng.at_end_index(idx);
	}

	template<typename Rng, typename ItLhs, typename ItRhs, std::enable_if_t< !has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
	constexpr auto equal_index(Rng const&, ItLhs const& itLhs, ItRhs const& itRhs) MAYTHROW return_decltype(
		itLhs==itRhs
	)

	template<typename Rng, typename IndexLhs, typename IndexRhs, std::enable_if_t< has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
	constexpr auto equal_index(Rng const& rng, IndexLhs const& idxLhs, IndexRhs const& idxRhs) MAYTHROW return_decltype(
		rng.equal_index(idxLhs, idxRhs)
	)

	TC_HAS_EXPR(equal_index, (Rng), tc::equal_index(std::declval<Rng const&>(),tc::begin_index_impl::begin_index(std::declval<Rng const&>()),tc::begin_index_impl::begin_index(std::declval<Rng const&>())))

	template<typename Rng, typename It, std::enable_if_t< !has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
	constexpr auto dereference_index(Rng&& rng, It&& it) MAYTHROW -> decltype(*tc::begin(std::forward<Rng>(rng))) {
		static_assert(tc::is_safely_convertible<decltype(*std::forward<It>(it)), decltype(*tc::begin(std::forward<Rng>(rng))) >::value);
		return *std::forward<It>(it);
	}

	template<typename Rng, typename Index, std::enable_if_t< has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
	constexpr decltype(auto) dereference_index(Rng&& rng, Index&& idx) MAYTHROW {
		return std::forward<Rng>(rng).dereference_index(std::forward<Index>(idx));
	}

	template<typename Rng, typename It, std::enable_if_t< !has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
	constexpr void increment_index(Rng const&, It& it) MAYTHROW {
		++it;
	}

	template<typename Rng, typename Index, std::enable_if_t< has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
	constexpr void increment_index(Rng const& rng, Index& idx) MAYTHROW {
		rng.increment_index(idx);
	}

	template<typename Rng, typename It, std::enable_if_t< !has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
	constexpr void decrement_index(Rng const&, It& it) MAYTHROW {
		--it;
	}

	template<typename Rng, typename Index, std::enable_if_t< has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
	constexpr void decrement_index(Rng const& rng, Index& idx) MAYTHROW {
		rng.decrement_index(idx);
	}

	template<typename Rng, typename It, typename Difference, std::enable_if_t< !has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
	constexpr void advance_index(Rng const&, It& it, Difference&& d) MAYTHROW {
		it+=std::forward<Difference>(d);
	}

	template<typename Rng, typename Index, typename Difference, std::enable_if_t< has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
	constexpr void advance_index(Rng const& rng, Index& idx, Difference&& d) MAYTHROW {
		rng.advance_index(idx, std::forward<Difference>(d));
	}

	template<typename Rng, typename IndexLhs, typename IndexRhs, std::enable_if_t< has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
	constexpr decltype(auto) distance_to_index(Rng const& rng, IndexLhs const& idxLhs, IndexRhs const& idxRhs) MAYTHROW {
		return rng.distance_to_index(idxLhs,idxRhs);
	}

	template<typename Rng, typename ItLhs, typename ItRhs, std::enable_if_t< !has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
	constexpr auto distance_to_index(Rng const&, ItLhs const& itLhs, ItRhs const& itRhs) MAYTHROW {
		return itRhs-itLhs;
	}

	template<typename Rng, typename ItLhs, typename ItRhs, std::enable_if_t< !has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
	constexpr void middle_point(Rng const&, ItLhs& itLhs, ItRhs const& itRhs) MAYTHROW {
		itLhs=tc::iterator::middle_point( tc::as_const(itLhs), itRhs );
	}

	template<typename Rng, typename IndexLhs, typename IndexRhs, std::enable_if_t< has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
	constexpr void middle_point(Rng const& rng, IndexLhs& idxLhs, IndexRhs const& idxRhs) MAYTHROW {
		rng.middle_point(idxLhs,idxRhs);
	}

	template<typename Rng, typename It, std::enable_if_t< !has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
	constexpr decltype(auto) make_iterator(Rng&&, It&& it) MAYTHROW {
		return std::forward<It>(it);
	}

	template<typename Rng, typename Index, std::enable_if_t< has_index< std::remove_reference_t<Rng> >::value >* =nullptr >
	constexpr decltype(auto) make_iterator(Rng&& rng, Index&& idx) MAYTHROW {
		return std::forward<Rng>(rng).make_iterator(std::forward<Index>(idx));
	}
}
