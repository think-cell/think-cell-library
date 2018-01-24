//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
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

#include "range_defines.h"
#include "break_or_continue.h"
#include "meta.h"
#include "size.h"

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

	template< typename It, typename ConstIt > struct iterator_base;

	namespace index_from_iterator_impl {
		template<typename It>
		struct index_from_iterator final {
			template< typename ItIb, typename ConstIt > friend struct tc::iterator_base;
			template< typename fIt > friend struct index_from_iterator; // enable a const compatible index to be initialized.

		private:
			It m_it;
		public:

			index_from_iterator() noexcept {}

			template< typename Rhs >
			index_from_iterator( aggregate_tag, Rhs&& rhs) noexcept
			:	m_it( std::forward<Rhs>(rhs) )
			{}
			template< typename Rhs >
			index_from_iterator( Rhs&& rhs, 
								std::enable_if_t< 
									std::is_constructible< It, decltype(Rhs::m_it) >::value,
									unused_arg 
								> = unused_arg() ) noexcept
			:	m_it(std::forward<Rhs>(rhs).m_it)
			{}

			// explicitly define the copy constructor to do what the template above does, as it would if the implicit copy consturctor wouldn't interfere
			index_from_iterator( index_from_iterator const& rhs ) noexcept
			:	m_it(rhs.m_it)
			{}
		};
	}
	using index_from_iterator_impl::index_from_iterator;

	template< typename It >
	index_from_iterator<tc::decay_t<It>> iterator2index( It const& it ) noexcept {
		return index_from_iterator<tc::decay_t<It>>(aggregate_tag(), it);
	}

	template< typename It, typename ConstIt=It >
	struct iterator_base {
		using iterator = It;
		using const_iterator = ConstIt;
		using index = index_from_iterator<It>;

/*		template< typename OtherIt, typename OtherConstIt >
		explicit iterator_base( iterator_base<OtherIt,OtherConstIt> const&, std::enable_if_t<
			std::is_convertible<OtherIt,It>::value && std::is_convertible<OtherConstIt,ConstIt>::value
		, unused_arg > =unused_arg() ) {};
		template< typename OtherIt, typename OtherConstIt, std::enable_if_t<
			std::is_convertible<OtherIt,It>::value && std::is_convertible<OtherConstIt,ConstIt>::value
		>* = nullptr> iterator_base& operator=( iterator_base<OtherIt,OtherConstIt> const& ) & noexcept { return *this; }
*/
		typename std::iterator_traits<iterator>::reference dereference_index(index const& idx) & noexcept {
			return *idx.m_it;
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
		> dereference_index(index const& idx) const& noexcept {
			return *idx.m_it;
		}

		bool equal_index(index const& idxLhs, index const& idxRhs) const& noexcept {
			return idxLhs.m_it==idxRhs.m_it;
		}

		void increment_index(index& idx) const& noexcept {
			++idx.m_it;
		}

		void decrement_index(index& idx) const& noexcept {
			--idx.m_it;
		}

		void advance_index(index& idx, typename std::iterator_traits<iterator>::difference_type d) const& noexcept {
			idx.m_it+=d;
		}

		typename std::iterator_traits<iterator>::difference_type distance_to_index(index const& idxLhs, index const& idxRhs) const& noexcept {
			return idxRhs.m_it-idxLhs.m_it;
		}

		void middle_point( index & idxBegin, index const& idxEnd ) const& noexcept {
			idxBegin.m_it=tc::iterator::middle_point( idxBegin.m_it, idxEnd.m_it );
		}

		iterator make_iterator( index idx ) & noexcept {
			return idx.m_it;
		}

		const_iterator make_iterator( index idx ) const& noexcept {
			return idx.m_it;
		}
	};

	BOOST_MPL_HAS_XXX_TRAIT_DEF(index)

	////////////////////////////////////
	// adding index to legacy ranges

	template< typename T >
	struct is_boost_iterator_range final : std::false_type {};

	template< typename T >
	struct is_boost_iterator_range< boost::iterator_range<T> > final : std::true_type {};

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
			STATIC_VIRTUAL(begin_index)
			STATIC_VIRTUAL(at_end_index)

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

	namespace void_generator_type_check_impl {
		template<typename Func>
		struct ensure_non_breaking_functor final {
			explicit ensure_non_breaking_functor(Func&& f) noexcept
				: m_func(f)
			{}

			template<typename... Args>
			decltype(auto) operator()(Args&&... args) & MAYTHROW {
				static_assert(
					!std::is_same<
						decltype(m_func(std::forward<Args>(args)...)),
						break_or_continue
					>::value &&
					!std::is_same<
						decltype(m_func(std::forward<Args>(args)...)),
						INTEGRAL_CONSTANT(tc::break_)
					>::value,
					"Functor may return break_, but range does not support it."
				);
				return m_func(std::forward<Args>(args)...); // MAYTHROW
			}

		private:
			tc::decay_t<Func&&> m_func;
		};
		template<typename Func>
		struct ensure_always_breaking_functor final {
			explicit ensure_always_breaking_functor(Func&& f) noexcept
				: m_func(f)
			{}

			template<typename... Args>
			decltype(auto) operator()(Args&&... args) & MAYTHROW {
				static_assert(
					std::is_same<
						decltype(m_func(std::forward<Args>(args)...)),
						INTEGRAL_CONSTANT(tc::break_)
					>::value
				);
				return m_func(std::forward<Args>(args)...); // MAYTHROW
			}

		private:
			tc::decay_t<Func&&> m_func;
		};
	}

	template<typename Rng>
	struct constexpr_size_proxy : decltype(constexpr_size(std::declval<Rng>())) {};

	template< typename Rng >
	struct index_range final {
	private:
		struct add_index_interface : range_generator_from_index< add_index_interface, iterator_base<
			typename boost::range_iterator< std::remove_reference_t< typename reference_or_value< Rng >::reference > >::type,
			typename boost::range_iterator< std::remove_reference_t< typename reference_or_value< Rng >::const_reference > >::type
		> > {
		private:
			using this_type = add_index_interface;
			
		public:
			// Index interface
			using index = typename this_type::index;

			add_index_interface() noexcept {}

			template< typename Rhs, std::enable_if_t< !tc::is_base_of_decayed< add_index_interface, Rhs >::value>* =nullptr >
			add_index_interface( Rhs&& rhs ) noexcept
			:	m_rng( aggregate_tag(), std::forward<Rhs>(rhs) )
			{}

			STATIC_FINAL(begin_index)() const& noexcept -> index {
				return index( aggregate_tag(), boost::begin(m_rng.best_access()) );
			}
	
			auto end_index() const& noexcept -> index {
				return index( aggregate_tag(), boost::end(m_rng.best_access()) );
			}

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				return this->equal_index( idx, this->end_index() );
			}

			operator typename reference_or_value< Rng >::reference () & noexcept {
				return *m_rng;
			}

			operator typename reference_or_value< Rng >::const_reference () const& noexcept {
				return *m_rng;
			}

			template< typename Rng2 = Rng, std::enable_if_t<tc::size_impl::has_size<Rng2>::value>* = nullptr >
			auto size() const& noexcept {
				return tc::size_impl::size(*m_rng);
			}

			// Range interface
			using iterator = decltype(boost::begin(*std::declval<reference_or_value< Rng >&>()));
			using const_iterator = decltype(boost::begin(*std::declval<reference_or_value< Rng > const&>()));

			auto begin() const& noexcept { return boost::begin(*m_rng); }
			auto end() const& noexcept { return boost::end(*m_rng); }
			auto begin() & noexcept { return boost::begin(*m_rng); }
			auto end() & noexcept { return boost::end(*m_rng); }

		private:
			static_assert(
				!std::is_rvalue_reference<Rng>::value
				, "Rng is rvalue reference"
			);
			reference_or_value< Rng > m_rng;

			friend constexpr_size_proxy<decltype(*m_rng)> constexpr_size(add_index_interface const&);
		};

	public:
		using type=std::conditional_t< is_range_with_iterators< Rng >::value && !has_index< std::remove_reference_t<Rng> >::value,
			add_index_interface,
			Rng
		>;
	};

	template<typename T>
	using index_range_t = typename index_range<T>::type;
}
