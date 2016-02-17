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

#include "range_defines.h"
#include "range_fwd.h"
#include "range_adaptor.h"
#include "meta.h"
#include "types.h"
#include "size.h"
#include "as_lvalue.h"

#include <boost/variant.hpp>
#include <boost/serialization/strong_typedef.hpp>


namespace tc {

	namespace concat_adaptor_impl {

		template<
			typename Rng0,
			typename Rng1,
			bool HasIterator = is_range_with_iterators< Rng0 >::value && is_range_with_iterators< Rng1 >::value
		>
		struct concat_adaptor;

		template<
			typename Rng0,
			typename Rng1
		>
		struct concat_adaptor<Rng0, Rng1, false> {
			std::tuple<
				reference_or_value< index_range_t<Rng0> >,
				reference_or_value< index_range_t<Rng1> >
			> m_baserng;

			concat_adaptor(concat_adaptor&& rng) noexcept
				: m_baserng(tc_move(rng.m_baserng))
			{}

			template<typename Rhs0, typename Rhs1>
			concat_adaptor(Rhs0&& rhs0, Rhs1&& rhs1) noexcept
				: m_baserng(
					reference_or_value< index_range_t<Rng0> >(std::forward<Rhs0>(rhs0), aggregate_tag()),
					reference_or_value< index_range_t<Rng1> >(std::forward<Rhs1>(rhs1), aggregate_tag())
				)
			{}

			template< typename Func >
			auto operator()(Func func) MAYTHROW ->
				std::enable_if_t<
					std::is_same<
						decltype((*std::get<0>(m_baserng))(std::ref(func))),
						break_or_continue
					>::value &&
					std::is_same<
						decltype((*std::get<1>(m_baserng))(std::ref(func))),
						break_or_continue
					>::value,
					break_or_continue
				>
			{
				RETURN_IF_BREAK((*std::get<0>(m_baserng))(std::ref(func)));
				RETURN_IF_BREAK((*std::get<1>(m_baserng))(std::ref(func)));
				return tc::continue_;
			}

			template< typename Func >
			auto operator()(Func func) MAYTHROW ->
				typename std::enable_if<
					!std::is_same<
						decltype((*std::get<0>(m_baserng))(std::ref(func))),
						break_or_continue
					>::value ||
					!std::is_same<
						decltype((*std::get<1>(m_baserng))(std::ref(func))),
						break_or_continue
					>::value
				>::type
			{
				(*std::get<0>(m_baserng))(std::ref(func));
				(*std::get<1>(m_baserng))(std::ref(func));
			}

			template< typename Func >
			auto operator()(Func func) const MAYTHROW ->
				std::enable_if_t<
					std::is_same<
						decltype((*std::get<0>(m_baserng))(std::ref(func))),
						break_or_continue
					>::value &&
					std::is_same<
						decltype((*std::get<1>(m_baserng))(std::ref(func))),
						break_or_continue
					>::value,
					break_or_continue
				>
			{
				RETURN_IF_BREAK((*std::get<0>(m_baserng))(std::ref(func)));
				RETURN_IF_BREAK((*std::get<1>(m_baserng))(std::ref(func)));
				return tc::continue_;
			}

			template< typename Func >
			auto operator()(Func func) const MAYTHROW ->
				typename std::enable_if<
					!std::is_same<
						decltype((*std::get<0>(m_baserng))(std::ref(func))),
						break_or_continue
					>::value ||
					!std::is_same<
						decltype((*std::get<1>(m_baserng))(std::ref(func))),
						break_or_continue
					>::value
				>::type
			{
				(*std::get<0>(m_baserng))(std::ref(func));
				(*std::get<1>(m_baserng))(std::ref(func));
			}
		};

		template<
			int N,
			typename T0,
			typename... T
		>
		struct type_by_index final {
			using type = typename type_by_index<N-1,T...>::type;
		};

		template<
			typename T0,
			typename... T
		>
		struct type_by_index<0, T0, T...> final {
			using type = T0;
		};

		template<
			typename BaseIndex0,
			typename BaseIndex1
		>
		struct concat_index {
			BOOST_STRONG_TYPEDEF(BaseIndex0, Index0)
			BOOST_STRONG_TYPEDEF(BaseIndex1, Index1)

			using type = boost::variant<Index0, Index1>;

			template<int N>
			using index_t = typename type_by_index<N, Index0, Index1>::type;
		};

		template<
			typename Rng0,
			typename Rng1
		>
		struct concat_adaptor<Rng0, Rng1, true> :
			concat_adaptor < Rng0, Rng1, false >,
			range_iterator_from_index<
				concat_adaptor<Rng0, Rng1, true>,
				typename concat_index<
					typename std::remove_reference_t<
						index_range_t<Rng0>
					>::index,
					typename std::remove_reference_t<
						index_range_t<Rng1>
					>::index
				>::type,
				typename boost::range_detail::demote_iterator_traversal_tag<
					traversal_t<Rng0>,
					traversal_t<Rng1>
				>::type
			>
		{
		private:
			using this_type = concat_adaptor;
		public:
			using concat_index_type = concat_index<
				typename std::remove_reference_t<
					index_range_t<Rng0>
				>::index,
				typename std::remove_reference_t<
					index_range_t<Rng1>
				>::index
			>;

			template <int N>
			using index_t = typename concat_index_type::template index_t<N>;

			using Traversal = typename boost::range_detail::demote_iterator_traversal_tag<
				traversal_t<Rng0>,
				traversal_t<Rng1>
			>::type;

			using base_ = range_iterator_from_index<
				concat_adaptor<Rng0, Rng1, true>,
				typename concat_index_type::type,
				Traversal
			>;

			using typename base_::index;
			using concat_adaptor < Rng0, Rng1, false >::m_baserng;

			concat_adaptor(concat_adaptor&& rng) noexcept
				: concat_adaptor<Rng0, Rng1, false>(tc_move(rng))
				{}

			template<typename Rhs0, typename Rhs1>
			concat_adaptor(Rhs0&& rhs0, Rhs1&& rhs1) noexcept
				: concat_adaptor<Rng0,Rng1,false>(std::forward<Rhs0>(rhs0), std::forward<Rhs1>(rhs1))
			{}

		private:
			index correct_index(index idx) const noexcept {
				switch_no_default(idx.which()) {
				case 0: if (std::get<0>(m_baserng)->at_end_index(boost::get<index_t<0>>(idx))) idx = index_t<1>(std::get<1>(m_baserng)->begin_index());
				case 1:;
				}
				return idx;
			}

		public:
			STATIC_FINAL(begin_index)() const noexcept -> index {
				return correct_index( index_t<0>(std::get<0>(m_baserng)->begin_index()) );
			}

			STATIC_FINAL(end_index)() const noexcept -> index {
				return index_t<1>( std::get<1>(m_baserng)->end_index() );
			}

			STATIC_FINAL(at_end_index)(index const& idx) const noexcept -> bool {
				return 1 == idx.which() && std::get<1>(m_baserng)->at_end_index(boost::get<index_t<1>>(idx));
			}

			STATIC_FINAL(increment_index)(index& idx) const noexcept -> void {
				switch_no_default(idx.which()) {
				case 0:
					std::get<0>(m_baserng)->increment_index(boost::get<index_t<0>>(idx));
					idx = correct_index(boost::get<index_t<0>>(idx));
					break;
				case 1:
					{
						auto& idx1 = boost::get<index_t<1>>(idx);
						_ASSERT(!std::get<1>(m_baserng)->at_end_index(idx1));
						std::get<1>(m_baserng)->increment_index(idx1);
					}
				}
			}

			STATIC_FINAL(decrement_index)(index& idx) const noexcept -> void{
				switch_no_default(idx.which()) {
				case 1:
					{
						auto& idx1 = boost::get<index_t<1>>(idx);
						if (!std::get<1>(m_baserng)->equal_index(std::get<1>(m_baserng)->begin_index(), idx1)) {
							std::get<1>(m_baserng)->decrement_index(idx1);
							return;
						}
					}
					idx = index_t<0>(std::get<0>(m_baserng)->end_index());
				case 0:
					{
						auto& idx0 = boost::get<index_t<0>>(idx);
						_ASSERT(!std::get<0>(m_baserng)->equal_index(std::get<0>(m_baserng)->begin_index(), idx0));
						std::get<0>(m_baserng)->decrement_index(idx0);
					}
				}
			}

			template<int N>
			auto dereference_index_fwd(index const& idx) const noexcept return_decltype(
				std::get<N>(m_baserng)->dereference_index(boost::get<index_t<N>>(idx))
			)

			STATIC_FINAL(dereference_index)(index const& idx) const noexcept ->
			typename reference_type<
				typename range_traits<std::remove_reference_t<Rng0> const>::reference,
				typename range_traits<std::remove_reference_t<Rng1> const>::reference
			>::type {
				switch_no_default(idx.which()) {
					case 0: return dereference_index_fwd<0>(idx);
					case 1: return dereference_index_fwd<1>(idx);
				}
			}

			template<int N>
			auto dereference_index_fwd(index const& idx) noexcept return_decltype(
				std::get<N>(m_baserng)->dereference_index(boost::get<index_t<N>>(idx))
			)

			STATIC_FINAL(dereference_index)(index const& idx) noexcept ->
			typename reference_type<
				typename range_traits<Rng0>::reference,
				typename range_traits<Rng1>::reference
			>::type {
				switch_no_default(idx.which()) {
					case 0: return dereference_index_fwd<0>(idx);
					case 1: return dereference_index_fwd<1>(idx);
				}
			}

			template<int N>
			bool equal_index_fwd(index const& idxLhs, index const& idxRhs) const noexcept {
				return std::get<N>(m_baserng)->equal_index(boost::get<index_t<N>>(idxLhs), boost::get<index_t<N>>(idxRhs));
			}

			STATIC_FINAL(equal_index)(index const& idxLhs, index const& idxRhs) const noexcept -> bool {
				if ( idxLhs.which() != idxRhs.which() ) return false;
				switch_no_default(idxLhs.which()) {
					case 0: return equal_index_fwd<0>(idxLhs, idxRhs);
					case 1: return equal_index_fwd<1>(idxLhs, idxRhs);
				}
			}

			using difference_type = std::common_type_t<
				range_difference_type<Rng0,traversal_t<Rng0>>,
				range_difference_type<Rng1,traversal_t<Rng1>>
			>;

			STATIC_FINAL(advance_index)(index& idx, difference_type d) const noexcept -> void {
				if (d < 0) {
					switch_no_default(idx.which()) {
					case 1:
						{
							auto dToBegin = std::get<1>(m_baserng)->distance_to_index(
								boost::get<index_t<1>>(idx),
								std::get<1>(m_baserng)->begin_index()
							);

							if (!(d<dToBegin)) {
								std::get<1>(m_baserng)->advance_index(boost::get<index_t<1>>(idx), d);
								return;
							}
							d -= dToBegin;
							idx = index_t<0>(std::get<0>(m_baserng)->end_index());
						}
					case 0:
						{
#ifdef _CHECKS
							auto dToBegin = std::get<0>(m_baserng)->distance_to_index(
								boost::get<index_t<0>>(idx),
								std::get<0>(m_baserng)->begin_index()
							);

							_ASSERT(!(d < dToBegin));
#endif
							std::get<0>(m_baserng)->advance_index(boost::get<index_t<0>>(idx), d);
						}
					}

				} else {
					switch_no_default(idx.which()) {
					case 0:
						{
							auto dToEnd = std::get<0>(m_baserng)->distance_to_index(
								boost::get<index_t<0>>(idx),
								std::get<0>(m_baserng)->end_index()
							);

							if (d < dToEnd) {
								std::get<0>(m_baserng)->advance_index(boost::get<index_t<0>>(idx), d);
								return;
							}
							d -= dToEnd;
							idx = index_t<1>(std::get<1>(m_baserng)->begin_index());
						}
					case 1:
						{
#ifdef _CHECKS
							auto dToEnd = std::get<1>(m_baserng)->distance_to_index(
								boost::get<index_t<1>>(idx),
								std::get<1>(m_baserng)->end_index()
							);
							_ASSERT(!(dToEnd < d));
#endif
							std::get<1>(m_baserng)->advance_index(boost::get<index_t<1>>(idx), d);
						}
					}
				}
			}

			STATIC_FINAL(distance_to_index)(index const& idxLhs, index const& idxRhs) const noexcept -> difference_type {
				if (idxLhs.which() == idxRhs.which()) {
					switch_no_default(idxLhs.which()) {
						case 0: return std::get<0>(m_baserng)->distance_to_index(boost::get<index_t<0>>(idxLhs), boost::get<index_t<0>>(idxRhs));
						case 1: return std::get<1>(m_baserng)->distance_to_index(boost::get<index_t<1>>(idxLhs), boost::get<index_t<1>>(idxRhs));
					}
				} else {
					auto positive_distance = [&](index const& lhs, index const& rhs) {
						return
							std::get<0>(m_baserng)->distance_to_index(
								boost::get<index_t<0>>(lhs),
								std::get<0>(m_baserng)->end_index()
							) +
							std::get<1>(m_baserng)->distance_to_index(
								std::get<1>(m_baserng)->begin_index(),
								boost::get<index_t<1>>(rhs)
							);
					};
					if (idxLhs.which() < idxRhs.which()) {
						return positive_distance(idxLhs, idxRhs);
					} else {
						return -positive_distance(idxRhs, idxLhs);
					}
				}
			}

			template<typename R0 = Rng0, typename R1 = Rng1, std::enable_if_t<
				tc::size_impl::has_size<R0>::value &&
				tc::size_impl::has_size<R1>::value
			>* = nullptr>
			auto size() const noexcept return_decltype(
				tc::size_impl::size(boost::implicit_cast<std::remove_reference_t<R0> const&>(*std::get<0>(THIS_IN_DECLTYPE m_baserng))) +
				tc::size_impl::size(boost::implicit_cast<std::remove_reference_t<R1> const&>(*std::get<1>(THIS_IN_DECLTYPE m_baserng)))
			)
		};
	}

	using concat_adaptor_impl::concat_adaptor;

	template<
		typename Rng0,
		typename Rng1
	>
	struct const_range < concat_adaptor<Rng0, Rng1> > {
		using type = concat_adaptor <
			typename const_range<Rng0>::type,
			typename const_range<Rng1>::type
		>;
	};

	template<typename Rng0, typename Rng1>
	auto concat(Rng0&& rng0, Rng1&& rng1) noexcept return_ctor(
		concat_adaptor< range_by_value_t<Rng0> BOOST_PP_COMMA() range_by_value_t<Rng1>>,
		(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1))
	)
}
