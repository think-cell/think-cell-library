#pragma once

#include "range_defines.h"
#include "range_fwd.h"
#include "range_adaptor.h"
#include "meta.h"
#include "types.h"
#include "Library/Utilities/Range/size.h"

#include <boost/variant.hpp>
#include <boost/serialization/strong_typedef.hpp>
#include "../../ErrorReporting/make_lvalue.h"

namespace RANGE_PROPOSAL_NAMESPACE {

	namespace join_adaptor_impl {

		template<
			typename Rng0,
			typename Rng1,
			bool HasIterator = is_range_with_iterators< Rng0 >::value && is_range_with_iterators< Rng1 >::value
		>
		struct join_adaptor;

		template<
			typename Rng0,
			typename Rng1
		>
		struct join_adaptor<Rng0, Rng1, false> {
			std::tuple<
				reference_or_value< typename index_range<Rng0>::type >,
				reference_or_value< typename index_range<Rng1>::type >
			> m_baserng;

			join_adaptor(join_adaptor&& rng)
				: m_baserng(tc_move(rng.m_baserng))
			{}

			template<typename Rhs0, typename Rhs1>
			join_adaptor(Rhs0&& rhs0, Rhs1&& rhs1)
				: m_baserng(
					reference_or_value< typename index_range<Rng0>::type >(std::forward<Rhs0>(rhs0), aggregate_tag()),
					reference_or_value< typename index_range<Rng1>::type >(std::forward<Rhs1>(rhs1), aggregate_tag())
				)
			{}

			template< typename Func >
			auto operator()(Func func) ->
				typename std::enable_if<
					std::is_same<
						decltype((*std::get<0>(m_baserng))(std::ref(func))),
						break_or_continue
					>::value &&
					std::is_same<
						decltype((*std::get<1>(m_baserng))(std::ref(func))),
						break_or_continue
					>::value,
					break_or_continue
				>::type
			{
				RETURN_IF_BREAK((*std::get<0>(m_baserng))(std::ref(func)));
				RETURN_IF_BREAK((*std::get<1>(m_baserng))(std::ref(func)));
				return tc::continue_;
			}

			template< typename Func >
			auto operator()(Func func) ->
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
			auto operator()(Func func) const ->
				typename std::enable_if<
					std::is_same<
						decltype((*std::get<0>(m_baserng))(std::ref(func))),
						break_or_continue
					>::value &&
					std::is_same<
						decltype((*std::get<1>(m_baserng))(std::ref(func))),
						break_or_continue
					>::value,
					break_or_continue
				>::type
			{
				RETURN_IF_BREAK((*std::get<0>(m_baserng))(std::ref(func)));
				RETURN_IF_BREAK((*std::get<1>(m_baserng))(std::ref(func)));
				return tc::continue_;
			}

			template< typename Func >
			auto operator()(Func func) const ->
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
		struct type_by_index {
			using type = typename type_by_index<N-1,T...>::type;
		};

		template<
			typename T0,
			typename... T
		>
		struct type_by_index<0, T0, T...> {
			using type = T0;
		};

		template<
			typename BaseIndex0,
			typename BaseIndex1
		>
		struct join_index {
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
		struct join_adaptor<Rng0, Rng1, true> :
			join_adaptor < Rng0, Rng1, false >,
			range_iterator_from_index<
				join_adaptor<Rng0, Rng1, true>,
				typename join_index<
					typename std::remove_reference<
						typename index_range<Rng0>::type
					>::type::index,
					typename std::remove_reference<
						typename index_range<Rng1>::type
					>::type::index
				>::type,
				typename boost::range_detail::demote_iterator_traversal_tag<
					traversal_t<Rng0>,
					traversal_t<Rng1>
				>::type
			>
		{
		private:
			using this_type = join_adaptor;
		public:
			using join_index_type = join_index<
				typename std::remove_reference<
					typename index_range<Rng0>::type
				>::type::index,
				typename std::remove_reference<
					typename index_range<Rng1>::type
				>::type::index
			>;

			template <int N>
			using index_t = typename join_index_type::template index_t<N>;

			using Traversal = typename boost::range_detail::demote_iterator_traversal_tag<
				traversal_t<Rng0>,
				traversal_t<Rng1>
			>::type;

			using base_ = range_iterator_from_index<
				join_adaptor<Rng0, Rng1, true>,
				typename join_index_type::type,
				Traversal
			>;

			using typename base_::index;
			using join_adaptor < Rng0, Rng1, false >::m_baserng;

			join_adaptor(join_adaptor&& rng)
				: join_adaptor<Rng0, Rng1, false>(tc_move(rng))
				{}

			template<typename Rhs0, typename Rhs1>
			join_adaptor(Rhs0&& rhs0, Rhs1&& rhs1)
				: join_adaptor<Rng0,Rng1,false>(std::forward<Rhs0>(rhs0), std::forward<Rhs1>(rhs1))
			{}

		private:
			index correct_index(index idx) const {
				switch_no_default(idx.which()) {
				case 0: if (std::get<0>(m_baserng)->at_end_index(boost::get<index_t<0>>(idx))) idx = index_t<1>(std::get<1>(m_baserng)->begin_index());
				case 1:;
				}
				return idx;
			}

		public:
			STATIC_FINAL(begin_index)() const -> index {
				return correct_index( index_t<0>(std::get<0>(m_baserng)->begin_index()) );
			}

			STATIC_FINAL(end_index)() const -> index {
				return index_t<1>( std::get<1>(m_baserng)->end_index() );
			}

			STATIC_FINAL(at_end_index)(index const& idx) const -> bool {
				return 1 == idx.which() && std::get<1>(m_baserng)->at_end_index(boost::get<index_t<1>>(idx));
			}

			STATIC_FINAL(increment_index)(index& idx) const -> void {
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

			void decrement_index(index& idx) const {
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
			auto dereference_index_fwd(index const& idx) const return_decltype(
				std::get<N>(m_baserng)->dereference_index(boost::get<index_t<N>>(idx))
			)

			STATIC_FINAL(dereference_index)(index const& idx) const ->
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
			auto dereference_index_fwd(index const& idx) return_decltype(
				std::get<N>(m_baserng)->dereference_index(boost::get<index_t<N>>(idx))
			)

			STATIC_FINAL(dereference_index)(index const& idx) ->
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
			bool equal_index(index const& idxLhs, index const& idxRhs) const {
				return std::get<N>(m_baserng)->equal_index(boost::get<index_t<N>>(idxLhs), boost::get<index_t<N>>(idxRhs));
			}

			bool equal_index(index const& idxLhs, index const& idxRhs) const {
				if ( idxLhs.which() != idxRhs.which() ) return false;
				switch_no_default(idxLhs.which()) {
					case 0: return equal_index<0>(idxLhs, idxRhs);
					case 1: return equal_index<1>(idxLhs, idxRhs);
				}
			}

			using difference_type = std::common_type_t<
				range_difference_type<Rng0,traversal_t<Rng0>>,
				range_difference_type<Rng1,traversal_t<Rng1>>
			>;

			void advance_index(index& idx, difference_type d) const {
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

			template<
				typename traversal = Traversal,
				typename std::enable_if<
					std::is_convertible<
						traversal,
						boost::iterators::random_access_traversal_tag
					>::value
				>::type* = nullptr
			>
			difference_type distance_to_index(index const& idxLhs, index const& idxRhs) const {
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
			auto size() const return_decltype(
				tc::size_impl::size(boost::implicit_cast<std::remove_reference_t<R0> const&>(*std::get<0>(THIS_IN_DECLTYPE m_baserng))) +
				tc::size_impl::size(boost::implicit_cast<std::remove_reference_t<R1> const&>(*std::get<1>(THIS_IN_DECLTYPE m_baserng)))
			)
		};
	}

	using join_adaptor_impl::join_adaptor;

	template<
		typename Rng0,
		typename Rng1
	>
	struct const_range < join_adaptor<Rng0, Rng1> > {
		using type = join_adaptor <
			typename const_range<Rng0>::type,
			typename const_range<Rng1>::type
		>;
	};

	template<typename Rng0, typename Rng1>
	auto join(Rng0&& rng0, Rng1&& rng1) return_ctor(
		join_adaptor< typename range_by_value<Rng0>::type BOOST_PP_COMMA() typename range_by_value<Rng1>::type>,
		(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1))
	)
}
