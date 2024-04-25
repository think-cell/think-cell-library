
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../container/insert.h"
#include "../static_vector.h"
#include "../interval_types.h"
#include "../range/reverse_adaptor.h"

#include <boost/iterator/transform_iterator.hpp>
#include <boost/next_prior.hpp>
#include <boost/range/algorithm/mismatch.hpp>

#include <boost/multi_index/ordered_index.hpp>

#include <functional>

namespace tc {

	namespace iterator {

		template<typename It> constexpr It middle_point(It const&, It const&) noexcept;

		namespace adl {
			// default forward iterator implementation
			template<typename It> requires true // to make it more constrained than tc::iterator::middle_point
			It 
			middle_point( It const& itBegin, It const& itEnd ) noexcept {
				// SEnumerateGapConstraints::SEnumerateGapConstraints calls intersect on transforms of counting ranges of tree iterators
				// intersect calls tc::upper_bound
				// TODO efficient implementation for tree iterators
				// static_assert(!std::is_same<It, It>::value, "provide more efficient middle_point or apply linear search");
				return itBegin;
			}

			template< typename It, typename UnaryPred >
			std::reverse_iterator<It>
			middle_point(std::reverse_iterator<It> const& itBegin, std::reverse_iterator<It> const& itEnd ) noexcept {
				static_assert(!std::is_same<It, It>::value, "This code has never been tested");
				return std::reverse_iterator<It>(
					tc::iterator::middle_point(
						itEnd.base(),
						itBegin.base()
					)
				);
			}

			// NodeBase is a base type of the multi_index_container's node (internal data
			// structure used to store elements).
			#if defined(BOOST_MULTI_INDEX_ENABLE_SAFE_MODE)
				template< typename AugmentPolicy, typename NodeBase >
				boost::multi_index::safe_mode::safe_iterator<
					boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<AugmentPolicy, NodeBase> >
				>
				middle_point( 
					boost::multi_index::safe_mode::safe_iterator<
						boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<AugmentPolicy, NodeBase> >
					> itBegin,
					boost::multi_index::safe_mode::safe_iterator<
						boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<AugmentPolicy, NodeBase> >
					> itEnd
				)
			#else
				template< typename AugmentPolicy, typename NodeBase >
				boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<AugmentPolicy, NodeBase> >
				middle_point( 
					boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<AugmentPolicy, NodeBase> > itBegin,
					boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<AugmentPolicy, NodeBase> > itEnd
				) noexcept
			#endif
			{
				using node_type = boost::multi_index::detail::ordered_index_node<AugmentPolicy, NodeBase>;
				using TNodeVector = typename tc::static_vector<
					node_type*, 
					2* // 2*log(N) is maximum hight of RB tree
						(CHAR_BIT*sizeof(std::size_t)-3) // 2^3==8 roughly minimum size of node
				>;

				// The parent of the root of the tree is a special header node (representing end()) whose
				// parent is again the root node.
				tc_static_auto_constexpr_lambda(PathToRoot) = []( node_type* pnode ) noexcept ->TNodeVector {
					TNodeVector vecpnode;
					node_type* pnodeParent=node_type::from_impl(pnode->parent());

					for(;;) {
						tc::cont_emplace_back(vecpnode, pnode);
						node_type* pnodeGrandparent=node_type::from_impl(pnodeParent->parent());
						if(pnode==pnodeGrandparent) return vecpnode; // abort if pnode is root
						pnode=pnodeParent;
						pnodeParent=pnodeGrandparent;
					}
				};
				TNodeVector vecpnodeBegin=PathToRoot(itBegin.get_node());
				TNodeVector vecpnodeEnd=PathToRoot(tc_modified(itEnd, --_).get_node());
				_ASSERTEQUAL( tc::back(vecpnodeBegin), tc::back(vecpnodeEnd) ); // both paths terminate at the root
				node_type* pnodeCommon=*tc_modified( boost::mismatch(
					tc::reverse(vecpnodeBegin),
					tc::reverse(vecpnodeEnd)
				).first, --_ ); // or second, same thing
				#if defined(BOOST_MULTI_INDEX_ENABLE_SAFE_MODE)
					return boost::multi_index::safe_mode::safe_iterator<boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<AugmentPolicy, NodeBase> >>(pnodeCommon, tc::as_mutable_ptr(itBegin.owner()));
				#else
					return boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<AugmentPolicy, NodeBase> >(pnodeCommon);
				#endif
			}

			template<typename It>
			constexpr It middle_point_dispatch( It const& itBegin, It const& itEnd, boost::iterators::forward_traversal_tag ) noexcept {
				return middle_point(itBegin,itEnd);
			}

			// default random-access iterator implementation
			template<typename It>
			constexpr It middle_point_dispatch( It const& itBegin, It const& itEnd, boost::iterators::random_access_traversal_tag ) noexcept {
				return itBegin+(itEnd-itBegin)/2;
			}

		}

		template<typename It>
		[[nodiscard]] constexpr It middle_point( It const& itBegin, It const& itEnd ) noexcept {
			return adl::middle_point_dispatch( itBegin, itEnd, typename boost::iterator_traversal<It>::type() );
		}

		template<typename It, typename UnaryPred>
		[[nodiscard]] constexpr It internal_partition_point( It itBegin, It itEnd, UnaryPred pred ) noexcept {
		#ifdef _DEBUG /* is pred a partitioning? All true must be before all false. */
			It itPartitionPoint = itBegin;
			while( itPartitionPoint!=itEnd && pred(tc::as_const(itPartitionPoint)) ) ++itPartitionPoint;
			for( It itRest=itPartitionPoint; itRest!=itEnd; ++itRest ) {
				_ASSERT( !tc::explicit_cast<bool>(pred(tc::as_const(itRest))) );
			}
		#endif
			while( itBegin!=itEnd ) {
				It itMid = iterator::middle_point( itBegin, itEnd ); // may return any itMid in [itBegin,itEnd[
				_ASSERTDEBUG(itMid != itEnd);
				if( pred(tc::as_const(itMid)) ) {
					itBegin=tc_move(itMid);
					++itBegin;
				} else {
					itEnd=tc_move(itMid);
				}
			}
			// we have already found the partition point; compare result
			_ASSERTDEBUGEQUAL(itBegin, itPartitionPoint);
			return itBegin;
		}

		template<typename It, typename UnaryPred>
		[[nodiscard]] constexpr It partition_point( It itBegin, It itEnd, UnaryPred pred ) noexcept {
			return internal_partition_point( tc_move(itBegin), tc_move(itEnd), [&pred](It it) noexcept {
				return pred(tc::as_const(*it));
			} );
		}

		template<typename It, typename UnaryPred>
		[[nodiscard]] It partition_pair( It itBegin, It itEnd, UnaryPred pred ) noexcept {
			_ASSERT( itBegin!=itEnd );
			--itEnd;
			return internal_partition_point( tc_move(itBegin), tc_move(itEnd), [&pred](It it) noexcept {
				return pred(tc::as_const(*it),tc::as_const(*tc_modified(it, ++_)));
			} );
		}

		////////////////////////////////////////////////////////////////////////
		// Iterator functions forwarding to partition_point

		template <typename It, typename Value, typename UnaryPredicate = tc::fn_less>
		[[nodiscard]] It lower_bound(It itBegin,It itEnd,Value const& val,UnaryPredicate pred = {}) noexcept {
			return iterator::partition_point(
				itBegin,
				itEnd,
				[&](auto const& _) noexcept { return pred(_, val); }
			);
		}

		template <typename It, typename Value, typename UnaryPredicate = tc::fn_less>
		[[nodiscard]] It upper_bound(It itBegin,It itEnd,Value const& val,UnaryPredicate&& pred = {}) noexcept {
			return iterator::partition_point(itBegin,itEnd,[&](auto const& _) noexcept { return !pred(val, _); });
		}

		template <typename It, typename Value, typename SortPredicate = tc::fn_less>
		[[nodiscard]] std::pair<It,It> equal_range(It itBegin,It itEnd,Value const& val,SortPredicate pred = {}) noexcept {
			// Construct std::pair<It,It> initialized so that transform_iterator functor
			// does have to be neither default-constructible nor assignable. This is non-standard conformant,
			// but may be practical.
			It itEqualBegin=iterator::lower_bound(itBegin,itEnd,val,pred);
			return std::pair<It,It>( itEqualBegin, iterator::upper_bound(itEqualBegin,itEnd,val,pred) );
		}
	}
	using namespace iterator;
	using iterator::middle_point;
}

