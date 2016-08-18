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
#include "reverse_adaptor.h"

#include <boost/iterator/transform_iterator.hpp>
#include <boost/algorithm/string/compare.hpp>
#include <boost/range/algorithm/mismatch.hpp>

#include <boost/multi_index/ordered_index.hpp>

#include <functional>

namespace tc {

	namespace iterator {

		template<typename It> It middle_point(It const&, It const&) noexcept;

		namespace adl {
			// default forward iterator implementation
			template<typename It>
			It 
			middle_point( It const& itBegin, It const& itEnd ) noexcept {
				// SEnumerateGapConstraints::SEnumerateGapConstraints calls intersect on transforms of counting ranges of tree iterators
				// intersect calls tc::upper_bound
				// TODO efficient implementation for tree iterators
				// static_assert(!std::is_same<It, It>::value, "provide more efficient middle_point or apply linear search");
				return itBegin;
			}

			template< typename T, typename Traversal, typename Difference >
			boost::iterators::counting_iterator<T, Traversal, Difference>
			middle_point(boost::iterators::counting_iterator<T, Traversal, Difference> const& itBegin, boost::iterators::counting_iterator<T, Traversal, Difference> const& itEnd) noexcept {
				return boost::iterators::counting_iterator<T, Traversal, Difference>(
					tc::iterator::middle_point(itBegin.base(), itEnd.base())
				);
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

/*			template<typename UnaryFunction, typename It, typename Reference, typename Value>
			boost::transform_iterator<UnaryFunction, It, Reference, Value>
			middle_point( boost::transform_iterator<UnaryFunction, It, Reference, Value> const& itBegin, boost::transform_iterator<UnaryFunction, It, Reference, Value> const& itEnd ) noexcept {
				return boost::transform_iterator<UnaryFunction, It, Reference, Value>(
					tc::iterator::middle_point(
						itBegin.base(),
						itEnd.base()
					),
					itBegin.functor()
				);
			}*/

			// NodeBase is a base type of the multi_index_container's node (internal data
			// structure used to store elements).
			#if defined(BOOST_MULTI_INDEX_ENABLE_SAFE_MODE)
				template< typename AugmentPolicy, typename NodeBase, typename OrderedIndex >
				boost::multi_index::safe_mode::safe_iterator<
					boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<AugmentPolicy, NodeBase> >,
					OrderedIndex
				>
				middle_point( 
					boost::multi_index::safe_mode::safe_iterator<
						boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<AugmentPolicy, NodeBase> >,
						OrderedIndex
					> itBegin,
					boost::multi_index::safe_mode::safe_iterator<
						boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<AugmentPolicy, NodeBase> >,
						OrderedIndex
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
				using TNodeVector = tc::static_vector<
					node_type*, 
					2* // 2*log(N) is maximum hight of RB tree
						(CHAR_BIT*sizeof(std::size_t)-3) // 2^3==8 roughly minimum size of node
				>;

				// The parent of the root of the tree is a special header node (representing end()) whose
				// parent is again the root node.
				auto PathToRoot=[]( node_type* pnode )->TNodeVector {
					TNodeVector vecpnode;
					node_type* pnodeParent=node_type::from_impl(pnode->parent());

					for(;;) {
						vecpnode.emplace_back(pnode);
						node_type* pnodeGrandparent=node_type::from_impl(pnodeParent->parent());
						if(pnode==pnodeGrandparent) return vecpnode; // abort if pnode is root
						pnode=pnodeParent;
						pnodeParent=pnodeGrandparent;
					}
				};
				TNodeVector vecpnodeBegin=PathToRoot(itBegin.get_node());
				TNodeVector vecpnodeEnd=PathToRoot(boost::prior(itEnd).get_node());
				_ASSERTEQUAL( tc_back(vecpnodeBegin), tc_back(vecpnodeEnd) ); // both paths terminate at the root
				node_type* pnodeCommon=*boost::prior( boost::mismatch(
					tc::reverse(vecpnodeBegin),
					tc::reverse(vecpnodeEnd)
				).first ); // or second, same thing
				#if defined(BOOST_MULTI_INDEX_ENABLE_SAFE_MODE)
					return boost::multi_index::safe_mode::safe_iterator<boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<AugmentPolicy, NodeBase> >, OrderedIndex>(pnodeCommon, tc::make_mutable_ptr(itBegin.owner()));
				#else
					return boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<AugmentPolicy, NodeBase> >(pnodeCommon);
				#endif
			}

			template<typename It>
			It middle_point_dispatch( It const& itBegin, It const& itEnd, boost::iterators::forward_traversal_tag ) noexcept {
				return middle_point(itBegin,itEnd);
			}

			// default random-access iterator implementation
			template<typename It>
			It middle_point_dispatch( It const& itBegin, It const& itEnd, boost::iterators::random_access_traversal_tag ) noexcept {
				return itBegin+(itEnd-itBegin)/2;
			}

		}

		template<typename It>
		It middle_point( It const& itBegin, It const& itEnd ) noexcept {
			return adl::middle_point_dispatch( itBegin, itEnd, typename boost::iterator_traversal<It>::type() );
		}

		template<typename It, typename UnaryPred>
		It internal_partition_point( It itBegin, It itEnd, UnaryPred pred ) noexcept {
		#ifdef _DEBUG /* is pred a partitioning? All true must be before all false. */
			It itPartitionPoint = itBegin;
			while( itPartitionPoint!=itEnd && pred(itPartitionPoint) ) ++itPartitionPoint;
			for( It itRest=itPartitionPoint; itRest!=itEnd; ++itRest ) {
				_ASSERT( !tc::bool_cast(pred(itRest)) );
			}
		#endif
			while( itBegin!=itEnd ) {
				It itMid = iterator::middle_point( itBegin, itEnd ); // may return any itMid in [itBegin,itEnd[
				_ASSERT(itMid != itEnd);
				if( pred(itMid) ) {
					itBegin=tc_move(itMid);
					++itBegin;
				} else {
					itEnd=tc_move(itMid);
				}
			}
		#ifdef _DEBUG /* we have already found the partition point; compare result */
			_ASSERT( itBegin == itPartitionPoint );
		#endif
			return itBegin;
		}

		template<typename It, typename UnaryPred>
		It partition_point( It itBegin, It itEnd, UnaryPred pred ) noexcept {
			return internal_partition_point( tc_move(itBegin), tc_move(itEnd), [&pred](It it){
				return pred(*it);
			} );
		}

		template<typename It, typename UnaryPred>
		It partition_pair( It itBegin, It itEnd, UnaryPred pred ) noexcept {
			_ASSERT( itBegin!=itEnd );
			--itEnd;
			return internal_partition_point( tc_move(itBegin), tc_move(itEnd), [&pred](It it){
				return pred(*it,*boost::next(it));
			} );
		}

		////////////////////////////////////////////////////////////////////////
		// Iterator functions forwarding to partition_point

		template< typename It, typename Value, typename UnaryPredicate >
		It lower_bound(It itBegin,It itEnd,Value const& val,UnaryPredicate&& pred) noexcept {
			return iterator::partition_point(itBegin,itEnd,std::bind(std::forward<UnaryPredicate>(pred),std::placeholders::_1,std::cref(val)));
		}

		template< typename It, typename Value, typename UnaryPredicate >
		It upper_bound(It itBegin,It itEnd,Value const& val,UnaryPredicate&& pred) noexcept {
			return iterator::partition_point(itBegin,itEnd,tc::not_fn(std::bind(std::forward<UnaryPredicate>(pred),std::cref(val),std::placeholders::_1)));
		}

		template< typename It, typename Value, typename SortPredicate >
		std::pair<It,It> equal_range(It itBegin,It itEnd,Value const& val,SortPredicate pred) noexcept {
			// Construct std::pair<It,It> initialized so that transform_iterator functor
			// does have to be neither default-constructible nor assignable. This is non-standard conformant,
			// but may be practical.
			It itEqualBegin=iterator::lower_bound(itBegin,itEnd,val,pred);
			return std::pair<It,It>( itEqualBegin, iterator::upper_bound(itEqualBegin,itEnd,val,pred) );
		}

		template< typename It, typename Value >
		It lower_bound(It itBegin,It itEnd,Value const& val) noexcept {
			return iterator::lower_bound( itBegin, itEnd, val, std::less<>() );
		}

		template< typename It, typename Value >
		It upper_bound(It itBegin,It itEnd,Value const& val) noexcept {
			return iterator::upper_bound( itBegin, itEnd, val, std::less<>() );
		}

		template< typename It, typename Value >
		std::pair<It,It> equal_range(It itBegin,It itEnd,Value const& val) noexcept {
			return iterator::equal_range( itBegin, itEnd, val, std::less<>() );
		}
	}
	using namespace iterator;
	using iterator::middle_point;
}

