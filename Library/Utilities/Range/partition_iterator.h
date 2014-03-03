#pragma once

#include "range_defines.h"

#include "Library/Utilities/static_vector.h"

#include <boost/iterator/filter_iterator.hpp>
#include <boost/iterator/indirect_iterator.hpp>
#include <boost/iterator/reverse_iterator.hpp>
#include <boost/iterator/transform_iterator.hpp>

#include <boost/algorithm/string/compare.hpp>
#include <boost/range/algorithm/sort.hpp>
#include <boost/range/algorithm/reverse.hpp>
#include <boost/range/adaptor/reversed.hpp>
#include <boost/range/algorithm/mismatch.hpp>

#include <boost/multi_index/ordered_index.hpp>

#include <boost/shared_container_iterator.hpp>

namespace RANGE_PROPOSAL_NAMESPACE {

	namespace iterator {

		template<typename It> It middle_point(It const&, It const&);

		namespace adl {
			// default forward iterator implementation
			template<typename It>
			It 
			middle_point( It const& itBegin, It const& itEnd ) {
				static_assert(std::is_same<It, It>::value, "provide more efficient middle_point or apply linear search");
				return itBegin;
			}

			template< typename UnaryPred, typename It >
			boost::filter_iterator<UnaryPred, It>
			middle_point( boost::filter_iterator<UnaryPred, It> const& itBegin, boost::filter_iterator<UnaryPred, It> const& itEnd ) {
				_ASSERT( itBegin.end()==itEnd.end() ); // should point within the same range
				boost::filter_iterator<UnaryPred, It> itMid( 
					itBegin.predicate(),
					tc::iterator::middle_point(
						itBegin.base(),
						itEnd.base()
					),
					itBegin.end()
				);
				if( itMid==itEnd ) {
					--itMid;
				}
				return itMid;
			}

			template< typename It >
			boost::indirect_iterator<It>
			middle_point( boost::indirect_iterator<It> const& itBegin, boost::indirect_iterator<It> const& itEnd ) {
				_ASSERTNOTIFYFALSE; // untested
				return boost::indirect_iterator<It>(
					tc::iterator::middle_point(
						itBegin.base(),
						itEnd.base()
					)
				);
			}

			template< typename It, typename UnaryPred >
			boost::reverse_iterator<It>
			middle_point( boost::reverse_iterator<It> const& itBegin, boost::reverse_iterator<It> const& itEnd ) {
				_ASSERTNOTIFYFALSE; // untested
				return boost::reverse_iterator<It>(
					tc::iterator::middle_point(
						itEnd.base(),
						itBegin.base()
					)
				);
			}

			template<typename UnaryFunction, typename It, typename Reference, typename Value>
			boost::transform_iterator<UnaryFunction, It, Reference, Value>
			middle_point( boost::transform_iterator<UnaryFunction, It, Reference, Value> const& itBegin, boost::transform_iterator<UnaryFunction, It, Reference, Value> const& itEnd ) {
				return boost::transform_iterator<UnaryFunction, It, Reference, Value>(
					tc::iterator::middle_point(
						itBegin.base(),
						itEnd.base()
					),
					itBegin.functor()
				);
			}

			// NodeBase is a base type of the multi_index_container's node (internal data
			// structure used to store elements).
			#if defined(BOOST_MULTI_INDEX_ENABLE_SAFE_MODE)
				template< typename NodeBase, typename OrderedIndex >
				boost::multi_index::safe_mode::safe_iterator<
					boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<NodeBase> >,
					OrderedIndex
				>
				middle_point( 
					boost::multi_index::safe_mode::safe_iterator<
						boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<NodeBase> >,
						OrderedIndex
					> itBegin,
					boost::multi_index::safe_mode::safe_iterator<
						boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<NodeBase> >,
						OrderedIndex
					> itEnd
				)
			#else
				template< typename NodeBase >
				boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<NodeBase> >
				middle_point( 
					boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<NodeBase> > itBegin,
					boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<NodeBase> > itEnd
				)
			#endif
			{
				typedef boost::multi_index::detail::ordered_index_node<NodeBase> node_type;
				typedef static_vector<
					node_type*, 
					2* // 2*log(N) is maximum hight of RB tree
						(8*sizeof(std::size_t)-3) // 2^3==8 roughly minimum size of node
				> TNodeVector;

				// The parent of the root of the tree is a special header node (representing end()) whose
				// parent is again the root node.
				auto PathToRoot=[]( node_type* pnode )->TNodeVector {
					typedef node_type node_type;
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
				_ASSERTEQUAL( vecpnodeBegin.back(), vecpnodeEnd.back() ); // both paths terminate at the root
				node_type* pnodeCommon=*boost::prior( boost::mismatch(
					boost::adaptors::reverse(vecpnodeBegin),
					boost::adaptors::reverse(vecpnodeEnd)
				).first ); // or second, same thing
				#if defined(BOOST_MULTI_INDEX_ENABLE_SAFE_MODE)
					return boost::multi_index::safe_mode::safe_iterator<boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<NodeBase> >, OrderedIndex>(pnodeCommon, const_cast<OrderedIndex*>(itBegin.owner()));
				#else
					return boost::multi_index::detail::bidir_node_iterator<boost::multi_index::detail::ordered_index_node<NodeBase> >(pnodeCommon);
				#endif
			}

			template< typename Container >
			boost::shared_container_iterator<Container> middle_point( boost::shared_container_iterator<Container> const& itBegin, boost::shared_container_iterator<Container> const& itEnd ) {
				return boost::shared_container_iterator<Container>(
					tc::iterator::middle_point( itBegin.base(), itEnd.base() ),
					itBegin.container_ref
				);
			}

			template<typename It>
			It middle_point_dispatch( It const& itBegin, It const& itEnd, boost::forward_traversal_tag ) {
				return middle_point(itBegin,itEnd);
			}

			// default random-access iterator implementation
			template<typename It>
			It middle_point_dispatch( It const& itBegin, It const& itEnd, boost::random_access_traversal_tag ) {
				return itBegin+(itEnd-itBegin)/2;
			}

		}

		template<typename It>
		It middle_point( It const& itBegin, It const& itEnd ) {
			return adl::middle_point_dispatch( itBegin, itEnd, typename boost::iterator_traversal<It>::type() );
		}

		template<typename It, typename UnaryPred>
		It internal_partition_point( It itBegin, It itEnd, UnaryPred pred ) {
		#ifdef _DEBUG /* is pred a partitioning? All true must be before all false. */
			It itPartitionPoint = itBegin;
			while( itPartitionPoint!=itEnd && pred(itPartitionPoint) ) ++itPartitionPoint;
			for( It itRest=itPartitionPoint; itRest!=itEnd; ++itRest ) {
				_ASSERT( !boost::implicit_cast<bool>(pred(itRest)) );
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
		It partition_point( It itBegin, It itEnd, UnaryPred pred ) {
			return internal_partition_point( tc_move(itBegin), tc_move(itEnd), [&pred](It it){
				return pred(*it);
			} );
		}

		template<typename It, typename UnaryPred>
		It partition_pair( It itBegin, It itEnd, UnaryPred pred ) {
			_ASSERT( itBegin!=itEnd );
			--itEnd;
			return internal_partition_point( tc_move(itBegin), tc_move(itEnd), [&pred](It it){
				return pred(*it,*std::next(it));
			} );
		}

		////////////////////////////////////////////////////////////////////////
		// Iterator functions forwarding to partition_point

		// std::bind1st/2nd require argument_type to be defined, at least in Dinkumware's implementation.
		// The standard does not require this for the predicate passed to lower_bound et. al.
		// We thus use boost::bind, which with explicit return type does not require any typedefs.

		template< typename It, typename Value, typename UnaryPredicate >
		It lower_bound(It itBegin,It itEnd,Value const& val,UnaryPredicate && pred) {
			return iterator::partition_point(itBegin,itEnd,boost::bind<bool>(std::forward<UnaryPredicate>(pred),_1,boost::cref(val)));
		}

		template< typename It, typename Value, typename UnaryPredicate >
		It upper_bound(It itBegin,It itEnd,Value const& val,UnaryPredicate && pred) {
			return iterator::partition_point(itBegin,itEnd,!boost::bind<bool>(std::forward<UnaryPredicate>(pred),boost::cref(val),_1));
		}

		template< typename It, typename Value, typename SortPredicate >
		bool binary_search(It itBegin,It itEnd,Value const& val,SortPredicate && pred) {
			It it = iterator::lower_bound(itBegin,itEnd,val,pred);
			return it!=itEnd && !pred(val,*it);
		}

		template< typename It, typename Value, typename SortPredicate >
		std::pair<It,It> equal_range(It itBegin,It itEnd,Value const& val,SortPredicate pred) {
			// Construct std::pair<It,It> initialized so that transform_iterator functor
			// does have to be neither default-constructible nor assignable. This is non-standard conformant,
			// but may be practical.
			It itEqualBegin=iterator::lower_bound(itBegin,itEnd,val,pred);
			return std::pair<It,It>( itEqualBegin, iterator::upper_bound(itEqualBegin,itEnd,val,pred) );
		}

		// According to http://www.open-std.org/jtc1/sc22/wg21/docs/lwg-defects.html#270
		// the less-than comparison may be carried out on unequal types. Thus 
		// we use boost::is_less instead of std::less.

		template< typename It, typename Value >
		It lower_bound(It itBegin,It itEnd,Value const& val) {
			return iterator::lower_bound( itBegin, itEnd, val, boost::is_less() );
		}

		template< typename It, typename Value >
		It upper_bound(It itBegin,It itEnd,Value const& val) {
			return iterator::upper_bound( itBegin, itEnd, val, boost::is_less() );
		}

		template< typename It, typename Value >
		bool binary_search(It itBegin,It itEnd,Value const& val) {
			return iterator::binary_search( itBegin, itEnd, val, boost::is_less() );
		}

		template< typename It, typename Value >
		std::pair<It,It> equal_range(It itBegin,It itEnd,Value const& val) {
			return iterator::equal_range( itBegin, itEnd, val, boost::is_less() );
		}
	}
	using namespace iterator;
	using iterator::middle_point;
}

