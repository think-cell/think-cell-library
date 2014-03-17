#pragma once

#include "range_defines.h"

#include "for_each.h"
#include "break_or_continue.h"
#include "meta.h"

#include "Library/Utilities/conversion_traits.h"
#include "Library/Utilities/reference_or_value.h"

#pragma warning( push )
#pragma warning( disable: 4018 )
#include <boost/range/reference.hpp>
#include <boost/range/difference_type.hpp>
#include <boost/range/category.hpp>
#pragma warning( pop )

#include <boost/range/iterator_range.hpp>

#include <boost/mpl/has_xxx.hpp>
#include <boost/mpl/logical.hpp>

#include <type_traits>

namespace RANGE_PROPOSAL_NAMESPACE {
	namespace iterator {
		template<typename It> It middle_point(It const&, It const&);
	}

	template< typename It >
	class const_iterator_ {
		struct wrapper: public It {
			typedef typename boost::mpl::if_< std::is_lvalue_reference< typename std::iterator_traits<It>::reference >
				, typename add_const_also_to_ref<
					typename std::iterator_traits<It>::reference
				>::type
				, typename std::iterator_traits<It>::value_type // TODO
			>::type reference;
			wrapper() {}
			wrapper( It const& rhs )
				: It( rhs )
			{}
			wrapper( wrapper const& rhs )
				: It( tc::base_cast<It>(rhs) )
			{}
			wrapper& operator=( It const& rhs ) {
				It::operator=( rhs );
				return *this;
			}
			wrapper& operator=( wrapper const& rhs ) {
				It::operator=( tc::base_cast<It>(rhs) );
				return *this;
			}
			reference operator*() const {
				return It::operator*();
			}
		};
	public:
		typedef typename boost::mpl::if_< std::is_pointer<It>
			, typename std::remove_pointer<It>::type const*
			, wrapper
		>::type type;
	};

	template< typename It, typename ConstIt > struct iterator_base;
	struct aggregate_tag {}; // tag to distinguish constructors that aggregate their single argument from templated copy constructors

	template<typename It>
	class index_from_iterator {
		template< typename ItIb, typename ConstIt > friend struct iterator_base;
		template< typename fIt > friend class index_from_iterator; // enable a const compatible index to be initialized.

		It m_it;
	public:

		index_from_iterator() {}

		template< typename Rhs >
		index_from_iterator( Rhs && rhs, aggregate_tag)
		:	m_it( std::forward<Rhs>(rhs) )
		{}
		template< typename Rhs >
		index_from_iterator( Rhs && rhs )
		:	m_it(std::forward<Rhs>(rhs).m_it)
		{}

		// explicitly define the copy constructor to do what the template above does, as it would if the implicit copy consturctor wouldn't interfere
		index_from_iterator( index_from_iterator const& rhs )
		:	m_it(rhs.m_it)
		{}
	};

	template< typename It, typename ConstIt=typename const_iterator_<It>::type >
	struct iterator_base {
		typedef It iterator;
		typedef ConstIt const_iterator;
		typedef index_from_iterator<It> index;

		static bool const index_valid_after_copy=true;

		static index iterator2index( iterator it ) {
			return index(it, aggregate_tag());
		}

		typename std::iterator_traits<iterator>::reference dereference_index(index const& idx) {
			return *idx.m_it;
		}

		// We cannot require that dereference_index(...) const returns the same type as const_iterator::operator*()
		// because we use the stronger iterator as index for mutable legacy ranges, which returns reference, with is not required
		// to be at all related to const_reference.
		// Simply casting iterator to const_iterator and dereferencing does not work either because references do not outlive
		// their iterators.
		// TODO: It is probably most sensible to impose the same requirement to this return value as to const_reference,
		// namely that it is convertible to value_type. A proxy encapsulating const_iterator, which we can create from iterator, would be a fine implementation then.
		typename boost::mpl::if_<
			std::is_convertible< 
				typename std::iterator_traits<iterator>::reference,
				typename std::iterator_traits<const_iterator>::reference
			>,
			typename std::iterator_traits<const_iterator>::reference,
			typename std::iterator_traits<iterator>::value_type
		>::type dereference_index(index const& idx) const {
			return *idx.m_it;
		}

		bool equal_index(index const& idxLhs, index const& idxRhs) const {
			return idxLhs.m_it==idxRhs.m_it;
		}

		void increment_index(index& idx) const {
			++idx.m_it;
		}

		void decrement_index(index& idx) const {
			--idx.m_it;
		}

		void advance_index(index& idx, typename std::iterator_traits<iterator>::difference_type d) const {
			idx.m_it+=d;
		}

		typename std::iterator_traits<iterator>::difference_type distance_to_index(index const& idxLhs, index const& idxRhs) const {
			return idxRhs.m_it-idxLhs.m_it;
		}

		void middle_point( index & idxBegin, index const& idxEnd ) const {
			idxBegin.m_it=tc::iterator::middle_point( idxBegin.m_it, idxEnd.m_it );
		}

		iterator make_iterator( index idx ) {
			return idx.m_it;
		}

		const_iterator make_iterator( index idx ) const {
			return idx.m_it;
		}
	};

	BOOST_MPL_HAS_XXX_TRAIT_DEF(index)

	////////////////////////////////////
	// adding index to legacy ranges

	template< typename T >
	struct is_boost_iterator_range : std::false_type {};

	template< typename T >
	struct is_boost_iterator_range< boost::iterator_range<T> > : std::true_type {};

	namespace range_generator_from_index_impl {
		class empty_chain {};

		template<
			typename Derived,
			typename Chain=empty_chain
		>
		struct range_generator_from_index : Chain {
		public:
			template< typename Func >
			auto operator()(Func func) -> tc::break_or_continue {
				for( auto idx=tc::derived_cast<Derived>(this)->begin_index();
					!tc::derived_cast<Derived>(this)->at_end_index(idx);
					tc::derived_cast<Derived>(this)->increment_index(idx)
				) {
					RETURN_IF_BREAK( tc::continue_if_void( func, tc::derived_cast<Derived>(this)->dereference_index(idx) ) );
				}
				return tc::continue_;
			}

			template< typename Func >
			auto operator()(Func func) const -> tc::break_or_continue {
				for( auto idx=tc::derived_cast<Derived>(this)->begin_index();
					!tc::derived_cast<Derived>(this)->at_end_index(idx);
					tc::derived_cast<Derived>(this)->increment_index(idx)
				) {
					RETURN_IF_BREAK( tc::continue_if_void( func, tc::derived_cast<Derived>(this)->dereference_index(idx) ) );
				}
				return tc::continue_;
			}
		};
	}
	using range_generator_from_index_impl::range_generator_from_index;

	template< typename Rng >
	struct index_range {
	private:
		struct add_index_interface : range_generator_from_index< add_index_interface, iterator_base<
			typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type,
			typename boost::range_iterator< typename std::remove_reference<Rng>::type const >::type 
		> > {
		private:
			// add_index_interface is deliberately not a range itself, e.g., it is missing begin() and end().
			// Users should use Rng directly instead, and use add_index_interface only to add the index interface.
			typedef iterator_base<
				typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type,
				typename boost::range_iterator< typename std::remove_reference<Rng>::type const >::type 
			> base_;

		public:
			using typename base_::index;
			static bool const index_valid_after_copy=std::is_reference<Rng>::value || /*char* ranges:*/std::is_pointer<Rng>::value || is_boost_iterator_range<Rng>::value;

			add_index_interface() {}

			template< typename Rhs >
			add_index_interface( Rhs && rhs, typename boost::disable_if< is_base_of< add_index_interface, typename remove_cvref< Rhs >::type >, unused_arg>::type=unused_arg() )
			:	m_rng( std::forward<Rhs>(rhs) )
			{}
			template< typename Rhs >
			add_index_interface( Rhs && rhs, typename boost::enable_if< is_base_of< add_index_interface, typename remove_cvref< Rhs >::type >, unused_arg>::type=unused_arg() )
			:	m_rng(std::forward<Rhs>(rhs).m_rng)
			{}
			// explicitly define the copy constructor to do what the template above does, as it would if the implicit copy consturctor wouldn't interfere
			add_index_interface( add_index_interface const& rhs)
			:	m_rng(rhs.m_rng)
			{}

			index begin_index() const {
				return index( boost::begin(m_rng.best_access()), aggregate_tag() );
			}
	
			index end_index() const {
				return index( boost::end(m_rng.best_access()), aggregate_tag() );
			}

			bool at_end_index(index const& idx) const {
				return this->equal_index( idx, end_index() );
			}

			operator Rng &() {
				return *m_rng;
			};

			operator Rng const&() const {
				return *m_rng;
			};
		private:
			reference_or_value<Rng> m_rng;
		};

	public:
		typedef typename boost::mpl::if_< boost::mpl::and_< is_range_with_iterators< Rng >, boost::mpl::not_< has_index< typename std::remove_reference<Rng>::type > > >,
			add_index_interface,
			Rng
		>::type type;
	};

	template< typename Rng > 
	auto ensure_index_range( Rng && rng ) 
		return_decltype ( static_cast< typename index_range<Rng>::type >(std::forward<Rng>(rng)) )
}
