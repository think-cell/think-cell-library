#pragma once

#include "range_defines.h"
#include "break_or_continue.h"
#include "meta.h"

#include "conversion_traits.h"
#include "reference_or_value.h"
#include "static_polymorphism.h"

#pragma warning( push )
#pragma warning( disable: 4018 )
#include <boost/range/reference.hpp>
#include <boost/range/difference_type.hpp>
#include <boost/range/category.hpp>
#pragma warning( pop )

#include <boost/range/iterator_range.hpp>

#ifdef TC_MAC
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#else
#pragma warning( push )
#pragma warning( disable: 4244 )
#endif
#include <boost/iterator/counting_iterator.hpp>
#ifdef TC_MAC
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

namespace RANGE_PROPOSAL_NAMESPACE {
	namespace iterator {
		template<typename It> It middle_point(It const&, It const&);
	}

	template< typename It >
	struct const_iterator_ {
	private:
		struct wrapper: public It {
			using reference=std::conditional_t< std::is_lvalue_reference< typename std::iterator_traits<It>::reference >::value
				, tc::add_const_also_to_ref_t<
					typename std::iterator_traits<It>::reference
				>
				, typename std::iterator_traits<It>::value_type // TODO
			>;
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
		using type = wrapper;
	};

	template<typename T>
	struct const_iterator_<T*> {
		using type = T const*;
	};

	template <
		typename Incrementable
		, typename CategoryOrTraversal
		, typename Difference
	>
	struct const_iterator_<boost::iterators::counting_iterator<Incrementable,CategoryOrTraversal,Difference>> {
		using type = boost::iterators::counting_iterator<Incrementable,CategoryOrTraversal,Difference>;
	};

	template< typename It, typename ConstIt > struct iterator_base;

	namespace index_from_iterator_impl {
		template<typename It>
		struct index_from_iterator {
			template< typename ItIb, typename ConstIt > friend struct RANGE_PROPOSAL_NAMESPACE::iterator_base;
			template< typename fIt > friend struct index_from_iterator; // enable a const compatible index to be initialized.

		private:
			It m_it;
		public:

			index_from_iterator() {}

			template< typename Rhs >
			index_from_iterator( Rhs&& rhs, aggregate_tag)
			:	m_it( std::forward<Rhs>(rhs) )
			{}
			template< typename Rhs >
			index_from_iterator( Rhs&& rhs, 
								typename std::enable_if< 
									std::is_constructible< It, decltype(Rhs::m_it) >::value,
									unused_arg 
								>::type = unused_arg() )
			:	m_it(std::forward<Rhs>(rhs).m_it)
			{}

			// explicitly define the copy constructor to do what the template above does, as it would if the implicit copy consturctor wouldn't interfere
			index_from_iterator( index_from_iterator const& rhs )
			:	m_it(rhs.m_it)
			{}
		};
	}
	using index_from_iterator_impl::index_from_iterator;

	template< typename It >
	index_from_iterator<std::decay_t<It>> iterator2index( It const& it ) {
		return index_from_iterator<std::decay_t<It>>(it, aggregate_tag());
	}

	template< typename It, typename ConstIt=typename const_iterator_<It>::type >
	struct iterator_base {
		using iterator = It;
		using const_iterator = ConstIt;
		using index = index_from_iterator<It>;

/*		iterator_base(iterator_base const&) {};
		template< typename OtherIt, typename OtherConstIt >
		explicit iterator_base( iterator_base<OtherIt,OtherConstIt> const&, typename std::enable_if<
			std::is_convertible<OtherIt,It>::value && std::is_convertible<OtherConstIt,ConstIt>::value
		, unused_arg >::type=unused_arg() ) {};
		template< typename OtherIt, typename OtherConstIt > typename std::enable_if<
			std::is_convertible<OtherIt,It>::value && std::is_convertible<OtherConstIt,ConstIt>::value
		, iterator_base& >::type operator=( iterator_base<OtherIt,OtherConstIt> const& ) { return *this; };
*/
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
		std::conditional_t<
			std::is_convertible<
				typename std::iterator_traits<iterator>::reference,
				typename std::iterator_traits<const_iterator>::reference
			>::value,
			typename std::iterator_traits<const_iterator>::reference,
			typename std::iterator_traits<iterator>::value_type
		> dereference_index(index const& idx) const {
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
		struct empty_chain {};

		template<
			typename Derived,
			typename Chain=empty_chain
		>
		struct range_generator_from_index : Chain {
			STATIC_VIRTUAL(begin_index)
			STATIC_VIRTUAL(end_index)
			STATIC_VIRTUAL(at_end_index)

			template< typename Func >
			tc::break_or_continue operator()(Func func) {
				for( auto idx=begin_index();
					!at_end_index(idx);
					this->increment_index(idx)
				) {
					RETURN_IF_BREAK( tc::continue_if_not_break( func, this->dereference_index(idx) ) );
				}
				return tc::continue_;
			}

			template< typename Func >
			tc::break_or_continue operator()(Func func) const {
				for( auto idx=begin_index();
					!at_end_index(idx);
					this->increment_index(idx)
				) {
					RETURN_IF_BREAK( tc::continue_if_not_break( func, this->dereference_index(idx) ) );
				}
				return tc::continue_;
			}
		};
	}
	using range_generator_from_index_impl::range_generator_from_index;

	namespace void_generator_type_check_impl {
		template<typename Func>
		struct ensure_non_break_or_continue_functor {
			explicit ensure_non_break_or_continue_functor(Func& f)
				: m_func(f)
			{}

			template<typename Arg>
			void operator()(Arg&& arg) {
				static_assert(
					!std::is_same<
						decltype(m_func(std::forward<Arg>(arg))),
						break_or_continue
					>::value,
					"Functor to void range must not return break_or_continue"
					);
				m_func(std::forward<Arg>(arg));
			}

		private:
			Func& m_func;
		};
	}

	template< typename Rng >
	struct index_range {
	private:
		struct add_index_interface : range_generator_from_index< add_index_interface, iterator_base<
			typename boost::range_iterator< std::remove_reference_t<Rng> >::type,
			typename boost::range_iterator< std::remove_reference_t<Rng> const >::type
		> > {
		private:
			// add_index_interface is deliberately not a range itself, e.g., it is missing begin() and end().
			// Users should use Rng directly instead, and use add_index_interface only to add the index interface.
			using this_type = add_index_interface;
		public:
			using index = typename this_type::index;

			add_index_interface() {}

			template< typename Rhs, std::enable_if_t< !std::is_base_of< add_index_interface, typename std::decay< Rhs >::type >::value>* =nullptr >
			add_index_interface( Rhs&& rhs )
			:	m_rng( std::forward<Rhs>(rhs), aggregate_tag() )
			{}

			STATIC_FINAL(begin_index)() const -> index {
				return index( boost::begin(m_rng.best_access()), aggregate_tag() );
			}
	
			STATIC_FINAL(end_index)() const -> index {
				return index( boost::end(m_rng.best_access()), aggregate_tag() );
			}

			STATIC_FINAL(at_end_index)(index const& idx) const -> bool {
				return this->equal_index( idx, this->end_index() );
			}

			operator std::remove_reference_t<Rng> &() {
				return *m_rng;
			};

			operator std::remove_reference_t<Rng> const&() const {
				return *m_rng;
			};
		private:
			reference_or_value< std::conditional_t< std::is_rvalue_reference<Rng>::value, std::remove_reference_t<Rng>, Rng > > m_rng;
		};

	public:
		using type=std::conditional_t< is_range_with_iterators< Rng >::value && !has_index< std::remove_reference_t<Rng> >::value,
			add_index_interface,
			Rng
		>;
	};
}
