#pragma once

#include "range_defines.h"

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
	class const_iterator_ {
		struct wrapper: public It {
			typedef typename std::conditional< std::is_lvalue_reference< typename std::iterator_traits<It>::reference >::value
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
		typedef wrapper type;
	};

	template<typename T>
	struct const_iterator_<T*> {
		typedef T const* type;
	};

	template <
		typename Incrementable
		, typename CategoryOrTraversal
		, typename Difference
	>
	struct const_iterator_<boost::iterators::counting_iterator<Incrementable,CategoryOrTraversal,Difference>> {
		typedef boost::iterators::counting_iterator<Incrementable,CategoryOrTraversal,Difference> type;
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

		static index iterator2index( iterator it ) {
			return index(it, aggregate_tag());
		}

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
		typename std::conditional<
			std::is_convertible<
				typename std::iterator_traits<iterator>::reference,
				typename std::iterator_traits<const_iterator>::reference
			>::value,
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

		template<typename Rng>
		struct check_void_generator_functor : std::remove_cv<typename std::remove_reference<Rng>::type>::type {
			using base_ = typename std::remove_reference<Rng>::type;

			template< typename Func >
			typename std::enable_if<
				std::is_same<
					break_or_continue,
					typename tc::result_of< Rng( Func ) >::type
				>::value,
				break_or_continue
			>::type
			operator()(Func&& func) {
				return base_::operator()(std::forward<Func>(func));
			}

			template< typename Func >
			typename std::enable_if<
				std::is_same<
					void,
					typename tc::result_of< Rng( Func ) >::type
				>::value
			>::type
			operator()(Func func) {
				base_::operator()(ensure_non_break_or_continue_functor<Func>(func));
			}

			template< typename Func >
			typename std::enable_if<
				std::is_same<
					break_or_continue,
					typename tc::result_of< Rng( Func ) >::type
				>::value,
				break_or_continue
			>::type
			operator()(Func&& func) const {
				return base_::operator()(std::forward<Func>(func));
			}

			template< typename Func >
			typename std::enable_if<
				std::is_same<
					void,
					typename tc::result_of< Rng( Func ) >::type
				>::value
			>::type
			operator()(Func func) const {
				base_::operator()(ensure_non_break_or_continue_functor<Func>(func));
			}
		};
	}

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

			add_index_interface() {}

			template< typename Rhs >
			add_index_interface( Rhs && rhs, typename std::enable_if< !std::is_base_of< add_index_interface, typename std::decay< Rhs >::type >::value, unused_arg>::type=unused_arg() )
			:	m_rng( std::forward<Rhs>(rhs) )
			{}

			template< typename Rhs >
			add_index_interface( Rhs && rhs, typename std::enable_if< std::is_base_of< add_index_interface, typename std::decay< Rhs >::type >::value, unused_arg>::type=unused_arg() )
			:	m_rng(std::forward<Rhs>(rhs).m_rng)
			{}

			// explicitly define the copy ctor to do what the template above does, as it would if the implicit copy ctor wouldn't interfere
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

			operator typename std::remove_reference<Rng>::type &() {
				return *m_rng;
			};

			operator typename std::remove_reference<Rng>::type const&() const {
				return *m_rng;
			};
		private:
			reference_or_value< typename std::conditional< std::is_rvalue_reference<Rng>::value, typename std::remove_reference<Rng>::type, Rng >::type > m_rng;
		};

	public:
		typedef typename std::conditional< is_range_with_iterators< Rng >::value && !has_index< typename std::remove_reference<Rng>::type >::value,
			add_index_interface,
			Rng
		>::type type;
	};

	template<
		typename Rng,
		typename std::enable_if<
			is_range_with_iterators< Rng >::value &&
			!has_index< typename std::remove_reference<Rng>::type >::value
		>::type* = nullptr
	>
	auto ensure_index_range( Rng && rng )
		return_decltype(
			static_cast<typename index_range< Rng >::type>(std::forward<Rng>(rng))
		)

	template<
		typename Rng,
		typename std::enable_if<
			!(is_range_with_iterators< Rng >::value &&
			!has_index< typename std::remove_reference<Rng>::type >::value)
		>::type* = nullptr
	>
	auto ensure_index_range( Rng && rng )
		return_decltype(
			derived_or_base_cast<void_generator_type_check_impl::check_void_generator_functor<Rng&&>>(std::forward<Rng>(rng))
		)
}
