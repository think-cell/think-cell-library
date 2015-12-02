#pragma once

#include "range_defines.h"
#include "container_traits.h"
#include "size.h"
#include "equal.h"
#include "quantifier.h"
#include "partition_iterator.h"
#include "partition_range.h"
#include "empty.h"
#include "sub_range.h"
#include "unique_range_adaptor.h"

#include "Library/ErrorReporting/storage_for.h"
#include "Library/ErrorReporting/functors.h"
#ifdef _DEBUG
#include "Library/Utilities/SetAndRestore.h"
#endif
#include <boost/algorithm/string/compare.hpp>
#include <boost/range/algorithm/copy.hpp>
#include <boost/range/algorithm/sort.hpp>
#include <boost/range/algorithm/reverse.hpp>
#include <boost/range/algorithm_ext/is_sorted.hpp>
#include <boost/range/algorithm/adjacent_find.hpp>
#include <boost/range/algorithm/reverse.hpp>
#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/utility.hpp>
#include <boost/implicit_cast.hpp>
#include <boost/filesystem.hpp>


#include <type_traits>
#include <vector>
#include <deque>
#include <set>

namespace RANGE_PROPOSAL_NAMESPACE {
	template< typename Rng, typename Less >
	bool is_strictly_sorted(Rng const& rng, Less&& less) {
		return boost::end(rng)==boost::adjacent_find( rng, !boost::bind<bool>( std::forward<Less>(less), _1, _2 ) );
	}
	template< typename Rng >
	bool is_strictly_sorted(Rng const& rng) {
		return is_strictly_sorted( rng, boost::is_less() );
	}

	template< typename Rng >
	bool all_same(Rng const& rng) {
		return RANGE_PROPOSAL_NAMESPACE::empty(rng)
			|| all_of(
				tc::drop_first(rng),
				boost::bind<bool>(boost::is_equal(), *boost::begin(rng), _1)
			);
	}

	template< range_return_value re, typename Rng, typename Pred >
	typename range_return<Rng,re>::type find_unique_if(Rng&& rng, Pred pred) {
		auto const itEnd=boost::end(rng);
		for( auto it=boost::begin(rng); it!=itEnd; ++it ) {
			if( pred(*it) ) {
				_ASSERT( std::none_of( boost::next(it), itEnd, pred ) );
				return range_return<Rng,re>::pack(it,std::forward<Rng>(rng));
			}
		}
		return range_return<Rng,re>::pack_singleton(std::forward<Rng>(rng));
	};

	namespace find_first_if_adl_barrier {
		template< range_return_value re >
		struct find_first_if_impl {
			template< typename Rng, typename Pred >
			typename range_return<Rng,re>::type operator()(Rng&& rng, Pred pred) const {
				auto const itEnd=boost::end(rng);
				for( auto it=boost::begin(rng); it!=itEnd; ++it ) {
					if( pred(*it) ) {
						return range_return<Rng,re>::pack(it,std::forward<Rng>(rng));
					}
				}
				return range_return<Rng,re>::pack_singleton(std::forward<Rng>(rng));
			};
		};
		template<>
		struct find_first_if_impl<tc::return_bool> {
			template< typename Rng, typename Pred >
			typename range_return<Rng,tc::return_bool>::type operator()(Rng&& rng, Pred&& pred) const {
				return any_of( std::forward<Rng>(rng), std::forward<Pred>(pred) );
			}
		};
	}

	template< range_return_value re, typename Rng, typename Pred >
	typename range_return<Rng,re>::type find_first_if(Rng&& rng, Pred&& pred) {
		return find_first_if_adl_barrier::find_first_if_impl<re>()( std::forward<Rng>(rng),std::forward<Pred>(pred) );
	};

	template< range_return_value re, typename Rng, typename Pred >
	typename range_return<Rng,re>::type find_last_if(Rng&& rng, Pred pred) {
		auto itBegin=boost::begin(rng);
		for( auto it=boost::end(rng); it!=itBegin; ) {
			--it;
			if( pred(*it) ) return range_return<Rng,re>::pack(it,std::forward<Rng>(rng));
		}
		return range_return<Rng,re>::pack_singleton(std::forward<Rng>(rng));
	}

	template <range_return_value re, typename Rng, typename It, typename Pred>
	typename range_return<Rng, re>::type find_closest_if(Rng&& rng, It it, Pred pred) {
		auto const itEnd = boost::end(rng);
		auto const itBegin = boost::begin(rng);
		auto itForward = it;

		auto PackResult=[&](It const& itResult, It const& itSingleton) {
			return itResult == itSingleton
				? range_return<Rng, re>::pack_singleton(std::forward<Rng>(rng))
				: range_return<Rng, re>::pack(itResult, std::forward<Rng>(rng));
		};

		for (;;) {
			if (itEnd == itForward) { return PackResult(tc::find_last_if<return_iterator_or_end>(tc::take(rng, it), pred), it); }
			if (pred(*itForward)) { return range_return<Rng, re>::pack(itForward, std::forward<Rng>(rng)); }
			++itForward;

			if (itBegin == it) { return PackResult(tc::find_first_if<return_iterator_or_end>(tc::drop(rng, itForward), pred), itEnd); }
			if (pred(*--it)) { return range_return<Rng, re>::pack(it, std::forward<Rng>(rng)); }
		}
	}

	template< range_return_value re, typename Rng, typename T >
	typename range_return<Rng,re>::type find_unique(Rng&& rng, T const& t) {
		return find_unique_if<re>( std::forward<Rng>(rng), boost::bind<bool>( tc::fn_equal_to(), _1, boost::cref(t) ) );
	};

	template< range_return_value re, typename Rng, typename T >
	typename range_return<Rng,re>::type find_first(Rng&& rng, T const& t) {
		return tc::find_first_if<re>( std::forward<Rng>(rng), boost::bind<bool>( tc::fn_equal_to(), _1, boost::cref(t) ) );
	};

	DEFINE_FN_TMPL(find_first, (range_return_value) )
	DEFINE_FN_TMPL(find_unique, (range_return_value))

	template< range_return_value re, typename Rng, typename T >
	typename range_return<Rng,re>::type find_last(Rng&& rng, T const& t) {
		return tc::find_last_if<re>( std::forward<Rng>(rng), boost::bind<bool>( tc::fn_equal_to(), _1, boost::cref(t) ) );
	};

	template<typename Rng>
	typename boost::range_iterator< std::remove_reference_t<Rng> >::type plurality_element(Rng&& rng) {
		_ASSERT( !tc::empty(rng) );
		using TVec = decltype(tc::sorted_iterator_range(rng, tc::fn_less()));

		return *( tc::accumulate(
			std::bind( tc::fn_ordered_for_each_occurrence(), tc::sorted_iterator_range( rng, tc::fn_less() ), tc::compare_less( fn_indirection() ), std::placeholders::_1 ),
			std::pair<
				typename boost::range_iterator<TVec const>::type,
				typename boost::range_size< TVec >::type
			>(), // value-initialized, second=0
			std::bind( tc::fn_assign_better(), std::placeholders::_1, std::placeholders::_2, tc::compare_greater( dot_member_second() ) )
		).first );
	}

	template< typename Rng, typename Pred >
	typename range_return<Rng,return_tail_or_empty>::type trim_left_if(Rng&& rng, Pred&& pred) {
		return tc::find_first_if<return_tail_or_empty>( std::forward<Rng>(rng), !boost::bind<bool>( std::forward<Pred>(pred), _1) );
	}

	template< typename Rng, typename Pred >
	typename range_return<Rng,return_head_next_or_empty>::type trim_right_if(Rng&& rng, Pred&& pred) {
		return tc::find_last_if<return_head_next_or_empty>( std::forward<Rng>(rng), !boost::bind<bool>( std::forward<Pred>(pred), _1) );
	}

	template< typename Rng, typename RngTrim >
	typename range_return<Rng,return_tail_or_empty>::type trim_left(Rng&& rng, RngTrim const& rngTrim) {
		return tc::trim_left_if( std::forward<Rng>(rng), boost::bind<bool>( tc::fn_find_first<tc::return_bool>(), boost::cref(rngTrim), _1 ) );
	}

	template< typename Rng, typename RngTrim >
	typename range_return<Rng,return_head_next_or_empty>::type trim_right(Rng&& rng, RngTrim const& rngTrim) {
		return tc::trim_right_if( std::forward<Rng>(rng), boost::bind<bool>( tc::fn_find_first<tc::return_bool>(), boost::cref(rngTrim), _1 ) );
	}

	template< typename Rng >
	auto distance_bounded(Rng const& rng, typename boost::range_size<Rng>::type nBound) return_ctor(
		size_proxy< typename boost::range_size<Rng>::type >, ( advance_forward_bounded( boost::begin(rng), nBound, boost::end(rng) ) )
	)

	template< typename Rng, typename T >
	bool contains_single(Rng const& rng, T const& t) {
		return 1==distance_bounded(rng,2) && *boost::begin( rng )==t;
	};

	////////////////////////
	// generic container algorithms

	template< typename Cont, typename Rng >
	std::enable_if_t<!has_mem_fn_lower_bound<Cont>::value, Cont&>
	cont_assign(Cont& cont, Rng&& rng) {
		tc::cont_clear(cont);
		tc::cont_append<tc::return_void>(cont, std::forward<Rng>(rng));
		return cont;
	}

	template< typename Cont, typename Rng >
	std::enable_if_t<has_mem_fn_lower_bound<Cont>::value, Cont&>
	cont_assign(Cont& cont, Rng&& rng) {
		tc::cont_clear(cont);
		tc::cont_must_insert_range(cont, std::forward<Rng>(rng));
		return cont;
	}

	namespace cont_assign_impl {
		template< typename T, std::size_t N >
		struct assign {
			assign(T (&at)[N])
			: m_at(at), m_i(0) {}
			
			template< typename Rhs >
			void operator()( Rhs&& rhs ) {
				m_at[m_i]=std::forward<Rhs>(rhs);
				++m_i;
			}

			~assign() {
				_ASSERTEQUAL( m_i, N );
			}
		private:
			T (&m_at)[N];
			std::size_t m_i;
		};
	}

	template< typename T, std::size_t N, typename Rng >
	void cont_assign(T (&at)[N], Rng&& rng) {
		for_each( std::forward<Rng>(rng), std::ref(make_lvalue(cont_assign_impl::assign<T,N>(at))) );
	}

	template<typename Cont, typename Rng, typename Flag>
	void cont_change_with_or(Cont& cont, Rng const& rng, Flag& flag, Flag flagChanged) {
		_ASSERTINITIALIZED(flag);
		if( flag==flagChanged ) {
			tc::cont_assign(cont, rng);
		} else if( tc::cont_change(cont, rng) ) {
			flag=tc_move(flagChanged);
		}
	}

	template<typename Cont, typename Rng>
	void cont_change_with_or(Cont& cont, Rng const& rng, bool& flag) {
		cont_change_with_or(cont, rng, flag, true);
	}

	template< typename T, typename A, typename Rng >
	bool cont_change(std::vector<T,A>& vec, Rng const& rng) {
		typename std::vector<T,A>::iterator itvec=boost::begin(vec);
		auto itrng=boost::const_begin(rng);
		for(;;) {
			if( itvec==boost::end(vec) ) {
				if( itrng==boost::const_end(rng) ) {
					return false;
				} else {
					break;
				}
			}
			if( itrng==boost::const_end(rng) || !tc::bool_cast(*itvec==*itrng) ) {
				tc::take_inplace( vec, itvec );
				break;
			}
			++itvec;
			++itrng;
		}
		NOBADALLOC( vec.insert( boost::end(vec), itrng, boost::const_end(rng) ) );
		return true;
	}

	template<typename T, std::size_t N, typename Rng>
	bool cont_change(T (&a)[N], Rng const& rng) {
		auto it = boost::begin(rng);
		bool bChanged = false;
		for(std::size_t i=0; i<N; ++i) {
			_ASSERT(it != boost::end(rng));
			bChanged = tc::change(a[i], *it) || bChanged;
			++it;
		}

		_ASSERT(it == boost::end(rng));
		return bChanged;
	}

	//	renew overload and cont_resize together guarantee that for N element insertions in a sequence of any of
	//		emplace_back
	//		pop_back
	//		insert (at end)
	//		erase (at end)
	//		resize
	//		renew (instead of clear)
	//	- no more than O(N) element moves
	//	- no more than O(log N) memory operations

	template< typename Cont >
	typename boost::range_size< Cont >::type cont_extended_memory(Cont const& cont) {
		// factor*cont.size() does not suffice for memory operation guarantee
		// 64 bit is enough to hold any memory money can buy
		return static_cast< typename Cont::size_type >(
			static_cast<std::uint64_t>(cont.capacity())*8/5
		);
	}

	template< typename Cont >
	Cont& cont_reserve( Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n ) noexcept {
		if( cont.capacity()<n ) {
			NOEXCEPT( cont.reserve(std::max(cont_extended_memory(cont),n)) );
		}
		return cont;
	}

	template< typename Cont >
	Cont& cont_resize( Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n ) {
		tc::cont_reserve(cont, n);
		cont.resize(n);
		return cont;
	}

	template< typename Cont >
	std::enable_if_t<!has_mem_fn_reserve<Cont>::value, Cont&>
	cont_clear( Cont& cont ) {
		cont.clear();
		return cont;
	}

	// erase elements in vector-like container to keep allocated memory
	template< typename Cont >
	std::enable_if_t<has_mem_fn_reserve<Cont>::value, Cont&>
	cont_clear( Cont& cont ) {
		cont.erase(boost::begin(cont),boost::end(cont));
		return cont;
	}

	template< typename Cont >
	typename boost::range_iterator<Cont>::type safe_cont_erase( Cont& cont, typename boost::range_iterator<Cont const>::type it ) {
		typename tc::range_value<std::remove_reference_t<Cont>>::type vt=std::move(*it); // *it may be const&
		return cont.erase(it);
	}

	// safer against reentrance in destructor of value by first moving the value out of the container, then erasing the element in the container and then letting the value go out of scope
	template< typename Cont >
	Cont& safe_cont_clear( Cont& cont ) {
		auto it=boost::end(cont);
		auto itBegin=boost::begin(cont);
		if( it!=itBegin ) {
			for(;;) {
				--it;
				if (it==itBegin) break;
				it=tc::safe_cont_erase(cont,it);
			}
			// special treatment of last iteration necessary because it!=itBegin cannot be tested anymore after itBegin has been erased
			tc::safe_cont_erase(cont,it);
		}
		_ASSERT(tc::empty(cont)); // no one put anything into the container during reentrance
		return cont;
	}

	template< typename Cont >
	Cont& cont_clear( Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n ) {
		cont_clear( cont );
		cont_resize( cont, n );
		return cont;
	}

	template< typename Cont >
	Cont& cont_extend( Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n ) {
		_ASSERT( cont.size()<=n );
		cont_resize( cont, n );
		return cont;
	}

	template< typename Cont >
	typename boost::range_reference< std::remove_reference_t<Cont> >::type cont_extend_at(Cont& cont, typename boost::range_size< std::remove_reference_t<Cont> >::type n) {
		if( cont.size()<=n ) {
			cont_resize( cont, n+1 );
		}
		return cont[n];
	}

	namespace cont_append_adl_barrier {
		// neither std::bind() in MSVC2013 nor boost::bind in Boost 1.58 support perfect forwarding
		// this may be ported to std::bind() if that is supported (MSVC2015?)
		template <typename Cont>
		struct mem_fn_emplace_back {
			mem_fn_emplace_back(Cont& cont)
				: m_cont(cont)
			{}

			template <typename... T, typename Cont = Cont>
			typename std::enable_if<has_mem_fn_emplace_back<Cont>::value, void>::type
			operator()(T&& ... value) const {
				NOBADALLOC( m_cont.emplace_back(std::forward<T>(value)...) );
			}

			template <typename T0, typename T1, typename... Ts, typename Cont = Cont>
			typename std::enable_if<!has_mem_fn_emplace_back<Cont>::value, void>::type
			operator()(T0&& v0, T1&& v1, Ts&& ... vs) const {
				NOBADALLOC( m_cont.push_back(typename tc::range_value<Cont>::type(std::forward<T0>(v0), std::forward<T1>(v1), std::forward<Ts>(vs)...)) );
			}

			template <typename T0, typename Cont = Cont>
			typename std::enable_if<!has_mem_fn_emplace_back<Cont>::value, void>::type
			operator()(T0&& v0) const {
				NOBADALLOC( m_cont.push_back(std::forward<T0>(v0)) );
			}

			template <typename Cont = Cont>
			typename std::enable_if<!has_mem_fn_emplace_back<Cont>::value, void>::type
			operator()() const {
				NOBADALLOC( m_cont.push_back(typename tc::range_value<Cont>::type()) );
			}

		private:
			Cont& m_cont;
		};
	}

	// do not use Cont::insert() or Cont(it, it)
	// iterators are slower than for_each in many cases (eg. filter ranges)

	// cont_append for target containers without reserve() member:
	// just run a for_each over the input
	// assume iterators are stable to get iterator to first inserted element
	template< range_return_value re, typename Cont, typename Rng >
	auto cont_append(Cont& cont, Rng&& rng) -> typename std::enable_if<!has_mem_fn_reserve<Cont>::value, typename range_return<Cont&,re>::type>::type {
		boost::optional<typename boost::range_iterator<Cont>::type> oit;
		if (!tc::empty(cont)) {
			oit = tc::end_prev(cont);
		}
		try {
			tc::for_each(std::forward<Rng>(rng), tc::cont_append_adl_barrier::mem_fn_emplace_back<Cont>(cont));
			return range_return<Cont&,re>::pack(
				oit ? boost::next(*oit) : boost::begin(cont),
				cont
			);
		} catch(...) {
			cont.erase(oit ? boost::next(*oit) : boost::begin(cont), boost::end(cont));
			throw;
		}
	}

	// cont_append for target containers with reserve() member and input range without tc::size():
	// just run a for_each over the input
	// assume random_access on container, and get iterator from offset
	template< range_return_value re, typename Cont, typename Rng >
	auto cont_append(Cont& cont, Rng&& rng) -> typename std::enable_if<has_mem_fn_reserve<Cont>::value && !tc::size_impl::has_size<Rng>::value, typename range_return<Cont&,re>::type>::type {
		typename Cont::size_type const nOffset = cont.size();
		try {
			tc::for_each(std::forward<Rng>(rng), tc::cont_append_adl_barrier::mem_fn_emplace_back<Cont>(cont));
			return range_return<Cont&,re>::pack(
				tc::begin_next(cont,nOffset),
				cont
			);
		} catch(...) {
			tc::take_first_inplace(cont,nOffset);
			throw;
		}
	}

	// cont_append for target containers with reserve() member and input range with tc::size():
	// same as above, but do a reserve() on the target container first
	template< range_return_value re, typename Cont, typename Rng >
	auto cont_append(Cont& cont, Rng&& rng) -> typename std::enable_if<has_mem_fn_reserve<Cont>::value && tc::size_impl::has_size<Rng>::value, typename range_return<Cont&,re>::type>::type {
		typename Cont::size_type const nOffset = cont.size();
		tc::cont_reserve(cont, nOffset + tc::size(rng));
		try {
			tc::for_each(std::forward<Rng>(rng), tc::cont_append_adl_barrier::mem_fn_emplace_back<Cont>(cont));
			return range_return<Cont&,re>::pack(
				tc::begin_next(cont,nOffset),
				cont
			);
		} catch(...) {
			tc::take_first_inplace(cont, nOffset);
			throw;
		}
	}

	template< range_return_value re, typename Cont, typename Arg >
	auto cont_find(Cont& cont, Arg&& arg) -> typename range_return<Cont&,re>::type {
		auto it=cont.find(std::forward<Arg>(arg));
		if( it==boost::end(cont) ) {
			return range_return<Cont&,re>::pack_singleton(
				cont
			);
		} else {
			return range_return<Cont&,re>::pack(
				tc_move(it),
				cont
			);
		}
	}

	template<typename Rng>
	typename std::enable_if< tc::is_char< typename tc::range_value<Rng>::type >::value,
	void >::type assert_no_null_terminator(Rng const& rng) {
		_ASSERT( !tc::find_first<tc::return_bool>(rng, tc::char_cast<typename tc::range_value<Rng>::type>('\0') ));
	}

	template<typename Rng>
	typename std::enable_if< !tc::is_char< typename tc::range_value<Rng>::type >::value,
	void >::type assert_no_null_terminator(Rng const& rng) {}

	template<typename Rng>
	void remove_null_terminator(Rng& rng) {
		static_assert( tc::is_char< typename tc::range_value<Rng>::type >::value, "" );
		_ASSERT( !tc::empty(rng) );
		_ASSERTEQUAL( tc_back(rng), tc::char_cast< typename tc::range_value<Rng>::type >('\0') );
		tc::take_inplace(rng,tc::end_prev(rng));
		tc::assert_no_null_terminator(rng);
	};

	template<typename Cont, typename Func>
	Cont get_truncating_buffer(Func func) {
		static_assert( tc::is_decayed<Cont>::value, "" );
		Cont cont;
		tc::cont_extend(cont,0<cont.capacity() ? cont.capacity() : tc::numeric_cast<typename Cont::size_type>(8));

		// sentinel to detect buffer overrun
		typename boost::range_size<Cont>::type const nSentinel=
#ifdef _DEBUG
			1
#else
			0
#endif
			;
		for (;;) {
			auto const nSize =
#ifdef _DEBUG
			 [&] {
				tc::uninitialize(tc_back(cont));
				scope_exit( _ASSERTDEBUG( !tc::check_initialized(tc_back(cont))) );
				return 
#endif
					func(tc::ptr_begin(cont), tc::size(cont)-nSentinel);
#ifdef _DEBUG
			}();
#endif
			if (nSize < tc::size(cont)-nSentinel) {
				_ASSERT(0 <= nSize);
				tc::take_first_inplace(cont, nSize);
				tc::assert_no_null_terminator(cont);
				return cont;
			}
			_ASSERTEQUAL(nSize, tc::size(cont)-nSentinel);
			tc::cont_clear(cont,tc::cont_extended_memory(cont));
		}
	}

	template<typename Cont, typename Func>
	Cont get_sized_buffer_may_be_null_terminated(Func func) {
		static_assert( tc::is_decayed<Cont>::value, "" );

		// sentinel to detect buffer overrun
		typename boost::range_size<Cont>::type const nSentinel=
#ifdef _DEBUG
			1
#else
			0
#endif
			;

		Cont cont;
		static_assert( std::is_trivially_copyable<std::decay_t<decltype(*tc::ptr_begin(cont))>>::value, "" );
		tc::cont_extend(cont,std::max(cont.capacity(),nSentinel));

		for (;;) {
			auto const nSize = 
#ifdef _DEBUG
			 [&] {
				tc::fill_with_dead_pattern(tc_back(cont));
				scope_exit( tc::assert_dead_pattern(tc_back(cont)) );
				return 
#endif
				func(tc::ptr_begin(cont), tc::size(cont)-nSentinel);
#ifdef _DEBUG
			}();
#endif
			if (nSize <= tc::size(cont)-nSentinel) {
				_ASSERT(0 <= nSize);
				tc::take_first_inplace(cont, nSize);
				return cont;
			}
			tc::cont_clear(cont,nSize+nSentinel);
		}
	}

	template<typename Cont, typename Func>
	Cont get_sized_buffer(Func&& func) {
		auto cont=tc::get_sized_buffer_may_be_null_terminated<Cont>(std::forward<Func>(func));
		tc::assert_no_null_terminator(cont);
		return cont;
	}


	template<typename Cont, typename Func>
	Cont get_sized_null_terminated_buffer(Func&& func) {
		static_assert( tc::is_char< typename tc::range_value<Cont>::type >::value, "" );
		auto cont=tc::get_sized_buffer_may_be_null_terminated<Cont>(std::forward<Func>(func));
		tc::remove_null_terminator(cont);
		return cont;
	}

	/////////////////////////////////
	// associative containers

	// std::set/map returns pair with bool=inserted?
	template< typename It >
	It && verify_inserted(std::pair<It,bool>&& pairitb) {
		_ASSERT(pairitb.second);
		return tc_move(pairitb.first);
	};

	// std::multiset/multimap always inserts and thus returns only iterator
	template< typename It >
	It && verify_inserted(It&& it) {
		return std::forward<It>(it);
	};

	template< typename Cont, typename TValue > // use extra template parameter instead of Cont::value_type to have both move and copy semantics
	typename Cont::iterator cont_must_insert(Cont& cont, TValue&& val) {
		return verify_inserted( NOBADALLOC(cont.insert(std::forward<TValue>(val))) );
	}

	DEFINE_FN(insert);
	DEFINE_FN(cont_must_insert);

	template< typename Cont, typename Rng >
	Cont& cont_try_insert_range(Cont& cont, Rng&& rng) {
		/*
			It might be more efficient for a container to insert a range at once
			with insert(begin(rng),end(rng)), but on the other hand, it is more
			efficient to access a range as generator range: If filters and costly
			transforms are involved, a generator range needs to dereference only once.
		*/
		tc::for_each(
			std::forward<Rng>(rng),
			boost::bind<void>(mem_fn_insert(), &cont, _1)
		);
		return cont;
	}

	template< typename Cont, typename Rng >
	Cont& cont_must_insert_range(Cont& cont, Rng&& rng) {
		tc::for_each(
			std::forward<Rng>(rng),
			boost::bind<void>(fn_cont_must_insert(), boost::ref(cont), _1)
		);
		return cont;
	}

	template< typename Cont, typename... Args >
	typename Cont::iterator cont_must_emplace(Cont& cont, Args&& ... args) {
		return verify_inserted( NOBADALLOC(cont.emplace(std::forward<Args>(args)...)) );
	}

	template< typename Cont, typename... Args >
	std::pair< typename Cont::iterator, bool > cont_try_emplace(Cont& cont, Args&& ... args) {
		return NOBADALLOC(cont.emplace(std::forward<Args>(args)...));
	}

	template< typename Cont, typename... Args >
	typename Cont::iterator cont_must_emplace_before(Cont& cont, typename Cont::const_iterator itHint, Args&& ... args) {
		auto c=cont.size();
		typename Cont::iterator it = NOBADALLOC(cont.emplace_hint(itHint, std::forward<Args>(args)...));
		_ASSERTEQUAL( cont.size(), c+1 );
		_ASSERT( boost::next(it)==itHint );
		return it;
	}

	template<typename Map, typename K, typename V, typename Better>
	void map_try_emplace_better(Map& map, K&& key, V&& val, Better&& better) {
		auto it = map.lower_bound(key);
		if (boost::end(map) == it || map.key_comp()(key, it->first)) {
			cont_must_emplace_before(map, it, std::forward<K>(key), std::forward<V>(val));
		} else {
			tc::assign_better(it->second, std::forward<V>(val), std::forward<Better>(better));
		}
	}

	template< typename Cont >
	void cont_must_erase(Cont& cont, typename Cont::key_type const& data) {
		VERIFYEQUAL( cont.erase(data), 1u );
	}

	template< typename Cont >
	void cont_try_erase(Cont& cont, typename Cont::key_type const& data) {
		VERIFY( cont.erase(data)<=1 );
	}

	template<typename Cont, typename Enable=void>
	struct range_filter;

	template<typename Cont>
	struct range_filter<Cont, typename std::enable_if< 
		has_efficient_erase<std::decay_t<Cont>>::value
		|| has_mem_fn_lower_bound<std::decay_t<Cont>>::value
	>::type >: tc::noncopyable {
		static_assert( tc::is_decayed< Cont >::value, "" );
		using iterator = typename boost::range_iterator<Cont>::type;
		using const_iterator = iterator; // no deep constness (analog to sub_range)

	private:
		Cont& m_cont;
		iterator m_itOutputEnd;

	public:
		explicit range_filter(Cont& cont)
			: m_cont(cont)
			, m_itOutputEnd(boost::begin(cont))
		{}

		range_filter(Cont& cont, iterator const& itStart)
			: m_cont(cont)
			, m_itOutputEnd(itStart)
		{}

		~range_filter() {
			tc::take_inplace( m_cont, m_itOutputEnd );
		}

		void keep(iterator it) {
			_ASSERT( 0<=std::distance(m_itOutputEnd,it) );
			m_itOutputEnd=m_cont.erase(m_itOutputEnd,it);
			++m_itOutputEnd;
		}

		///////////////////////////////////////////
		// range interface for output range
		// no deep constness (analog to sub_range)

		iterator begin() const {
			return boost::begin(tc::make_mutable(m_cont));
		}

		iterator end() const {
			return m_itOutputEnd;
		}

		void pop_back() {
			_ASSERT( m_itOutputEnd!=boost::begin(m_cont) );
			--m_itOutputEnd;
			m_itOutputEnd=m_cont.erase(m_itOutputEnd);
		}
	};

	template<typename Cont>
	struct range_filter< Cont, typename std::enable_if< 
		has_mem_fn_splice_after< std::decay_t<Cont> >::value
	>::type >: Cont, private tc::noncopyable {
		static_assert( tc::is_decayed< Cont >::value, "" );
		using typename Cont::iterator;
		using const_iterator = iterator; // no deep constness (analog to sub_range)

	private:
		Cont& m_contInput;
		iterator m_itLastOutput;

	public:
		explicit range_filter(Cont& cont)
			: m_contInput(cont)
			, m_itLastOutput(before_begin())
		{}

		explicit range_filter(Cont& cont, iterator const& itStart)
			: range_filter(cont)
		{
			for(;;) {
				auto it=boost::begin(m_contInput);
				if( it==itStart ) break;
				this->splice_after(m_itLastOutput,m_contInput.before_begin());
				m_itLastOutput=it;
			}
		}

		~range_filter() {
			m_contInput=tc_move_always( tc::base_cast<Cont>(*this) );
		}

		void keep(iterator it) {
			while( it!=boost::begin(m_contInput) ) tc::drop_first_inplace(m_cont);
			this->splice_after(m_itLastOutput,m_contInput.before_begin());
			m_itLastOutput=it;
		}
	};

	template<typename Cont>
	struct range_filter< Cont, typename std::enable_if< 
		has_mem_fn_splice< std::decay_t<Cont> >::value
	>::type >: Cont, private tc::noncopyable {
		static_assert( tc::is_decayed< Cont >::value, "" );
		Cont& m_contInput;
		using typename Cont::iterator;
		using const_iterator = iterator; // no deep constness (analog to sub_range)
	
		explicit range_filter(Cont& cont)
			: m_contInput(cont)
		{}

		range_filter(Cont& cont, iterator const& itStart)
			: m_contInput(cont)
		{
			this->splice( boost::end(*this), m_contInput, boost::begin(m_contInput), itStart );
		}

		~range_filter() {
			m_contInput=tc_move_always( tc::base_cast<Cont>(*this) );
		}

		void keep(iterator it) {
			_ASSERT( it!=boost::end(m_contInput) );
			this->splice( 
				boost::end(*this),
				m_contInput,
				m_contInput.erase( boost::begin(m_contInput), it )
			);
		}
	};


	template< typename T >
	struct is_deque : std::false_type {};

	template< typename Ty, typename Alloc >
	struct is_deque< std::deque<Ty,Alloc> > : std::true_type {};

	template <typename T>
	struct range_filter_by_move_element : std::integral_constant<bool,
		tc::is_vector<T>::value || tc::is_basic_string<T>::value || tc::is_deque<T>::value
	> {};

	template<typename Cont>
	struct range_filter<
		Cont,
		typename std::enable_if<range_filter_by_move_element<std::decay_t<Cont>>::value>::type
	>: tc::noncopyable {
		static_assert( tc::is_decayed< Cont >::value, "" );
		using iterator = typename boost::range_iterator<Cont>::type;
		using const_iterator = iterator; // no deep constness (analog to sub_range)

	protected:
		Cont& m_cont;
		iterator m_itOutput;

	private:
#ifdef _CHECKS
		iterator m_itFirstValid;
#endif

	public:
		explicit range_filter(Cont& cont)
			: m_cont(cont)
			, m_itOutput(boost::begin(cont))
#ifdef _CHECKS
			, m_itFirstValid(boost::begin(cont))
#endif
		{}

		range_filter(Cont& cont, iterator itStart)
			: m_cont(cont)
			, m_itOutput(itStart)
#ifdef _CHECKS
			, m_itFirstValid(itStart)
#endif
		{}

		~range_filter() {
			tc::take_inplace( m_cont, m_itOutput );
		}

		void keep(iterator it) {
#ifdef _CHECKS
			// Filter without reordering 
			_ASSERT( 0<=std::distance(m_itFirstValid,it) );
			m_itFirstValid=it;
			++m_itFirstValid;
#endif
			*m_itOutput=tc_move_always(*it);
			++m_itOutput;
		}

		///////////////////////////////////
		// range interface for output range
		// no deep constness (analog to sub_range)

		iterator begin() const {
			return boost::begin(tc::make_mutable(m_cont));
		}

		iterator end() const {
			return m_itOutput;
		}

		void pop_back() {
			_ASSERT( boost::begin(m_cont)!=m_itOutput );
			--m_itOutput;
		}
	};

	template<typename Cont>
	struct range_filter<
		tc::sub_range< Cont& >,
		typename std::enable_if<range_filter_by_move_element<Cont>::value>::type
	> {
		using iterator = typename boost::range_iterator<Cont>::type;
		using const_iterator = iterator; // no deep constness (analog to sub_range)

		range_filter(tc::sub_range< Cont& >& rng) : m_rng(rng)	{
			_ASSERT(boost::end(m_rng)==boost::end(Container())); // otherwise, we would need to keep [ end(m_rng), end(Container()) ) inside dtor
			m_rngfilter.ctor(Container(), boost::begin(rng));
		}

		void keep(iterator it) {
			m_rngfilter->keep(it);
		}

		iterator begin() const {
			return boost::begin(m_rng);
		}

		iterator end() const {
			return boost::end(*m_rngfilter);
		}

		void pop_back() {
			_ASSERT(boost::end(*this)!=boost::begin(*this));
			m_rngfilter->pop_back();
		}

		~range_filter() {
			auto& cont=Container();
			auto const nIndexBegin=boost::begin(m_rng)-boost::begin(cont);
			m_rngfilter.dtor(); // erases cont tail and invalidates iterators in m_rng
			m_rng=tc::drop_first(cont, nIndexBegin);
		}
	private:
		Cont& Container() const {
			return boost::implicit_cast<Cont&>(m_rng.base_range());
		}

		tc::sub_range< Cont& >& m_rng;
		tc::storage_for< tc::range_filter<Cont> > m_rngfilter;
	};

	/////////////////////////////////////////////////////
	// filter_inplace

	template<typename Cont, typename Pred>
	Cont & filter_inplace(Cont & cont, typename boost::range_iterator< std::remove_reference_t<Cont> >::type it, Pred pred) {
		for (auto const itEnd = boost::end(cont); it != itEnd; ++it) {
			if (!pred(*it)) {
				tc::range_filter< std::decay_t<Cont> > rngfilter(cont, it);
				++it;
				while (it != itEnd) {
					if (pred(*it)) {
						rngfilter.keep(it++); // may invalidate it, so move away first
					}
					else {
						++it;
					}
				}
				break;
			}
		}
		return cont;
	}

	template<typename Cont, typename Pred>
	Cont & filter_inplace(Cont& cont, Pred&& pred) {
		return tc::filter_inplace( cont, boost::begin(cont), std::forward<Pred>(pred) );
	}

	// cannot use list::remove because T may not be list::value_type
	// cannot use key-based lookup for set/map because T may not be Cont::value_type and !Cont::predicate()(a,b) && !Cont::predicate()(b,a) may not be the same as ==
	template<typename Cont, typename T>
	Cont& remove_inplace(Cont& cont, T const& t) {
		return tc::filter_inplace( cont, !boost::bind<bool>( boost::is_equal(), _1, boost::cref(t) ) );
	}

	/////////////////////////////////////////////////////
	// remove_count_erase

	template<typename Cont, typename Pred>
	typename tc::size_proxy< typename boost::range_size<Cont>::type > remove_count_erase_if( Cont& cont, Pred pred ) {
		typename boost::range_size<Cont>::type count=0;
		tc::filter_inplace( cont, [&]( typename boost::range_reference<Cont>::type t )->bool {
			bool const b=pred(tc_move_if_owned(t));
			count+=boost::implicit_cast<
#ifndef _MSC_VER
				typename
#endif
				boost::range_size<Cont>::type
			>(b);
			return !b;
		} );
		return tc::size_proxy< typename boost::range_size<Cont>::type >(count);
	}

	template<typename Cont, typename T>
	typename tc::size_proxy< typename boost::range_size<Cont>::type > remove_count_erase(Cont& cont, T const& t) {
		return remove_count_erase_if( cont, boost::bind<bool>( boost::is_equal(), _1, boost::cref(t) ) );
	}

	/////////////////////////////////////////////////////
	// sort

	template<typename Rng, typename Pred>
	typename std::enable_if< has_mem_fn_sort< tc::remove_cvref_t<Rng> >::value,
	Rng& >::type sort(Rng& rng, Pred&& pred) {
		rng.sort( std::forward<Pred>(pred) );
		return rng;
	}
	template<typename Rng, typename Pred>
	typename std::enable_if<!has_mem_fn_sort< tc::remove_cvref_t<Rng> >::value, 
	Rng& >::type sort(Rng& rng, Pred&& pred) {
		boost::sort( rng, std::forward<Pred>(pred) );
		return rng;
	}
	template<typename Rng>
	Rng& sort(Rng& rng) {
		return tc::sort( rng, boost::is_less() );
	}

	/////////////////////////////////////////////////////
	// reverse_inplace
	// inplace algorithms should accept only lvalue containers, but reverse_inplace is called with
	// sub_ranges of containers, so it must accept rvalues.

	template<typename Rng>
	typename std::enable_if< has_mem_fn_reverse< tc::remove_cvref_t<Rng> >::value, Rng&& >::type reverse_inplace(Rng&& rng) {
		rng.reverse();
		return std::forward<Rng>(rng);
	}
	template<typename Rng>
	typename std::enable_if<!has_mem_fn_reverse< tc::remove_cvref_t<Rng> >::value, Rng&& >::type reverse_inplace(Rng&& rng) {
		boost::reverse( rng );
		return std::forward<Rng>(rng);
	}

	template<typename Rng, typename Less>
	auto ordered_unique(Rng&& rng, Less less) code_return_decltype (
		_ASSERTDEBUG( boost::range::is_sorted( rng, less ) );,
		tc::adjacent_unique( std::forward<Rng>(rng), !boost::bind<bool>( tc_move(less), _1, _2 ) )
	)

	template<typename Rng>
	auto ordered_unique(Rng&& rng) return_decltype (
		tc::ordered_unique( std::forward<Rng>(rng), tc::fn_less() )
	)

	template<typename Rng, typename Less>
	auto sort_unique(Rng&& rng, Less less) code_return_decltype (
		tc::sort( rng, less );,
		tc::ordered_unique( std::forward<Rng>(rng), tc_move(less) )
	)

	template<typename Rng>
	auto sort_unique(Rng&& rng) return_decltype(
		sort_unique( std::forward<Rng>(rng), tc::fn_less() )
	)

	///////////////////////////////////////
	// partition ranges into subranges

	template<typename Rng, typename Less>
	auto ordered_unique_range(Rng&& rng, Less less) code_return_decltype (
		_ASSERTDEBUG( boost::range::is_sorted( rng, less ) );,
		tc::adjacent_unique_range( std::forward<Rng>(rng), !boost::bind<bool>( tc_move(less), _1, _2 ) )
	)

	template<typename Rng>
	auto ordered_unique_range(Rng&& rng) return_decltype (
		tc::ordered_unique_range( std::forward<Rng>(rng), tc::fn_less() )
	)

	template<typename Rng, typename Less>
	auto sort_unique_range(Rng&& rng, Less less) code_return_decltype (
		tc::sort( rng, less );,
		tc::ordered_unique_range( std::forward<Rng>(rng), tc_move(less) )
	)

	template<typename Rng>
	auto sort_unique_range(Rng&& rng) return_decltype(
		sort_unique_range( std::forward<Rng>(rng), tc::fn_less() )
	)

	template< typename Rng, typename Pred, typename Func >
	break_or_continue for_each_subrange_where(Rng&& rng, Pred pred, Func func) {
		auto it=boost::begin(rng);
		auto const itEnd=boost::end(rng);
		for(;;) {
			for(;;) {
				if( it==itEnd ) return continue_;
				if( pred(*it) ) break;
				++it;
			}
			auto itStart=it;
			do {
				++it;
				if( it==itEnd ) {
					return continue_if_not_break( func, slice( rng, itStart, it ) ); // may throw
				};
			} while( pred(*it) );
			if( break_==continue_if_not_break( func, slice( rng, itStart, it ) ) ) return break_; // may throw
			++it;
		}
	};

	DEFINE_FN( for_each_subrange_where );

	template< typename Rng, typename Less, typename Accu >
	Rng&& sort_accumulate_each_unique_range(Rng&& cont, Less less, Accu accu) {
		tc::sort( cont, less );
		{ range_filter< std::decay_t<Rng> > rngfilter( cont );
			tc::for_each(
				tc::ordered_unique_range(
					cont,
					tc_move(less)
				),
				[&accu,&rngfilter]( typename make_sub_range_result< std::add_lvalue_reference_t<Rng> >::type rngEqualSubRange ) {
					for(
						auto it=tc::begin_next(rngEqualSubRange);
						it!=boost::end(rngEqualSubRange);
						++it
					) {
						accu( *boost::begin(rngEqualSubRange), *it );
					}
					rngfilter.keep( boost::begin(rngEqualSubRange) );
				}
			);
		}
		return std::forward<Rng>(cont);
	}

	/*
		In contrase to std::unique, tc::adjacent_unique / tc::adjacent_unique_inplace always compares adjacent elements. This allows implementing
		bidirectional tc::adjacent_unique, with tc::adjacent_unique_inplace yielding the same result.
	*/
	template< typename Cont, typename Equals=tc::fn_equal_to >
	Cont& adjacent_unique_inplace( Cont & cont, Equals&& pred=Equals() ) {
		{
			tc::range_filter< std::decay_t<Cont> > rngfilter(cont);
			auto rngUnique = tc::adjacent_unique(cont, std::forward<Equals>(pred));

			auto it = boost::begin(rngUnique);
			auto const itEnd = boost::end(rngUnique);

			while (itEnd != it) {
				rngfilter.keep(it++.base());
			}
		}
		return cont;
	}

	template<typename Cont, typename Less=tc::fn_less>
	Cont& ordered_unique_inplace( Cont& cont, Less less=Less() ) {
		_ASSERTDEBUG( boost::range::is_sorted( cont, less ) );
		return tc::adjacent_unique_inplace( cont, !boost::bind<bool>( tc_move(less), _1, _2 ) );
	}

	template< typename Cont, typename Less=tc::fn_less >
	Cont& sort_unique_inplace(Cont& cont, Less less=Less()) {
		tc::sort( cont, less );
		return tc::ordered_unique_inplace( cont, tc_move(less) );
	}

	DEFINE_FN( sort_unique_inplace );

	template<typename Cont, typename Rng>
	typename std::enable_if<std::is_constructible<Cont, Rng&&>::value, Cont>::type
	copy_range(Rng&& rng) {
		return tc::verify_class<Cont>(std::forward<Rng>(rng));
	}

	template<typename Cont, typename Rng>
	typename std::enable_if<!std::is_constructible<Cont, Rng&&>::value && !has_mem_fn_lower_bound<Cont>::value, Cont>::type
	copy_range(Rng&& rng) {
		Cont cont;
 		tc::cont_append<tc::return_void>(cont, std::forward<Rng>(rng));
		return cont;
	}

	template<typename Cont, typename Rng>
	typename std::enable_if<!std::is_constructible<Cont, Rng&&>::value && has_mem_fn_lower_bound<Cont>::value, Cont>::type
	copy_range(Rng&& rng) {
		Cont cont;
		// force each element to be inserted
		// if there is need to run copy_range with cont_try_insert_range (e.g. for removal of duplicates) implement a new function
		tc::cont_must_insert_range(cont, std::forward<Rng>(rng));
		return cont;
	}

	template< typename Rng >
	auto make_vector(Rng&& rng) return_decltype(
		tc::copy_range<std::vector<typename tc::range_value<std::remove_reference_t<Rng>>::type>>(std::forward<Rng>(rng))
	)

	template< typename Rng >
	auto make_basic_string(Rng&& rng) return_decltype(
		tc::copy_range<std::basic_string<typename tc::range_value<std::remove_reference_t<Rng>>::type>>(std::forward<Rng>(rng))
	)

	template< typename Char, typename Traits, typename Allocator >
	Char const* as_c_str(std::basic_string< Char, Traits, Allocator > const& str)
	{
		return str.c_str();
	}

	template<typename Char, std::enable_if_t< tc::is_char<Char>::value >* = nullptr>
	typename std::enable_if< tc::is_char<Char>::value, Char const* >::type as_c_str(Char const* psz) {
		return psz;
	}

	inline boost::filesystem::path::value_type const* as_c_str(boost::filesystem::path const& path) {
		return path.c_str();
	}

	template< tc::range_return_value re, typename Rng, typename T, typename Pred >
	typename tc::range_return<Rng,re>::type binary_find_unique(Rng&& rng, T const& t, Pred pred) {
		// The result of tc::binary_find_unique must be unambiguous. In general, this means that rng is strictly
		// ordered. In some cases, it is convenient to allow multiple occurrences of the same item in
		// rng, which is not a problem as long as these items are not searched for.
		_ASSERTDEBUG( boost::range::is_sorted( rng, pred ) );
		auto it=tc::lower_bound<tc::return_iterator>( rng, t, pred );
		if( it==boost::end( rng ) || pred( t, *it ) ) {
			return tc::range_return<Rng,re>::pack_singleton(std::forward<Rng>(rng));
		} else {
	#ifdef _CHECKS
			auto itNext = boost::next(it);
			_ASSERT( boost::end(rng)==itNext || pred(t, *itNext) );
	#endif
			return tc::range_return<Rng,re>::pack(it,std::forward<Rng>(rng));
		}
	}

	template< tc::range_return_value re, typename Rng, typename T >
	typename tc::range_return<Rng,re>::type binary_find_unique(Rng&& rng, T const& t) {
		return tc::binary_find_unique<re>( std::forward<Rng>(rng), t, tc::fn_less() );
	}

	template< tc::range_return_value re, typename Rng, typename T, typename Pred >
	typename tc::range_return<Rng,re>::type binary_find_first(Rng&& rng, T const& t, Pred pred) {
		_ASSERTDEBUG( boost::range::is_sorted( rng, pred ) );
		auto it=tc::lower_bound<tc::return_iterator>( rng, t, pred );
		if( it==boost::end( rng ) || pred( t, *it ) ) {
			return tc::range_return<Rng,re>::pack_singleton(std::forward<Rng>(rng));
		} else {
			return tc::range_return<Rng,re>::pack(it,std::forward<Rng>(rng));
		}
	}

	template< tc::range_return_value re, typename Rng, typename T >
	typename tc::range_return<Rng,re>::type binary_find_first(Rng&& rng, T const& t) {
		return tc::binary_find_first<re>( std::forward<Rng>(rng), t, tc::fn_less() );
	}

	// would be cleaner to search on the distance metric (starting with lower_bound(rng,0)),
	// but subtraction may cause unnecessary overflows
	template< typename Rng, typename T >
	typename boost::range_iterator< std::remove_reference_t<Rng> >::type binary_closest(Rng&& rng, T const& t) {
		auto it = tc::lower_bound<tc::return_iterator>(rng, t);
		if( boost::begin(rng)==it ) {
			return it;
		} else if( boost::end(rng)==it ) {
			return boost::prior(it);
		} else {
			auto itPrior = boost::prior(it);
			return (t - *itPrior) < (*it - t) ? itPrior : it;
		}
	}

	template< typename RngA, typename RngB, typename Comp, typename FuncElementA, typename FuncElementB, typename FuncElementBoth > 
	break_or_continue interleave_may_remove_current(RngA&& rngA, RngB&& rngB, Comp comp, FuncElementA fnElementA, FuncElementB fnElementB,  FuncElementBoth fnElementBoth) {
		auto itA=boost::begin(rngA);
		auto itEndA=boost::end(rngA);
		auto itB=boost::begin(rngB);
		auto itEndB=boost::end(rngB);
		if( itA==itEndA ) goto endA;
		if( itB==itEndB ) goto endB;
		for(;;) {
			switch_no_default( boost::implicit_cast<tc::order>(comp( *itA, *itB )) ) { // make sure comp returns tc::order
			case tc::order::less:
				RETURN_IF_BREAK( tc::continue_if_not_break(fnElementA, *(itA++)));
				if( itA==itEndA ) goto endA;
				break;
			case tc::order::equal:
				RETURN_IF_BREAK( tc::continue_if_not_break(fnElementBoth, *(itA++), *(itB++)));
				if( itA==itEndA ) goto endA;
				if( itB==itEndB ) goto endB;
				break;
			case tc::order::greater:
				RETURN_IF_BREAK( tc::continue_if_not_break(fnElementB, *(itB++)));
				if( itB==itEndB ) goto endB;
				break;
			}
		}
	endB:
		return tc::for_each_may_remove_current(tc::drop(rngA,itA), fnElementA);
	endA:
		return tc::for_each_may_remove_current(tc::drop(rngB,itB), fnElementB);
	}

	namespace interleave_impl {
		// Workaround until [](auto&&) lamdas are available
		template<typename ItB, typename Comp, typename FnElementA, typename FnElementB, typename FnElementBoth>
		struct FInterleaveRngA {
			ItB& m_itB;
			ItB const& m_itEndB;
			Comp& m_comp;
			FnElementA& m_fnElementA;
			FnElementB& m_fnElementB;
			FnElementBoth& m_fnElementBoth;

			FInterleaveRngA(
				ItB& itB,
				ItB const& itEndB,
				Comp& comp,
				FnElementA& fnElementA,
				FnElementB& fnElementB,
				FnElementBoth& fnElementBoth
				) :
				m_itB(itB),
				m_itEndB(itEndB),
				m_comp(comp),
				m_fnElementA(fnElementA),
				m_fnElementB(fnElementB),
				m_fnElementBoth(fnElementBoth)
			{}

			template<typename TypeA>
			tc::break_or_continue operator()(TypeA&& refA) {
				for (;;) {
					tc::order order;
					if( m_itB == m_itEndB || (order=m_comp( refA, *m_itB ))<tc::order::equal ) {
						return tc::continue_if_not_break(m_fnElementA, refA);
					} else if( tc::order::equal<VERIFYINITIALIZED(order) ) {
						RETURN_IF_BREAK( tc::continue_if_not_break(m_fnElementB, *m_itB));
						++m_itB;
					} else {
						return tc::continue_if_not_break(m_fnElementBoth, refA, *m_itB++);
					}
				}
			}
		};
	}

	template< typename RngA, typename RngB, typename Comp, typename FuncElementA, typename FuncElementB, typename FuncElementBoth >
	tc::break_or_continue interleave(RngA&& rngA, RngB&& rngB, Comp comp, FuncElementA fnElementA, FuncElementB fnElementB,  FuncElementBoth fnElementBoth) {
		auto itB=boost::begin(rngB);
		auto itEndB=boost::end(rngB);

		if (tc::continue_ == tc::for_each(
			rngA,
			interleave_impl::FInterleaveRngA<decltype(itB),Comp,FuncElementA,FuncElementB,FuncElementBoth>(itB, itEndB, comp, fnElementA, fnElementB, fnElementBoth)
			) ) {
			while (itB != itEndB) {
				RETURN_IF_BREAK( tc::continue_if_not_break(fnElementB, *itB));
				++itB;
			}
			return tc::continue_;
		} else {
			return tc::break_;
		}
	}
}
