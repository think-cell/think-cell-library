#pragma once

#include "range_defines.h"
#include "container_traits.h"
#include "size.h"
#include "equal.h"
#include "quantifier.h"
#include "is_ordered.h"
#include "partition_iterator.h"
#include "partition_range.h"
#include "empty.h"

#include "Library/ErrorReporting/functors.h"
#include "Library/Utilities/inherit_ctors.h"
#include "Library/Utilities/static_vector.h"

#include <boost/algorithm/string/compare.hpp>
#include <boost/range/algorithm/sort.hpp>
#include <boost/range/algorithm/reverse.hpp>
#include <boost/range/algorithm_ext/is_sorted.hpp>
#include <boost/range/algorithm/adjacent_find.hpp>
#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/utility.hpp>
#include <boost/implicit_cast.hpp>

#include <type_traits>
#include <vector>
#include <set>

namespace RANGE_PROPOSAL_NAMESPACE {
	template< typename Rng, typename Less >
	bool is_strictly_sorted( Rng const& rng, Less && less ) {
		return boost::end(rng)==boost::adjacent_find( rng, !boost::bind<bool>( std::forward<Less>(less), _1, _2 ) );
	}
	template< typename Rng >
	bool is_strictly_sorted( Rng const& rng ) {
		return is_strictly_sorted( rng, boost::is_less() );
	}

	template< typename Rng >
	bool all_same( Rng const& rng ) {
		return RANGE_PROPOSAL_NAMESPACE::empty(rng)
			|| all_of(
				make_iterator_range(boost::next(boost::begin(rng)), boost::end(rng)),
				boost::bind<bool>(boost::is_equal(), *boost::begin(rng), _1)
			);
	}

	template< range_return_value re, typename Rng, typename Pred >
	typename range_return<Rng,re>::type find_unique_if( Rng && rng, Pred pred ) {
		auto const itEnd=boost::end(rng);
		for( auto it=boost::begin(rng); it!=itEnd; ++it ) {
			if( pred(*it) ) {
				_ASSERT( std::none_of( boost::next(it), itEnd, pred ) );
				return range_return<Rng,re>::pack(it,std::forward<Rng>(rng));
			}
		}
		return range_return<Rng,re>::pack_singleton(std::forward<Rng>(rng));
	};

	template< range_return_value re, typename Rng, typename Pred >
	typename range_return<Rng,re>::type find_first_if( Rng && rng, Pred pred ) {
		auto const itEnd=boost::end(rng);
		for( auto it=boost::begin(rng); it!=itEnd; ++it ) {
			if( pred(*it) ) {
				return range_return<Rng,re>::pack(it,std::forward<Rng>(rng));
			}
		}
		return range_return<Rng,re>::pack_singleton(std::forward<Rng>(rng));
	};

	template< range_return_value re, typename Rng, typename Pred >
	typename range_return<Rng,re>::type find_last_if(Rng && rng, Pred pred) {
		auto itBegin=boost::begin(rng);
		for( auto it=boost::end(rng); it!=itBegin; ) {
			--it;
			if( pred(*it) ) return range_return<Rng,re>::pack(it,std::forward<Rng>(rng));
		}
		return range_return<Rng,re>::pack_singleton(std::forward<Rng>(rng));
	}

	template< range_return_value re, typename Rng, typename T >
	typename range_return<Rng,re>::type find_unique( Rng && rng, T const& t ) {
		return find_unique_if<re>( std::forward<Rng>(rng), boost::bind<bool>( fn_equal_to(), _1, boost::cref(t) ) );
	};

	template< range_return_value re, typename Rng, typename T >
	typename range_return<Rng,re>::type find_first( Rng && rng, T const& t ) {
		return find_first_if<re>( std::forward<Rng>(rng), boost::bind<bool>( fn_equal_to(), _1, boost::cref(t) ) );
	};

	template< range_return_value re, typename Rng, typename T >
	typename range_return<Rng,re>::type find_last( Rng && rng, T const& t ) {
		return find_last_if<re>( std::forward<Rng>(rng), boost::bind<bool>( fn_equal_to(), _1, boost::cref(t) ) );
	};

	template< typename Rng, typename Pred >
	typename range_return<Rng,return_tail_or_empty>::type trim_left_if(Rng && rng, Pred && pred) {
		return find_first_if<return_tail_or_empty>( std::forward<Rng>(rng), !boost::bind<bool>( std::forward<Pred>(pred), _1) );
	}

	template< typename Rng, typename Pred >
	typename range_return<Rng,return_head_next_or_empty>::type trim_right_if(Rng && rng, Pred && pred) {
		return find_last_if<return_head_next_or_empty>( std::forward<Rng>(rng), !boost::bind<bool>( std::forward<Pred>(pred), _1) );
	}

	template< typename Rng >
	auto distance_bounded( Rng const& rng, typename boost::range_size<Rng>::type nBound ) return_ctor(
		size_proxy< typename boost::range_size<Rng>::type >, ( advance_forward_bounded( boost::begin(rng), nBound, boost::end(rng) ) )
	)

	template< typename Rng, typename T >
	bool contains_single( Rng const& rng, T const& t ) {
		return 1==distance_bounded(rng,2) && *boost::begin( rng )==t;
	};

	////////////////////////
	// generic container algorithms

	template< typename Cont, typename Rng >
	typename std::enable_if<
		has_mem_fn_assign< typename remove_cvref<Cont>::type
	>::value, Cont&& >::type cont_assign(Cont&& cont, Rng && rng) {
		cont.assign(boost::begin(rng), boost::end(rng));
		return std::forward<Cont>(cont);
	}

	template< typename T, typename P, typename A, typename Rng >
	std::set<T,P,A>& cont_assign(std::set<T,P,A>& cont, Rng && rng) {
		cont_clear(cont);
		cont_must_insert_range(cont, std::forward<Rng>(rng));
		return cont;
	}

	namespace cont_assign_impl {
		template< typename T, std::size_t N >
		struct assign {
			assign(T (&at)[N])
			: m_at(at), m_i(0) {}
			
			template< typename Rhs >
			void operator()( Rhs && rhs ) {
				m_at[m_i]=std::forward<Rhs>(rhs);
				++m_i;
			}

			~assign(){
				_ASSERTEQUAL( m_i, N );
			}
		private:
			T (&m_at)[N];
			std::size_t m_i;
		};
	}

	template< typename T, std::size_t N, typename Rng >
	void cont_assign(T (&at)[N], Rng && rng) {
		for_each( std::forward<Rng>(rng), std::ref(make_lvalue(cont_assign_impl::assign<T,N>(at))) );
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
	typename boost::range_size< Cont >::type cont_extended_memory( Cont const& cont ) {
		// factor*cont.size() does not suffice for memory operation guarantee
		// 64 bit is enough to hold any memory money can buy
		return static_cast< typename Cont::size_type >(
			static_cast<std::uint64_t>(cont.capacity())*8/5
		);
	}

	template< typename Cont >
	Cont&& cont_resize( Cont&& cont, typename boost::range_size< typename std::remove_reference<Cont>::type >::type n ) {
		if( cont.capacity()<n ) {
			cont.reserve(std::max(cont_extended_memory(cont),n));
		}
		cont.resize(n);
		return std::forward<Cont>(cont);
	}

	template< typename Cont >
	Cont&& cont_clear( Cont&& cont ) {
		cont.erase(cont.begin(),cont.end());
		return std::forward<Cont>(cont);
	}

	template< typename Cont >
	Cont&& cont_clear( Cont&& cont, typename boost::range_size< typename std::remove_reference<Cont>::type >::type n ) {
		cont_clear( cont );
		cont_resize( cont, n );
		return std::forward<Cont>(cont);
	}

	template< typename Cont >
	Cont&& cont_extend( Cont&& cont, typename boost::range_size< typename std::remove_reference<Cont>::type >::type n ) {
		_ASSERT( cont.size()<=n );
		cont_resize( cont, n );
		return std::forward<Cont>(cont);
	}

	template< typename Cont >
	typename boost::range_reference< typename std::remove_reference<Cont>::type >::type cont_extend_at( Cont& cont, typename boost::range_size< typename std::remove_reference<Cont>::type >::type n ) {
		if( cont.size()<=n ) {
			cont_resize( cont, n+1 );
		}
		return cont[n];
	}

	template< range_return_value re, typename Cont, typename Rng >
	auto cont_append(Cont& cont, Rng&& rng) -> typename range_return<Cont&,re>::type {
		return range_return<Cont&,re>::pack(
			cont.insert(boost::end(cont), boost::begin(rng), boost::end(rng)),
			cont
		);
	}

	/////////////////////////////////
	// associative containers

	// std::set/map returns pair with bool=inserted?
	template< typename It >
	It && verify_inserted( std::pair<It,bool> && pairitb ) {
		_ASSERT(pairitb.second);
		return tc_move(pairitb.first);
	};

	// std::multiset/multimap always inserts and thus returns only iterator
	template< typename It >
	It && verify_inserted( It && it ) {
		return std::forward<It>(it);
	};

	template< typename Cont, typename TValue > // use extra template parameter instead of Cont::value_type to have both move and copy semantics
	typename Cont::iterator cont_must_insert(Cont& cont, TValue&& val ) {
		return verify_inserted(cont.insert(std::forward<TValue>(val)));
	}

	template< typename Cont, typename Rng >
	Cont&& cont_try_insert_range(Cont&& cont, Rng && rng ) {
		cont.insert( boost::begin(rng),boost::end(rng) );
		return std::forward<Cont>(cont);
	}

	template< typename Cont, typename Rng >
	Cont&& cont_must_insert_range(Cont&& cont, Rng && rng ) {
#ifdef _CHECKS
		auto c=cont.size();
		auto r=boost::distance(rng);
#endif
		cont_try_insert_range( cont, std::forward<Rng>(rng) );
		_ASSERTEQUAL( cont.size(), c+r );
		return std::forward<Cont>(cont);
	}

	template< typename Cont, typename... Args>
	typename Cont::iterator cont_must_emplace(Cont& cont,Args&&... args) {
		return verify_inserted( cont.emplace(std::forward<Args>(args)...) );
	}

	template< typename Cont, typename... Args>
	std::pair< typename Cont::iterator, bool > cont_try_emplace(Cont& cont,Args&&... args) {
		return cont.emplace(std::forward<Args>(args)...);
	}

	template< typename Cont, typename... Args>
	typename Cont::iterator cont_must_emplace_before(Cont& cont, typename Cont::const_iterator itHint,Args&&... args) {
		auto c=cont.size();
		typename Cont::iterator it=cont.emplace_hint(itHint,std::forward<Args>(args)...);
		_ASSERTEQUAL( cont.size(), c+1 );
		_ASSERT( boost::next(it)==itHint );
		return it;
	}

	template< typename Cont >
	void cont_must_erase( Cont& cont, typename Cont::key_type const& data ) {
		VERIFYEQUAL( cont.erase(data), 1u );
	}

	template< typename Cont >
	void cont_try_erase( Cont& cont, typename Cont::key_type const& data ) {
		VERIFY( cont.erase(data)<=1 );
	}

	template<typename Cont, typename Enable=void>
	class range_filter;

	template<typename Cont>
	class range_filter<Cont, typename std::enable_if< 
		has_efficient_erase< typename remove_cvref<Cont>::type >::value
		|| has_mem_fn_lower_bound< typename remove_cvref<Cont>::type >::value
	>::type >: boost::noncopyable {
	public:
		typedef typename boost::range_iterator<Cont>::type iterator;
		typedef typename boost::range_iterator<const Cont>::type const_iterator;

	private:
		Cont& m_cont;
		iterator m_itOutputEnd;

	public:
		explicit range_filter( Cont& cont )
			: m_cont(cont)
			, m_itOutputEnd(cont.begin())
		{}

		range_filter( Cont& cont, iterator const& itStart )
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

		///////////////////////////////////
		// range interface for output range

		const_iterator cbegin() const {
			return m_cont.begin();
		}

		const_iterator cend() const {
			return m_itOutputEnd;
		}

		iterator begin() {
			return m_cont.begin();
		}

		iterator end() {
			return m_itOutputEnd;
		}

		const_iterator begin() const {
			return cbegin();
		}

		const_iterator end() const {
			return cend();
		}

		void pop_back() {
			_ASSERT( m_itOutputEnd!=boost::begin(m_cont) );
			--m_itOutputEnd;
			m_itOutputEnd=m_cont.erase(m_itOutputEnd);
		}
	};

	template<typename Cont>
	class range_filter< Cont, typename std::enable_if< 
		has_mem_fn_splice< typename remove_cvref<Cont>::type >::value
	>::type >: boost::noncopyable {
		Cont& m_contInput;
		Cont m_contOutput;
	public:
		typedef typename Cont::iterator iterator;
		typedef typename Cont::const_iterator const_iterator;

		explicit range_filter( Cont& cont )
			: m_contInput(cont)
		{}

		range_filter( Cont& cont, iterator const& itStart )
			: m_contInput(cont)
		{
			m_contOutput.splice( m_contOutput.end(), m_contInput, m_contInput.begin(), itStart );
		}

		~range_filter() {
			m_contInput=tc_move_always( m_contOutput );
		}

		void keep(iterator it) {
			_ASSERT( it!=m_contInput.end() );
			m_contOutput.splice( 
				m_contOutput.end(),
				m_contInput,
				m_contInput.erase( m_contInput.begin(), it )
			);
		}

		///////////////////////////////////
		// range interface for output range

		const_iterator cbegin() const {
			return m_contOutput.cbegin();
		}

		const_iterator cend() const {
			return m_contOutput.cend();
		}

		iterator begin() {
			return m_contOutput.begin();
		}

		iterator end() {
			return m_contOutput.end();
		}

		const_iterator begin() const {
			return m_contOutput.begin();
		}

		const_iterator end() const {
			return m_contOutput.end();
		}

		void pop_back() {
			m_contOutput.pop_back();
		}
	};

	template<typename Cont>
	class range_filter_vector_string: boost::noncopyable {
	public:
		typedef typename boost::range_iterator<Cont>::type iterator;
		typedef typename boost::range_iterator<const Cont>::type const_iterator;

	protected:
		Cont& m_cont;
		typename boost::range_size<Cont>::type m_cOutput;

	private:
#ifdef _CHECKS
		iterator m_itFirstValid;
#endif

	public:
		explicit range_filter_vector_string( Cont& cont )
			: m_cont(cont)
			, m_cOutput(0)
#ifdef _CHECKS
			, m_itFirstValid(cont.begin())
#endif
		{}

		range_filter_vector_string( Cont& cont, iterator const& itStart )
			: m_cont(cont)
			, m_cOutput(itStart-cont.begin())
#ifdef _CHECKS
			, m_itFirstValid(itStart)
#endif
		{}

		~range_filter_vector_string() {
			tc::take_inplace( m_cont, m_cOutput );
		}

		void keep(iterator it) {
#ifdef _CHECKS
			_ASSERT( 0<=std::distance(m_itFirstValid,it) );
			m_itFirstValid=it;
			++m_itFirstValid;
#endif
			m_cont[m_cOutput]=tc_move_always(*it);
			++m_cOutput;
		}

		///////////////////////////////////
		// range interface for output range

		const_iterator cbegin() const {
			return m_cont.cbegin();
		}

		const_iterator cend() const {
			return m_cont.cbegin()+m_cOutput;
		}

		iterator begin() {
			return m_cont.begin();
		}

		iterator end() {
			return m_cont.begin()+m_cOutput;
		}

		const_iterator begin() const {
			return cbegin();
		}

		const_iterator end() const {
			return cend();
		}

		void pop_back() {
			_ASSERT( 0<m_cOutput );
			--m_cOutput;
		}
	};

	template<typename T, typename Alloc>
	class range_filter< std::vector<T,Alloc>, void >: public range_filter_vector_string< std::vector<T,Alloc> > {
		typedef range_filter_vector_string< std::vector<T,Alloc> > range_filter_vector_string;
	public:
		INHERIT_CTORS( range_filter, range_filter_vector_string );
	};

	template<typename T, typename Alloc>
	class range_filter< std::basic_string<T,Alloc>, void >: public range_filter_vector_string< std::basic_string<T,Alloc> > {
		typedef range_filter_vector_string< std::basic_string<T,Alloc> > range_filter_vector_string;
	public:
		INHERIT_CTORS( range_filter, range_filter_vector_string );
	};

	template<typename T, unsigned int N>
	class range_filter< static_vector<T,N>, void >: public range_filter_vector_string< static_vector<T,N> > {
		typedef range_filter_vector_string< static_vector<T,N> > range_filter_vector_string;
	public:
		INHERIT_CTORS( range_filter, range_filter_vector_string );
	};


	/////////////////////////////////////////////////////
	// remove_erase

	template<typename Cont, typename Pred>
	Cont & filter_inplace(Cont & cont, typename boost::range_iterator< typename std::remove_reference<Cont>::type >::type it, Pred pred) {
		{ range_filter< typename remove_cvref<Cont>::type > rngfilter(cont,it);
			auto const itEnd=boost::end(cont);
			while( it!=itEnd ) {
				if( pred(*it)) {
					rngfilter.keep(it++); // may invalidate it, so move away first
				} else {
					++it;
				}
			}
		}
		return cont;
	}

	template<typename Cont, typename Pred>
	Cont & filter_inplace(Cont & cont, Pred && pred) {
		return tc::filter_inplace( cont, boost::begin(cont), std::forward<Pred>(pred) );
	}

	// cannot use list::remove because T may not be list::value_type
	// cannot use key-based lookup for set/map because T may not be Cont::value_type and !Cont::predicate()(a,b) && !Cont::predicate()(b,a) may not be the same as ==
	template<typename Cont, typename T>
	Cont& remove_erase( Cont& cont, T const& t ) {
		return tc::filter_inplace( cont, !boost::bind<bool>( boost::is_equal(), _1, boost::cref(t) ) );
	}

	/////////////////////////////////////////////////////
	// remove_count_erase

	template<typename Cont, typename Pred>
	typename boost::range_size<Cont>::type remove_count_erase_if( Cont& cont, Pred pred ) {
		typename boost::range_size<Cont>::type count=0;
		filter_inplace( cont, [&]( typename boost::range_reference<Cont>::type t )->bool {
			bool const b=pred(tc_move_if_owned(t));
			count+=boost::implicit_cast<
#ifndef _MSC_VER
				typename
#endif
				boost::range_size<Cont>::type
			>(b);
			return !b;
		} );
		return count;
	}

	template<typename Cont, typename T>
	typename boost::range_size<Cont>::type remove_count_erase( Cont& cont, T const& t ) {
		return remove_count_erase_if( cont, boost::bind<bool>( boost::is_equal(), _1, boost::cref(t) ) );
	}

	/////////////////////////////////////////////////////
	// sort

	template<typename Rng, typename Pred>
	typename std::enable_if< has_mem_fn_sort< typename remove_cvref<Rng>::type >::value,
	Rng && >::type sort(Rng && rng, Pred&& pred) {
		rng.sort( std::forward<Pred>(pred) );
		return std::forward<Rng>(rng);
	}
	template<typename Rng, typename Pred>
	typename std::enable_if<!has_mem_fn_sort< typename remove_cvref<Rng>::type >::value, 
	Rng && >::type sort(Rng && rng, Pred&& pred) {
		boost::sort( rng, std::forward<Pred>(pred) );
		return std::forward<Rng>(rng);
	}
	template<typename Rng>
	Rng && sort(Rng && rng) {
		return tc::sort( std::forward<Rng>(rng), boost::is_less() );
	}

	/////////////////////////////////////////////////////
	// reverse_inplace

	template<typename Rng>
	typename std::enable_if< has_mem_fn_reverse< typename remove_cvref<Rng>::type >::value, Rng & >::type reverse_inplace(Rng & rng) {
		rng.reverse();
		return rng;
	}
	template<typename Rng>
	typename std::enable_if<!has_mem_fn_reverse< typename remove_cvref<Rng>::type >::value, Rng & >::type reverse_inplace(Rng & rng) {
		boost::reverse( rng );
		return rng;
	}

	///////////////////////////////////////
	// partition ranges into subranges

	// comparison with first in range is like std::unique
	template<typename Rng, typename Equals, typename Func>
	break_or_continue for_each_unique_range_front( Rng && rng, Equals pred, Func func ) {
		auto itLeft = boost::begin(rng);
		if (itLeft!=boost::end(rng)) {
			auto itRight=boost::next(itLeft);
			for( ; itRight!=boost::end(rng) ; ++itRight) {
				if(!pred(*itLeft, *itRight)) {
					if( break_==continue_if_void( func, slice(rng, itLeft, itRight) ) ) return break_;
					itLeft=itRight;
				}
			}
			return tc::continue_if_void( func, slice(rng, itLeft, itRight) );
		} else {
			return continue_;
		}
	}

	// comparison with last in range
	template<typename Rng, typename Equals, typename Func>
	break_or_continue for_each_unique_range_adjacent( Rng && rng, Equals pred, Func func) {
		typedef typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type TIt;

		TIt itLeft=boost::begin(rng);
		if (itLeft!=boost::end(rng)) {
			TIt itPrior=itLeft;
			TIt itRight=boost::next(itPrior);
			for ( ; itRight!=boost::end(rng); ++itRight) {
				if (!pred(*itPrior, *itRight)) {
					if( break_==tc::continue_if_void( func, slice(rng, itLeft, itRight) ) ) return break_;
					itLeft=itRight;
				}
				itPrior=itRight;
			}
			return tc::continue_if_void( func, slice(rng, itLeft, itRight) );
		} else {
			return continue_;
		}
	}

	template<typename Rng, typename Less, typename Func>
	break_or_continue ordered_for_each_unique_range( Rng && rng, Less less, Func&& func ) {
		_ASSERTDEBUG( boost::range::is_sorted( rng, less ) );
		return tc::for_each_unique_range_front( std::forward<Rng>(rng), !boost::bind<bool>( tc_move(less), _1, _2 ), std::forward<Func>(func) );
	}

	template<typename Rng, typename Func>
	break_or_continue ordered_for_each_unique_range( Rng && rng, Func&& func ) {
		return tc::ordered_for_each_unique_range( std::forward<Rng>(rng), fn_less(), std::forward<Func>(func) );
	}

	template<typename Rng, typename Less, typename Func>
	break_or_continue sort_for_each_unique_range( Rng && rng, Less less, Func&& func ) {
		tc::sort( rng, less );
		return tc::ordered_for_each_unique_range( std::forward<Rng>(rng), tc_move(less), std::forward<Func>(func) );
	}

	template<typename Rng, typename Func>
	break_or_continue sort_for_each_unique_range( Rng && rng, Func&& func ) {
		return sort_for_each_unique_range( std::forward<Rng>(rng), fn_less(), std::forward<Func>(func) );
	}

	template< typename Rng, typename Pred, typename Func >
	break_or_continue for_each_subrange_where( Rng && rng, Pred pred, Func func ) {
		typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type it=boost::begin(rng);
		typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type itEnd=boost::end(rng);
		for(;;) {
			for(;;) {
				if( it==itEnd ) return continue_;
				if( pred(*it) ) break;
				++it;
			}
			typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type itStart=it;
			do {
				++it;
				if( it==itEnd ) {
					return continue_if_void( func, slice( rng, itStart, it ) ); // may throw
				};
			} while( pred(*it) );
			if( break_==continue_if_void( func, slice( rng, itStart, it ) ) ) return break_; // may throw
			++it;
		}
	};

	DEFINE_FN( for_each_subrange_where );

	template< typename Cont, typename Less, typename Accu >
	Cont && sort_accumulate_each_unique_range(Cont && cont, Less less, Accu accu) {
		tc::sort( cont, less );
		{ range_filter< typename remove_cvref<Cont>::type > rngfilter( cont );
			tc::ordered_for_each_unique_range(
				cont,
				tc_move(less),
				[&accu,&rngfilter]( typename make_sub_range_result< typename std::add_lvalue_reference<Cont>::type >::type rngEqualSubRange ) {
					for(
						auto it=boost::next(boost::begin(rngEqualSubRange));
						it!=boost::end(rngEqualSubRange);
						++it
					) {
						accu( *boost::begin(rngEqualSubRange), *it );
					}
					rngfilter.keep( boost::begin(rngEqualSubRange) );
				}
			);
		}
		return std::forward<Cont>(cont);
	}

	template< typename Cont, typename Equals >
	Cont&& unique_erase( Cont && cont, Equals && pred ) {
		{ tc::range_filter< typename remove_cvref<Cont>::type > rngfilter(cont);
			tc::for_each_unique_range_front( cont, std::forward<Equals>(pred),
				[&]( typename make_sub_range_result< typename std::add_lvalue_reference<Cont>::type >::type rng ) {
					rngfilter.keep(boost::begin(rng));
				}
			);
		}
		return std::forward<Cont>(cont);
	}

	template< typename Cont >
	Cont && unique_erase(Cont && cont) {
		return tc::unique_erase( std::forward<Cont>(cont), fn_equal_to() );
	}

	template<typename Cont, typename Less>
	Cont && ordered_unique_erase( Cont && cont, Less less ) {
		_ASSERTDEBUG( boost::range::is_sorted( cont, less ) );
		return tc::unique_erase( std::forward<Cont>(cont), !boost::bind<bool>( tc_move(less), _1, _2 ) );
	}

	template<typename Cont>
	Cont && ordered_unique_erase( Cont && cont ) {
		return ordered_unique_erase( std::forward<Cont>(cont), fn_less() );
	}

	template< typename Cont, typename Less >
	Cont && sort_unique_erase(Cont && cont, Less less) {
		tc::sort( cont, less );
		return tc::ordered_unique_erase( std::forward<Cont>(cont), tc_move(less) );
	}

	template< typename Cont >
	Cont && sort_unique_erase(Cont && cont) {
		return tc::sort_unique_erase( std::forward<Cont>(cont), fn_less() );
	}

	DEFINE_FN( sort_unique_erase );

	template< typename Rng >
	auto make_vector( Rng && rng ) return_ctor(
		std::vector< typename boost::range_value< typename std::remove_reference<Rng>::type >::type >, (boost::begin(rng),boost::end(rng))
	)

	template< typename ValueType >
	std::vector< ValueType >&& make_vector(std::vector< ValueType >&& vec) {
		return tc_move( vec );
	}

	// a temporary substitute for make_vector; can be used if make_vector is not able to determine element type of given range
	template< typename ValueType, typename Rng >
	std::vector<ValueType> make_vector_from_generator_range(Rng const& rng) {
		std::vector<ValueType> vec;
		tc::for_each(rng, std::bind( mem_fn_emplace_back(), &vec, std::placeholders::_1 ) );
		return vec;
	}

	template< typename Rng >
	auto make_basic_string( Rng && rng ) return_ctor(
		std::basic_string< typename boost::range_value< typename std::remove_reference<Rng>::type >::type >, (boost::begin(rng),boost::end(rng))
	)

	template< typename CharT >
	std::basic_string< CharT >&& make_basic_string(std::basic_string< CharT >&& str) {
		return tc_move( str );
	}

	template< tc::range_return_value re, typename Rng, typename T, typename Pred >
	typename tc::range_return<Rng,re>::type binary_find_unique( Rng && rng, T const& t, Pred pred ) {
		// The result of tc::binary_find_unique must be unambiguous. In general, this means that rng is strictly
		// ordered. In some cases, it is convenient to allow multiple occurrences of the same item in
		// rng, which is not a problem as long as these items are not searched for.
		_ASSERTDEBUG( boost::range::is_sorted( rng, pred ) );
		auto it=tc::lower_bound<tc::return_iterator_or_end>( rng, t, pred );
		if( it==boost::end( rng ) || pred( t, *it ) ) {
			return tc::range_return<Rng,re>::pack_singleton(std::forward<Rng>(rng));
		} else {
	#ifdef _CHECKS
			typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type itNext = boost::next(it);
			_ASSERT( boost::end(rng)==itNext || pred(t, *itNext) );
	#endif
			return tc::range_return<Rng,re>::pack(it,std::forward<Rng>(rng));
		}
	}

	template< tc::range_return_value re, typename Rng, typename T >
	typename tc::range_return<Rng,re>::type binary_find_unique( Rng && rng, T const& t ) {
		return tc::binary_find_unique<re>( std::forward<Rng>(rng), t, fn_less() );
	}

	// would be cleaner to search on the distance metric (starting with lower_bound(rng,0)),
	// but subtraction may cause unnecessary overflows
	template< typename Rng, typename T >
	typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type binary_closest( Rng && rng, T const& t ) {
		typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type it = tc::lower_bound<tc::return_iterator_or_end>(rng, t);
		if( boost::begin(rng)==it ) {
			return it;
		} else if( boost::end(rng)==it ) {
			return boost::prior(it);
		} else {
			typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type itPrior = boost::prior(it);
			return (t - *itPrior) < (*it - t) ? itPrior : it;
		}
	}
}
