#pragma once

#include "range_defines.h"
#include "container_traits.h"
#include "size.h"
#include "equal.h"
#include "quantifier.h"
#include "is_ordered.h"
#include "partition_iterator.h"
#include "partition_range.h"

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
		return boost::empty(rng) 
			|| all_of( 
				make_iterator_range(boost::next(boost::begin(rng)), boost::end(rng)), 
				boost::bind<bool>(boost::is_equal(), *boost::begin(rng), _1) 
			);
	}

	namespace find_unique_detail {
		template< typename It, typename T >
		It find_unique( It itBegin, It itEnd, T const& t ) {
			It it=std::find( itBegin, itEnd, t );
			_ASSERT( it==itEnd || !contains( boost::make_iterator_range( boost::next( it ), itEnd ), t ) );
			return it;
		}

		template< typename It, typename Pred >
		It find_unique_if( It itBegin, It itEnd, Pred pred ) {
			It it=std::find_if( itBegin, itEnd, pred );
			_ASSERT( it==itEnd || std::none_of( boost::next(it), itEnd,
				[&pred](typename boost::iterator_value<It>::type const& t) {
					return pred(t); // must not modify t inside ASSERT
				}
			) );
			return it;
		}
	}

	template< range_return_value re, typename Rng, typename T >
	typename range_return<Rng,re>::type find_unique( Rng && rng, T && t ) {
		return range_return<Rng,re>::pack(find_unique_detail::find_unique( boost::begin(rng), boost::end(rng), std::forward<T>(t) ), std::forward<Rng>(rng));
	}

	template< range_return_value re, typename Rng, typename Pred >
	typename range_return<Rng,re>::type find_unique_if( Rng && rng, Pred&& pred ) {
		return range_return<Rng,re>::pack(find_unique_detail::find_unique_if( boost::begin(rng), boost::end(rng), std::forward<Pred>(pred) ),std::forward<Rng>(rng));
	}

	template< range_return_value re, typename Rng, typename T >
	typename range_return<Rng,re>::type find_first( Rng && rng, T && t ) {
		return range_return<Rng,re>::pack(std::find( boost::begin(rng), boost::end(rng), std::forward<T>(t) ),std::forward<Rng>(rng));
	};

	template< range_return_value re, typename Rng, typename Pred >
	typename range_return<Rng,re>::type find_first_if( Rng && rng, Pred&& pred ) {
		return range_return<Rng,re>::pack(std::find_if( boost::begin(rng), boost::end(rng), std::forward<Pred>(pred) ),std::forward<Rng>(rng));
	};

	template< range_return_value re, typename Rng, typename Pred >
	typename range_return<Rng,re>::type find_last_if(Rng && rng, Pred pred) {
		auto itBegin=boost::begin(rng);
		for( auto it=boost::end(rng); it!=itBegin; ) {
			--it;
			if( pred(*it) ) return range_return<Rng,re>::pack(it,std::forward<Rng>(rng));
		}
		return range_return<Rng,re>::pack_end(std::forward<Rng>(rng));
	}

	template< typename Rng, typename Pred >
	typename range_return<Rng,return_tail>::type trim_left_if(Rng && rng, Pred && pred) {
		return find_first_if<return_tail>( std::forward<Rng>(rng), !boost::bind<bool>( std::forward<Pred>(pred), _1) );
	}

	template< typename Rng, typename Pred >
	typename range_return<Rng,return_head_next>::type trim_right_if(Rng && rng, Pred && pred) {
		return find_last_if<return_head_next>( std::forward<Rng>(rng), !boost::bind<bool>( std::forward<Pred>(pred), _1) );
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

	template< typename T, std::size_t N, typename Rng >
	void cont_assign(T (&at)[N], Rng && rng) {
		std::size_t i=0;
		tc::for_each( std::forward<Rng>(rng), [&]( typename boost::range_reference< typename std::remove_reference<Rng>::type >::type t ) {
			_ASSERT( i<N );
			at[i]=tc_move_if_owned(t);
			++i;
		} );
		_ASSERTEQUAL( i, N );
	}

	template< typename Cont >
	Cont& head_inplace( Cont & cont, typename boost::range_iterator< typename std::remove_reference<Cont>::type >::type it ) {
		cont.erase( it, cont.end() );
		return cont;
	}

	template< typename Cont >
	Cont& head_inplace( Cont& cont, typename boost::range_size< typename std::remove_reference<Cont>::type >::type n ) {
		auto it=cont.begin();
		_ASSERT( n <= boost::implicit_cast< typename boost::range_size< typename std::remove_reference<Cont>::type >::type >(boost::distance(cont)) );
		std::advance( it, n );
		tc::head_inplace( cont, it );
		return cont;
	}

	template< typename Cont >
	Cont& truncate_inplace( Cont& cont, typename boost::range_size< typename std::remove_reference<Cont>::type >::type n ) {
		auto it=cont.begin();
		advance_forward_bounded( it, n, cont.end() );
		tc::head_inplace( cont, it );
		return cont;
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
	typename Cont::size_type cont_extended_memory( Cont const& cont ) {
		// factor*cont.size() does not suffice for memory operation guarantee
		// 64 bit is enough to hold any memory money can buy
		return static_cast< typename Cont::size_type >(
			static_cast<boost::uint64_t>(cont.capacity())*8/5
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

	#define PART1() \
		template< typename Cont, 
	#define PART2() \
		> typename Cont::iterator cont_must_emplace(Cont& cont,
	#define PART3() ) { \
			return verify_inserted( cont.emplace(
	#define PART4() ) ); \
		}
	PERFECT_FORWARD
	#undef PART1
	#undef PART2
	#undef PART3
	#undef PART4

	#define PART1() \
		template< typename Cont, 
	#define PART2() \
		> std::pair< typename Cont::iterator, bool > cont_try_emplace(Cont& cont,
	#define PART3() ) { \
			return cont.emplace(
	#define PART4() ); \
		}
	PERFECT_FORWARD
	#undef PART1
	#undef PART2
	#undef PART3
	#undef PART4

	#define PART1() \
		template< typename Cont, 
	#define PART2() \
		> typename Cont::iterator cont_must_emplace_before(Cont& cont, typename Cont::const_iterator itHint,
	#define PART3() ) { \
			auto c=cont.size(); \
			typename Cont::iterator it=cont.emplace_hint(itHint,
	#define PART4() ); \
			_ASSERTEQUAL( cont.size(), c+1 ); \
			_ASSERT( boost::next(it)==itHint ); \
			return it; \
		}
	PERFECT_FORWARD
	#undef PART1
	#undef PART2
	#undef PART3
	#undef PART4

	template< typename Cont >
	void cont_must_erase( Cont& cont, typename Cont::key_type const& data ) {
		VERIFYEQUAL( cont.erase(data), 1 );
	}

	template<typename Cont, typename Enable=void>
	class range_filter;

	template<typename Cont>
	class range_filter<Cont, typename boost::enable_if_c< 
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
			tc::head_inplace( m_cont, m_itOutputEnd );
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
	class range_filter< Cont, typename boost::enable_if_c< 
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

	private:
#ifdef _CHECKS
		iterator m_itFirstValid;
#endif

	protected:
		Cont& m_cont;
		typename boost::range_size<Cont>::type m_cOutput;

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
			tc::head_inplace( m_cont, m_cOutput );
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
	typename boost::enable_if_c< has_mem_fn_sort< typename remove_cvref<Rng>::type >::value,
	Rng && >::type sort(Rng && rng, Pred&& pred) {
		rng.sort( std::forward<Pred>(pred) );
		return std::forward<Rng>(rng);
	}
	template<typename Rng, typename Pred>
	typename boost::disable_if_c< has_mem_fn_sort< typename remove_cvref<Rng>::type >::value, 
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
	typename boost::enable_if_c< has_mem_fn_reverse< typename remove_cvref<Rng>::type >::value, Rng & >::type reverse_inplace(Rng & rng) {
		rng.reverse();
		return rng;
	}
	template<typename Rng>
	typename boost::disable_if_c< has_mem_fn_reverse< typename remove_cvref<Rng>::type >::value, Rng & >::type reverse_inplace(Rng & rng) {
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
		return tc::unique_erase( std::forward<Cont>(cont), fn_equal() );
	}

	template<typename Cont, typename Less>
	Cont && ordered_unique_erase( Cont && cont, Less less ) {
		_ASSERTDEBUG( boost::range::is_sorted( cont, less ) );
		return tc::unique_erase( std::forward<Cont>(cont), !boost::bind<bool>( tc_move(less), _1, _2 ) );
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
}