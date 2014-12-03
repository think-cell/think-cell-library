#include "../Range.h"
#include "range.t.h"

#include <vector>
#include <boost/range/adaptors.hpp>

namespace {

//---- Basic ------------------------------------------------------------------------------------------------------------------
UNITTESTDEF( basic ) {
	using namespace RANGE_PROPOSAL_NAMESPACE;

	TEST_init_hack(std::vector, int, v, {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20});

	auto evenvr = tc::filter(v, [](const int& v){ return (v%2==0);});

	TEST_init_hack(std::vector, int, vexp, {2, 4, 6, 8, 10, 12, 14, 16, 18, 20});
	TEST_RANGE_EQUAL(vexp, evenvr);
}

	using namespace RANGE_PROPOSAL_NAMESPACE;

	template<typename Func>
	struct WrapVoidFunc {
		static_assert(
			std::is_reference<Func>::value,
			"type must be a reference type"
		);

		WrapVoidFunc(Func func, break_or_continue& breakorcontinue) :
			m_func(std::move(func)), m_breakorcontinue(breakorcontinue)
		{}

		template<typename Arg>
		typename std::enable_if<
			std::is_same<
				decltype(std::declval<typename std::remove_reference<Func>::type >()(std::declval<Arg>())),
				break_or_continue
			>::value
		>::type
		operator()(Arg&& arg) {
			if (continue_ == m_breakorcontinue) {
				m_breakorcontinue = m_func(std::forward<Arg>(arg));
			}
		}

		template<typename Arg>
		typename std::enable_if<
			!std::is_same<
				decltype(std::declval<typename std::remove_reference<Func>::type >()(std::declval<Arg>())),
				break_or_continue
			>::value
		>::type
		operator()(Arg&& arg) {
			if (continue_ == m_breakorcontinue) {
				m_func(std::forward<Arg>(arg));
			}
		}

		private:
			Func m_func;
			break_or_continue& m_breakorcontinue;
	};

	template<typename Rng>
	struct void_range_struct : public std::remove_reference<Rng>::type {

		using base_ = typename std::remove_reference<Rng>::type;

		template< typename Func >
		typename std::enable_if<
			!std::is_same<
				decltype(std::declval<base_>()(std::declval<Func>())),
				break_or_continue
			>::value,
			break_or_continue
		>::type
		operator()(Func&& func) {
			break_or_continue breakorcontinue = continue_;
			base_::operator()(WrapVoidFunc<Func&&>(std::forward<Func>(func), breakorcontinue));
			return breakorcontinue;
		}

		template< typename Func >
		typename std::enable_if<
			!std::is_same<
				decltype(std::declval<base_>()(std::declval<Func>())),
				break_or_continue
			>::value,
			break_or_continue
		>::type
		operator()(Func&& func) const {
			break_or_continue breakorcontinue = continue_;
			base_::operator()(WrapVoidFunc<Func&&>(std::forward<Func>(func), breakorcontinue));
			return breakorcontinue;
		}

		template< typename Func >
		typename std::enable_if<
			std::is_same<
				decltype(std::declval<base_>()(std::declval<Func>())),
				break_or_continue
			>::value,
			break_or_continue
		>::type
		operator()(Func&& func) {
			return base_::operator()(std::forward<Func>(func));
		}
	
		template< typename Func >
		typename std::enable_if<
			std::is_same<
				decltype(std::declval<base_>()(std::declval<Func>())),
				break_or_continue
			>::value,
			break_or_continue
		>::type
		operator()(Func&& func) const {
			return base_::operator()(std::forward<Func>(func));
		}
	};

	template<typename Rng>
	auto void_range(Rng&& rng) return_decltype_rvalue_by_ref (
		derived_or_base_cast<void_range_struct<Rng&&>>(std::forward<Rng>(rng))
	)


//---- Generator Range --------------------------------------------------------------------------------------------------------
namespace {
	struct generator_range {
		template< typename Func >
		void operator()( Func func ) const {
			for(int i=0;i<50;++i) {
				func(i);
			}
		}
	};
}

UNITTESTDEF( generator_range ) {
   using namespace RANGE_PROPOSAL_NAMESPACE;
   
   TEST_init_hack(std::vector, int, vexp, {0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48});

   TEST_RANGE_EQUAL(vexp, tc::filter( void_range(generator_range()), [](int i){ return i%2==0; } ));
   TEST_RANGE_EQUAL(tc::filter( void_range(generator_range()), [](int i){ return i%2==0; } ), vexp);
}

//---- Generator Range (with break) -------------------------------------------------------------------------------------------
namespace {
	struct generator_range_break {
		template< typename Func >
		RANGE_PROPOSAL_NAMESPACE::break_or_continue operator()( Func func ) {
			using namespace RANGE_PROPOSAL_NAMESPACE;
			for(int i=0;i<5000;++i) {
				if (func(i)==break_) { return break_; }
			}
			return continue_;
		}
	};
}

// TODO, we need something like a tc::starts_with() and TC_RANGE_STARTS_WITH for this to make sense.
//UNITTESTDEF( generator_range_break ) {
//   using namespace RANGE_PROPOSAL_NAMESPACE;
//
//   TEST_init_hack(std::vector, int, vexp, {0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50});
//   TEST_RANGE_EQUAL(vexp, tc::filter( generator_range_break(), [](int i){ return i%2==0; } ));
//}

//---- N3752 filters examples  ------------------------------------------------------------------------------------------------
UNITTESTDEF( N3752 ) {
   using namespace RANGE_PROPOSAL_NAMESPACE;

   TEST_init_hack(std::vector, int, v, {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20});

   auto r =  tc::filter( tc::filter( tc::filter(
                                v,
                                [](int i){ return i%2!=0; } ),
                                [](int i){ return i%3!=0; } ),
                                [](int i){ return i%5!=0; } );

   TEST_init_hack(std::vector, int, vexp, {1, 7, 11, 13, 17, 19});
   TEST_RANGE_EQUAL(vexp, r);

   auto ir = tc::make_iterator_range(std::begin(r), std::end(r));    // you shouldn't do this in real code! 
   TEST_RANGE_EQUAL(vexp, ir);

   auto bir = boost::make_iterator_range(boost::begin(r), boost::end(r));    // you shouldn't do this in real code! 
   TEST_RANGE_EQUAL(vexp, bir);
}

//---- Stacked filters --------------------------------------------------------------------------------------------------------
// TODO, we need something like a tc::starts_with() and TC_RANGE_STARTS_WITH for this to make sense.
//UNITTESTDEF( stacked_filters) {
//   using namespace RANGE_PROPOSAL_NAMESPACE;
//
//   TEST_init_hack(std::vector, int, vexp, {1, 7, 11, 13, 17, 19});
//   TEST_RANGE_EQUAL(vexp, tc::filter( tc::filter( tc::filter(
//                               generator_range_break(),
//                               [](int i){ return i%2!=0; } ),
//                               [](int i){ return i%3!=0; } ),
//                               [](int i){ return i%5!=0; } ));
//}

UNITTESTDEF( zero_termination ) {
	// only char is treated as zero-terminated character array.
	// signed/unsigned char is treated as a regular array
	{
		char const ach[]={ 0x20, 0 };
		// _ASSERTEQUAL( tc::size(ach), 1 ); // does not compile
		char const* pch=ach;
		_ASSERTEQUAL( tc::size(pch), 1 );
	}
	{
		signed char const ach[]={ 0x20, 0 };
		_ASSERTEQUAL( tc::size(ach), 2 );
		// signed char const* pch=ach;
		// _ASSERTEQUAL( tc::size(pch), 2 ); // does not compile
	}
	{
		unsigned char const ach[]={ 0x20, 0 };
		_ASSERTEQUAL( tc::size(ach), 2 );
		// unsigned char const* pch=ach;
		// _ASSERTEQUAL( tc::size(pch), 2 ); // does not compile
	}
}

UNITTESTDEF( ensure_index_range_on_chars ) {
	static_assert( tc::is_range_with_iterators<char*>::value, "" );
	static_assert( tc::is_range_with_iterators<char const*>::value, "" );
	static_assert( tc::is_range_with_iterators<char* &>::value, "" );
	static_assert( tc::is_range_with_iterators<char* const&>::value, "" );

	struct check_5_chars {
		check_5_chars(): m_cch(0) {};
		void operator()( char ) {
			++m_cch;
		}
		~check_5_chars() {
			_ASSERTEQUAL(m_cch,5);
		}
	private:
		std::size_t m_cch;
	};

	{
		char * str="Hello";
		{
			check_5_chars chk;
			tc::ensure_index_range(str)(std::ref(chk));
		}
		{
			check_5_chars chk;
			tc::ensure_index_range(tc::make_const(str))(std::ref(chk));
		}
		{
			check_5_chars chk;
			tc::ensure_index_range(tc_move(str))(std::ref(chk));
		}
	}
	{
		char str[]="Hello";
		{
			check_5_chars chk;
			tc::ensure_index_range(str)(std::ref(chk));
		}
		{
			check_5_chars chk;
			tc::ensure_index_range(tc::make_const(str))(std::ref(chk));
		}
	}
	{
		char const* str="Hello";
		{
			check_5_chars chk;
			tc::ensure_index_range(str)(std::ref(chk));
		}
		{
			check_5_chars chk;
			tc::ensure_index_range(tc::make_const(str))(std::ref(chk));
		}
		{
			check_5_chars chk;
			tc::ensure_index_range(tc_move(str))(std::ref(chk));
		}
	}
	{
		char const str[]="Hello";
		{
			check_5_chars chk;
			tc::ensure_index_range(str)(std::ref(chk));
		}
		{
			check_5_chars chk;
			tc::ensure_index_range(tc::make_const(str))(std::ref(chk));
		}
	}
}

}

