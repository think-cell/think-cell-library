#include "sub_range.h"
#include "range.t.h"

#include <vector>
#include <type_traits>
#include <iostream>

namespace {
	using namespace RANGE_PROPOSAL_NAMESPACE;

	//-------------------------------------------------------------------------------------------------------------------------
	// make_sub_range_result stanity checks
	namespace sub_range_detail{
		template< typename Rng > struct make_sub_range_result_tests { 
			template <typename Lhs, typename Rhs> static void test_const_ref_variants() {
				//static_assert(std::is_same< typename make_sub_range_result<Lhs>::type, sub_range<Rhs> >::value, "make_sub_range_result(Rng) gives wrong result");
				//static_assert(std::is_same< typename make_sub_range_result<Lhs&>::type, sub_range<Rhs&> >::value, "make_sub_range_result(Rng&) gives wrong result");
				//static_assert(std::is_same< typename make_sub_range_result<Lhs const>::type, sub_range<Rhs const> >::value, "make_sub_range_result(Rng const) gives wrong result");
				//static_assert(std::is_same< typename make_sub_range_result<Lhs const&>::type, sub_range<Rhs const&> >::value, "make_sub_range_result(Rng const&) gives wrong result");
			}

			static void test_sub_range_unrolling() {
				test_const_ref_variants<Rng, Rng>();
				test_const_ref_variants<sub_range<Rng>, Rng>();
				test_const_ref_variants<sub_range<sub_range<Rng>>, Rng>();
				test_const_ref_variants<sub_range<sub_range<sub_range<sub_range<sub_range<Rng>>>>>, Rng>(); // "proof" of induction
			}

			static void test() {
				test_sub_range_unrolling();
			}
		};

		static void test_make_sub_range_result() {
			make_sub_range_result_tests<std::vector<int>>::test();
			make_sub_range_result_tests<std::string>::test();
			//make_sub_range_result_tests<tc::bstr>::test();
		}
	}

	//-------------------------------------------------------------------------------------------------------------------------

	typedef make_sub_range_result<std::vector<int>&>::type  SRVI;
	typedef make_sub_range_result<std::vector<int> const&>::type  CSRVI;

	typedef make_sub_range_result<make_sub_range_result<std::vector<int>&>::type>::type SSRVI;
	typedef make_sub_range_result<make_sub_range_result<std::vector<int> const&>::type>::type CSSRVI;

	//void const_ref_test(SRVI const & rng) {
	//	//CSRVI const_rng(rng);  // TODO: this fails, but shouldn't! sub_range does detect this, but range_adapter can't cope ...
	//	SRVI non_const_rng(rng); // Todo: should not work!
	//}

	//void ref_test(SRVI & rng) {
	//	CSRVI const_rng(rng);  
	//	SRVI non_const_rng(rng); 
	//}

	UNITTESTDEF( sub_range_array ) {

		int arr[4] = {1,2,3,4};
		auto arr_rng = slice(arr, 1,3);

		sub_range<iterator_base<int *>> mutable_iter_rng = arr_rng;
		//xx sub_range<iterator_base<int const*>> iter_rng = arr_rng;
	}


	UNITTESTDEF( const_sub_range_char_ptr ) {
		//xx sub_range<tc::iterator_base<char const*>> srccp = "test";
	}

	//-------------------------------------------------------------------------------------------------------------------------

	typedef make_sub_range_result<std::vector<int>&>::type  SRVI;
	typedef make_sub_range_result<std::vector<int> const&>::type  CSRVI;

	typedef make_sub_range_result<make_sub_range_result<std::vector<int>&>::type>::type SSRVI;
	typedef make_sub_range_result<make_sub_range_result<std::vector<int> const&>::type>::type CSSRVI;

	void const_ref_test(SRVI const & rng) {
		//CSRVI const_rng(rng);    // same as const_rng2 below. TODO: this fails, but shouldn't!
		//SRVI non_const_rng(rng); // same as non_const_rng4 below. fails to compile, as it should.
	}

	void ref_test(SRVI & rng) {
		CSRVI const_rng(rng);  
		SRVI non_const_rng(rng); 
	}

	UNITTESTDEF( const_sub_range ) {
		//static_assert(!std::is_convertible<CSRVI, SRVI>::value, "const sub-range is convertible to sub-range breaking const correctness (explicit)");

		std::vector<int> v;
		auto srvi = slice(v);
		auto csrvi = slice(make_const(v));

		(void) srvi;
		(void) csrvi;

		SRVI non_const_rng(srvi);
		
		CSRVI const_rng(srvi);
		//CSRVI const_rng2(make_const(srvi));   // TODO: this fails, but shouldn't! sub_range does detect this, but range_adapter can't cope ...
		
		//auto sr = make_const(srvi);
		//auto sr = slice(make_const(srvi));
		//auto sr = make_const(slice(v, boost::begin(v), boost::end(v))); // ??

		//static_assert(!std::is_convertible<decltype(csrvi), decltype(srvi)>::value, "const sub-range is convertible to sub-range breaking const correctness (decltype)"); // this assert fires! TODO: fix, should also improve error msg below
		//SRVI non_const_rng2(csrvi);             // fails to compile, as it should, msg is correct but still pretty horrible
		//SRVI non_const_rng3(make_const(v));     // fails to compile, as it should, msg is correct but still pretty horrible
		//SRVI non_const_rng4 = make_const(srvi); // fails to compile, as it should

		//_ASSERT(false);

		//ref_test(srvi);
		//const_ref_test(srvi);
	}

	UNITTESTDEF( sub_sub_range_rvalue ) {

		std::vector<int> v;
		
		auto srvi = slice(v);
		auto csrvi = slice(make_const(v));

		auto ssrvi = slice(slice(v));
		auto cssrvi = slice(slice(make_const(v)));

		static_assert(std::is_same<decltype(ssrvi), decltype(srvi)>::value, "Sub-sub-range does not flatten to sub-range (decltype)");
		static_assert(std::is_same<decltype(cssrvi), decltype(csrvi)>::value, "const sub-sub-range does not flatten to const sub-range (decltype)");
	}

	UNITTESTDEF( sub_sub_range_lvalue ) {

		std::vector<int> v;

		auto srvi = slice(v);
		auto csrvi = slice(make_const(v));

		auto ssrvi = slice(srvi);
		auto cssrvi = slice(csrvi);

		// sanity checks
		static_assert(std::is_same<decltype(srvi), SRVI>::value, "make_sub_range_result gives wrong result");
		static_assert(std::is_same<decltype(csrvi), CSRVI>::value, "make_sub_range_result gives wrong result");
		static_assert(std::is_same<decltype(ssrvi), SSRVI>::value, "make_sub_range_result gives wrong result");
		static_assert(std::is_same<decltype(cssrvi), CSSRVI>::value, "make_sub_range_result gives wrong result");

		// the actual test
		static_assert(std::is_same<decltype(ssrvi), decltype(srvi)>::value, "Sub-sub-range does not flatten to sub-range");
		static_assert(std::is_same<decltype(cssrvi), decltype(csrvi)>::value, "const sub-sub-range does not flatten to const sub-range");
	}

	std::vector<int> tmp_vec() {
		return std::vector<int>();
	}

	UNITTESTDEF( sub_sub_range_index ) {

		TEST_init_hack(std::vector, int, v, {1,2,3,4,5,6,7,8,9});
		TEST_init_hack(std::vector, int, exp36, {4,5,6});
		
		auto sr = slice(v);
		auto csr = slice(make_const(v));

		// use range_difference to specify bounds
		auto ssr1 = slice(sr, 3, 6);
		auto cssr1 = slice(csr, 3, 6);
		
		//_ASSERTEQUAL(ssr1, exp36);
		//_ASSERTEQUAL(cssr1, exp36);
		static_assert(std::is_same<decltype(ssr1), decltype(sr)>::value, "Sub-sub-range does not flatten to sub-range");
		static_assert(std::is_same<decltype(cssr1), decltype(csr)>::value, "const sub-sub-range does not flatten to const sub-range");

	}

	UNITTESTDEF( sub_sub_range_crazy_manual ) {

		std::vector<int> v;

		// don't try that at work! - Seriously: sub_range is build to work correctly in a variety of situations,
		// but normaly it should not be necessary to manualy utter the type. Use slice and make_sub_range_result instead.
		// if you do need to say the type (e.g. when deducting sth.)  ... 
		//_ASSERT(false);
		auto srvi = sub_range<std::vector<int>&>(v);                                                      // create a sub range from unrelated range
		auto ssrvi = sub_range<std::vector<int>&>(sub_range<std::vector<int>&>(v));                       // copy sub range
		//auto s2rvi = sub_range<std::vector<int>&>(sub_range<sub_range<std::vector<int>&>>(v));            // flatten sub range

		//auto s3rvi = sub_range<std::vector<int>&>(sub_range<sub_range<sub_range<std::vector<int>&>>>(v)); // recursive flatten 
		//auto s4rvi = sub_range<std::vector<int>&>(sub_range<sub_range<sub_range<sub_range<std::vector<int>&>>>>(v));

		// ... make sure to not build sub sub ranges (see below) - those should not occur in the wild.
		//auto ssr = sub_range<sub_range<std::vector<int>&>>(sub_range<std::vector<int>&>(v));
		//auto ssr2 = sub_range<sub_range<std::vector<int>&>>(v);
	}

}

