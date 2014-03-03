#include "../Range.h"
#include "range.t.h"

#include <vector>

namespace {
	using namespace RANGE_PROPOSAL_NAMESPACE;

	void static_tests() {
		static_assert(!is_range_with_iterators<int>::value, "has..._iterator reports that int has an iterator");

		std::vector<int> v;
		static_assert(is_range_with_iterators<decltype(v)>::value, "has..._iterator reports that vector has no iterator");
		//static_assert(has_range_mutable_iterator<decltype(v)>::value, "has..._iterator reports that vector has no mutable iterator");
		//static_assert(has_range_const_iterator<decltype(v)>::value, "has..._iterator reports that vector has no const iterator");

		//static_assert(std::is_same<range_iterator<decltype(v)>::type, std::vector<int>::iterator>::value, "vectors default iterator is not reported to be what it should be");
		//static_assert(std::is_same<range_mutable_iterator<decltype(v)>::type, std::vector<int>::iterator>::value, "vectors mutable iterator is not reported to be what it should be");
		//static_assert(std::is_same<range_const_iterator<decltype(v)>::type, std::vector<int>::const_iterator>::value, "vectors const iterator is not reported to be what it should be");

		//static_assert(std::is_same<range_iterator<decltype(make_const(v))>::type, std::vector<int>::const_iterator>::value, "const vectors default iterator is not reported to be what it should be");
		//static_assert(std::is_same<range_mutable_iterator<decltype(make_const(v))>::type, std::vector<int>::const_iterator>::value, "const vectors mutable iterator is not reported to be what it should be"); // Todo: should such a thing fail to compile instead?
		//static_assert(std::is_same<range_const_iterator<decltype(make_const(v))>::type, std::vector<int>::const_iterator>::value, "const vectors const iterator is not reported to be what it should be");
	}

	auto make_const_range(std::vector<int> const & v) return_decltype( make_iterator_range(v) ) // Todo: Do we want/need sometilladis as a generic tool?

UNITTESTDEF( const_range ) {

	TEST_init_hack(std::vector, int, original, {1,2,3,4,5,6,7,8});
	TEST_init_hack(std::vector, int, modified, {2,3,4,5,6,7,8,9});

	std::vector<int> v = original;

	auto mutable_range = make_iterator_range(v); TEST_RANGE_LENGTH(mutable_range, 8);

	TEST_RANGE_EQUAL(original, mutable_range);
	TEST_RANGE_NOT_EQUAL(modified, mutable_range);
	for_each(mutable_range, [](int& i) { i += 1; });
	TEST_RANGE_EQUAL(modified, mutable_range);
	TEST_RANGE_NOT_EQUAL(original, mutable_range);

	v = original;
	auto const_range = make_const_range(v); TEST_RANGE_LENGTH(const_range, 8);
	static_assert(std::is_same<decltype(make_const_range(v)), decltype(make_iterator_range(make_const(v)))>::value, "wrong const type");

	TEST_RANGE_EQUAL(original, const_range);
	TEST_RANGE_NOT_EQUAL(modified, const_range);
	//for_each(const_range, [](int& i) { i += 1; });        // breaks with a horrible error msg. Todo: see if we can make a better msg.
	//for_each(const_range, [](int const& i) { i += 1; });  // breaks with clear msg as it should.
	for_each(const_range, [](int const& i) { i; });
	TEST_RANGE_EQUAL(original, const_range);
	TEST_RANGE_NOT_EQUAL(modified, const_range);
}

//-----------------------------------------------------------------------------------------------------------------------------
// filter constness tests

template <typename Rng, typename Func>
void test_const_filter(filter_range<Func, Rng> const& f) {

	//filter_range<Func, Rng> non_const_should_fail(f); // do not add ctor_const_overload() here, you would be lying
//	filter_range<Func, Rng> const const_filter(f, ctor_const_overload());
//	filter_range<Func, Rng const> filter_of_const_rng(f, ctor_const_overload());
	//filter_range<Func, Rng const> filter_of_const_rng2(f); // this should work without ctor_const_overload()
}

UNITTESTDEF( const_filter_test ) {

	TEST_init_hack(std::vector, int, v, {1,2,3,4,5,6,7,8,9});

	auto fr = tc::filter(v, [](int const& i) { return (i % 2 == 0); });

	test_const_filter(fr);

	//auto const frc(make_const(fr), ctor_const_overload());
}

UNITTESTDEF( filter_const_filter_test ) {

	TEST_init_hack(std::vector, int, v, {1,2,3,4,5,6,7,8,9});

	auto fr = tc::filter(v, [](int const& i) { return (i % 2 == 0); });
	
	auto fcfr = tc::filter(make_const(fr), [](int const& i) { return (i % 2 == 0); });
	auto ffr = tc::filter(fr, [](int const& i) { return (i % 2 == 0); });
}

UNITTESTDEF( filter_filter_const_test ) {

	TEST_init_hack(std::vector, int, v, {1,2,3,4,5,6,7,8,9});

	auto frc = tc::filter(make_const(v), [](int const& i) { return (i % 2 == 0); });
	
	auto ffrc = tc::filter(frc, [](int const& i) { return (i % 2 == 0); });
	auto fcfrc = tc::filter(make_const(frc), [](int const& i) { return (i % 2 == 0); });
	auto ffrc2 = tc::filter(tc::filter(make_const(v), [](int const& i) { return (i % 2 == 0); }), [](int const& i) { return (i % 2 == 0); });
}

UNITTESTDEF( filter_filter_const_move_test ) {

	TEST_init_hack(std::vector, int, v, {1,2,3,4,5,6,7,8,9});

/*	auto ffcfr = tc::filter(
					tc::filter(
						make_const( tc::filter(v, [](int const& i) { return (i % 2 == 0); }))
						, [](int const& i) { return (i % 2 == 0); }
					)
					, [](int const& i) { return (i % 2 == 0); }
				);*/
}

//-----------------------------------------------------------------------------------------------------------------------------
// transform constness tests

template <typename Rng, typename Func>
void test_const_transform(transform_range<Func, Rng> const& t) {

	//transform_range<Func, Rng> non_const_should_fail(t); // do not add ctor_const_overload() here, you would be lying
//	transform_range<Func, Rng> const const_transform(t, ctor_const_overload());
//	transform_range<Func, Rng const> transform_of_const_rng(t, ctor_const_overload());
	//transform_range<Func, Rng const> transform_of_const_rng2(t); // this should work without ctor_const_overload()
}

UNITTESTDEF( const_transform_test ) {

	TEST_init_hack(std::vector, int, v, {1,2,3,4,5,6,7,8,9});

	auto tr = tc::transform(v, [](int const& i) { return (i * 2); });

	test_const_transform(tr);

	//auto const trc(make_const(tr), ctor_const_overload());
}

UNITTESTDEF( transform_const_transform_test ) {

	TEST_init_hack(std::vector, int, v, {1,2,3,4,5,6,7,8,9});

	auto tr = tc::transform(v, [](int const& i) { return (i * 2); });
	
	auto tctr = tc::transform(make_const(tr), [](int const& i) { return (i * 2); });
	auto ttr = tc::transform(tr, [](int const& i) { return (i * 2); });
}

UNITTESTDEF( transform_transform_const_test ) {

	TEST_init_hack(std::vector, int, v, {1,2,3,4,5,6,7,8,9});

	auto trc = tc::transform(make_const(v), [](int const& i) { return (i * 2); });
	
	auto ttrc = tc::transform(trc, [](int const& i) { return (i * 2); });

	auto ttrc2 = tc::transform(tc::transform(make_const(v), [](int const& i) { return (i * 2); }), [](int const& i) { return (i * 2); });

}

UNITTESTDEF( transform_transform_const_move_test ) {

	TEST_init_hack(std::vector, int, v, {1,2,3,4,5,6,7,8,9});

/*	auto ttctr = tc::transform(
					tc::transform(
						make_const( tc::transform(v, [](int const& i) { return (i * 2); }))
						, [](int const& i) { return (i * 2); }
					)
					, [](int const& i) { return (i * 2); }
				);*/
}

//-----------------------------------------------------------------------------------------------------------------------------
// sub constness tests

//template <typename Rng>
//void test_const_sub(sub_range<Rng> const& s) {
//
//	//sub_range<Rng> non_const_should_fail(s); // do not add ctor_const_overload() here, you would be lying
//	sub_range<Rng> const const_sub(s, ctor_const_overload());
//	sub_range<Rng const> sub_of_const_rng(s, ctor_const_overload());
//	//sub_range<Rng const> sub_of_const_rng2(s); // this should work without ctor_const_overload()
//}
//
//UNITTESTDEF( const_sub_test ) {
//
//	TEST_init_hack(std::vector, int, v, {1,2,3,4,5,6,7,8,9});
//
//	auto tr = tc::slice(v, boost::begin(v), boost::end(v));
//
//	test_const_sub(tr);
//
//	//auto const trc(make_const(tr), ctor_const_overload());
//}
//
//UNITTESTDEF( sub_const_sub_test ) {
//
//	TEST_init_hack(std::vector, int, v, {1,2,3,4,5,6,7,8,9});
//
//	auto sr = tc::slice(v);
//	
//	auto scsr = tc::slice(make_const(sr));
//	auto ssr = tc::slice(sr);
//}
//
//UNITTESTDEF( sub_sub_const_test ) {
//
//	TEST_init_hack(std::vector, int, v, {1,2,3,4,5,6,7,8,9});
//
//	auto src = tc::slice(make_const(v));
//	
//	auto ssrc = tc::slice(src);
//
//	auto ttrc2 = tc::slice(tc::slice(make_const(v)));
//}

//-----------------------------------------------------------------------------------------------------------------------------
// mixed ranges constness tests

UNITTESTDEF( transform_const_filter_test ) {

	TEST_init_hack(std::vector, int, v, {1,2,3,4,5,6,7,8,9});

	auto tr = tc::filter(v, [](int const& i) { return (i % 2 == 0); });
	
	auto tcfr = tc::transform(make_const(tr), [](int const& i) { return (i * 2); });
	auto tfr = tc::transform(tr, [](int const& i) { return (i * 2); });
}

//-----------------------------------------------------------------------------------------------------------------------------

// Todo: more tests with more complex setups, i.e. chains of (sub)ranges with variying constness

}

