//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

#include "range.h"
#include "range.t.h"

//---- for_each ---------------------------------------------------------------------------------------------------------------
namespace {
	using namespace tc;

	struct all_called_mock final {
		all_called_mock() noexcept : m_expect(0), m_index(0), m_break_at(0), m_copyed_or_moved_from(false) {}
		all_called_mock(tc::vector<int> const& v, std::size_t break_at = 0, bool expect_break = true) noexcept
			: m_expect(v),
			  m_index(0),
			  m_break_at((break_at == 0) ? v.size() : break_at),
			  m_expect_break((break_at != 0) && expect_break),
			  m_copyed_or_moved_from(false)
		{}
		all_called_mock(all_called_mock const& copy) noexcept :
			  m_expect(copy.m_expect),
			  m_index(copy.m_index),
			  m_break_at(copy.m_break_at),
			  m_expect_break(copy.m_expect_break),
			  m_copyed_or_moved_from(false)
		{
			copy.m_copyed_or_moved_from = true;
		}
		all_called_mock(all_called_mock&& move) noexcept :
			  m_expect(move.m_expect),
			  m_index(move.m_index),
			  m_break_at(move.m_break_at),
			  m_expect_break(move.m_expect_break),
			  m_copyed_or_moved_from(false)
		{
			move.m_copyed_or_moved_from = true;
		}
		~all_called_mock() {
			mock_reset();
		}

		void mock_reset(tc::vector<int> const& v = tc::vector<int>(), std::size_t break_at = 0, bool expect_break = true) & noexcept {
			if(!m_copyed_or_moved_from && !(m_index == tc::min(m_expect.size(), (m_expect_break) ? m_break_at + 1 : m_expect.size()))) {
				TEST_OUTPUT( << "unexpectedly terminated before index " << m_index
							 << " went to the expected index " << tc::min(m_expect.size(), m_break_at + 1) << '\n');
				_ASSERT(m_index == tc::min(m_expect.size(), m_break_at + 1));
			}
			m_index = 0;
			m_expect = v;
			m_break_at = (break_at == 0) ? v.size() : break_at;
			m_expect_break = (break_at != 0) && expect_break;
			m_copyed_or_moved_from = false;
		}

		break_or_continue operator()(int val) & noexcept {
			if (m_copyed_or_moved_from) {
				TEST_OUTPUT(<< "used copyed or moved consumer for real work!\n");
			}
			if(!(m_index < m_expect.size())) {
				TEST_OUTPUT( << "unexpectedly called for index " << m_index
							 << " when expect has size " << m_expect.size() << '\n');
				_ASSERT(m_index < m_expect.size());
			}
			if (val != m_expect[m_index]) {
				TEST_OUTPUT( << "unexpected value " << val << " at index " << m_index
							 << ", should be " << m_expect[m_index] << '\n');
				_ASSERT(val == m_expect[m_index]);
			}
			++m_index;
			return (m_index <= m_break_at)? continue_ : break_;
		}

		private:
			tc::vector<int> m_expect;
			std::size_t m_index;
			std::size_t m_break_at;
			bool m_expect_break;
			mutable bool m_copyed_or_moved_from; // so that the copy ctor can mark the copyied from instance
	};

	all_called_mock g_mock;

	void foo(int i) noexcept { g_mock(i); }
		
//-----------------------------------------------------------------------------------------------------------------------------
UNITTESTDEF( for_each ) {
	using namespace tc;
	using std::placeholders::_1;

	TEST_init_hack(tc::vector, int, v, {1,2,3,4,5,6,7,8,9,10});
	TEST_init_hack(tc::vector, int, exp, {1,2,3,4,5,6,7,8,9,10});

	auto gv = make_generator_range(v);

	// call with functor object.
	for_each(v, all_called_mock(exp));
	for_each(gv, all_called_mock(exp));

	// test if break_ works
	for_each(gv, all_called_mock(exp, 5));

	// call with function
	g_mock.mock_reset(exp); for_each(v, foo);
	g_mock.mock_reset(exp); for_each(gv, foo);

	//call with pointer to function
	g_mock.mock_reset(exp); for_each(v, &foo);
	g_mock.mock_reset(exp); for_each(gv, &foo);

	// call with lambda
	g_mock.mock_reset(exp); for_each(v, [](int i){ g_mock(i); });
	g_mock.mock_reset(exp); for_each(gv, [](int i){ g_mock(i); });

	// Todo: call with mem func, std::function and bind
}


//---- test break behavior  ---------------------------------------------------------------------------------------------------
	struct iterate final {
		break_or_continue generator_break_consumer_break(function< break_or_continue(int) > func) const& noexcept {
			for(int i = 1; i<11; ++i) {
				if(func(i) == break_) { return break_; }
			}
			return continue_;
		}

		break_or_continue generator_break_consumer_nobreak(std::function<void (int)> func) const& noexcept {
			for(int i = 1; i<11; ++i) {
				func(i);
			}
			return continue_;
		}

		void generator_nobreak_consumer_nobreak(std::function<void (int)> func) const& noexcept {
			for(int i = 1; i<11; ++i) {
				func(i);
			}
			return;
		}

		void generator_nobreak_consumer_break_correct(function< break_or_continue(int) > func) const& noexcept {
			for(int i = 1; i<11; ++i) {
				if(func(i) == break_) { return; }
			}
			return;
		}

		void generator_nobreak_consumer_break_incorrect(function< break_or_continue(int) > func) const& noexcept {
			for(int i = 1; i<11; ++i) {
				func(i);
			}
			return;
		}
	};

	struct consumer_break final {
		consumer_break(tc::vector<int> const& v, std::size_t break_at = 0, bool expect_break = true) noexcept : m_mock(v, break_at, expect_break) {}

		break_or_continue operator()(int i) /* no & */ noexcept { return m_mock(i); }
		private:
			all_called_mock m_mock;
	};

	struct consumer_nobreak final {
		consumer_nobreak(tc::vector<int> const& v, std::size_t break_at = 0, bool expect_break = true) noexcept : m_mock(v, break_at, expect_break), m_accu(0) {}
		~consumer_nobreak() {}

		void operator()(int i) /* no & */ noexcept { m_mock(i); m_accu += i; }

		operator int() & noexcept { return m_accu; }

		private:
			all_called_mock m_mock;
			int m_accu;
	};

//-----------------------------------------------------------------------------------------------------------------------------
UNITTESTDEF( break_behavior ) {
	using namespace tc;

	TEST_init_hack(tc::vector, int, exp, {1,2,3,4,5,6,7,8,9,10});

	// call on various implicit ranges
	for_each(std::bind(&iterate::generator_break_consumer_break, iterate(), std::placeholders::_1), consumer_break(exp, 4));
	for_each(std::bind(&iterate::generator_break_consumer_nobreak, iterate(), std::placeholders::_1), consumer_nobreak(exp, 5, false));
	for_each(std::bind(&iterate::generator_nobreak_consumer_nobreak, iterate(), std::placeholders::_1), consumer_nobreak(exp, 6, false));

	// these two are undefined and must not compile
//	for_each(std::bind(&iterate::generator_nobreak_consumer_break_correct, iterate(), std::placeholders::_1), all_called_mock(exp, 7));
//	for_each(std::bind(&iterate::generator_nobreak_consumer_break_incorrect, iterate(), std::placeholders::_1), all_called_mock(exp, 8, false));
}

UNITTESTDEF(for_each_adjacent_triple_initialization_order) {
	int n=0;
	auto f = [&] {return ++n;};
	int a[2] = { f(), f() };
	_ASSERT(1 == a[0] && 2 == a[1]);

	// Does not work in VC12:
	/*
	tc::array<int, 2> a2{ f(), f() };
	_ASSERT(3 == a2[0] && 4 == a2[1]);
	*/
}

#ifdef _CHECKS

	struct lr_overloads final{
		std::array<int,3> m_n;

		lr_overloads() noexcept {
			m_n[0] = 0;
			m_n[1] = 0;
			m_n[2] = 0;
		}

		void operator()(int const&, int const&, int const&) & noexcept { ++m_n[0];}
		void operator()(int&&, int const&, int const&) & noexcept { ++m_n[1]; }
		void operator()(int&&, int&&, int&&) & noexcept { ++m_n[2]; }
	};
#endif

UNITTESTDEF(for_each_adjacent_triple_deref) {
	tc::vector<int> vecn{0,0,0,0,0};
	tc::for_each_adjacent_tuple<3>(
		vecn,
		[](int& n0, int& n1, int& n2) {
			++n0;
			++n1;
			++n2;
		}
	);

	TEST_RANGE_EQUAL(
		vecn,
		make_initializer_list({1,2,3,2,1})
	);

	int nTransforms = 0;
	tc::for_each_adjacent_tuple<3>(
		tc::transform(
			vecn,
			[&](int n) {++nTransforms; return n;}
		),
		[](int n0, int n1, int n2) {
			++n0;
			++n1;
			++n2;
		}
	);
	_ASSERTEQUAL(nTransforms,5);

	{
		lr_overloads overloads;
		tc::for_each_adjacent_tuple<3>(
			vecn,
			std::ref(overloads)
		);

		TEST_RANGE_EQUAL(overloads.m_n, make_initializer_list({3,0,0}));
	}

	{
		lr_overloads overloads;
		tc::for_each_adjacent_tuple<3>(
			tc::transform(vecn, [](int n) {return n;}),
			std::ref(overloads)
		);

		TEST_RANGE_EQUAL(overloads.m_n, make_initializer_list({0,2,1}));
	}
}


UNITTESTDEF(for_each_ordered_pair) {
	tc::vector<int> vecn{1,2,3,4};
	tc::vector<std::pair<int, int>> vecpairnn;
	tc::for_each_ordered_pair(
		vecn,
		std::bind(mem_fn_emplace_back(), std::ref(vecpairnn), std::placeholders::_1, std::placeholders::_2)
	);
	_ASSERTEQUAL(tc::size(vecpairnn), 6);
	_ASSERT(tc::find_unique<tc::return_bool>(vecpairnn, std::make_pair(1, 2)));
	_ASSERT(tc::find_unique<tc::return_bool>(vecpairnn, std::make_pair(1, 3)));
	_ASSERT(tc::find_unique<tc::return_bool>(vecpairnn, std::make_pair(1, 4)));
	_ASSERT(tc::find_unique<tc::return_bool>(vecpairnn, std::make_pair(2, 3)));
	_ASSERT(tc::find_unique<tc::return_bool>(vecpairnn, std::make_pair(2, 4)));
	_ASSERT(tc::find_unique<tc::return_bool>(vecpairnn, std::make_pair(3, 4)));

	tc::for_each_ordered_pair(
		make_initializer_list<int>({}),
		[](int, int) { _ASSERTFALSE; }
	);

	tc::for_each_ordered_pair(
		make_initializer_list<int>({1}),
		[](int, int) { _ASSERTFALSE; }
	);
}

}
