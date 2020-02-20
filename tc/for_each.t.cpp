
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "range.h"
#include "range.t.h"
#include "ref.h"

//---- for_each ---------------------------------------------------------------------------------------------------------------
namespace {
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
				_ASSERTEQUAL(m_index, tc::min(m_expect.size(), m_break_at + 1));
			}
			m_index = 0;
			m_expect = v;
			m_break_at = (break_at == 0) ? v.size() : break_at;
			m_expect_break = (break_at != 0) && expect_break;
			m_copyed_or_moved_from = false;
		}

		tc::break_or_continue operator()(int val) const& noexcept {
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
				_ASSERTEQUAL(val, m_expect[m_index]);
			}
			++m_index;
			return tc::continue_if(m_index <= m_break_at);
		}

		private:
			tc::vector<int> m_expect;
			mutable std::size_t m_index;
			std::size_t m_break_at;
			bool m_expect_break;
			mutable bool m_copyed_or_moved_from; // so that the copy ctor can mark the copyied from instance
	};

	all_called_mock g_mock;

	void foo(int i) noexcept { g_mock(i); }

//-----------------------------------------------------------------------------------------------------------------------------
UNITTESTDEF( for_each ) {
	using std::placeholders::_1;

	TEST_init_hack(tc::vector, int, v, {1,2,3,4,5,6,7,8,9,10});
	TEST_init_hack(tc::vector, int, exp, {1,2,3,4,5,6,7,8,9,10});

	auto gv = tc::make_generator_range(v);

	// call with functor object.
	tc::for_each(v, all_called_mock(exp));
	tc::for_each(gv, all_called_mock(exp));

	// test if break_ works
	tc::for_each(gv, all_called_mock(exp, 5));

	// call with function
	g_mock.mock_reset(exp); tc::for_each(v, foo);
	g_mock.mock_reset(exp); tc::for_each(gv, foo);

	//call with pointer to function
	g_mock.mock_reset(exp); tc::for_each(v, &foo);
	g_mock.mock_reset(exp); tc::for_each(gv, &foo);

	// call with lambda
	g_mock.mock_reset(exp); tc::for_each(v, [](int i) noexcept { g_mock(i); });
	g_mock.mock_reset(exp); tc::for_each(gv, [](int i) noexcept { g_mock(i); });

	// Todo: call with mem func, std::function and bind
}


//---- test break behavior  ---------------------------------------------------------------------------------------------------
	struct iterate final {
		tc::break_or_continue generator_break_consumer_break(tc::function_ref< tc::break_or_continue(int) noexcept > func) const& noexcept {
			for(int i = 1; i<11; ++i) {
				if(func(i) == tc::break_) { return tc::break_; }
			}
			return tc::continue_;
		}

		tc::break_or_continue generator_break_consumer_nobreak(tc::function_ref<void (int) noexcept> func) const& noexcept {
			for(int i = 1; i<11; ++i) {
				func(i);
			}
			return tc::continue_;
		}

		void generator_nobreak_consumer_nobreak(tc::function_ref<void (int) noexcept> func) const& noexcept {
			for(int i = 1; i<11; ++i) {
				func(i);
			}
			return;
		}

		void generator_nobreak_consumer_break_correct(tc::function_ref< tc::break_or_continue(int) noexcept > func) const& noexcept {
			for(int i = 1; i<11; ++i) {
				if(func(i) == tc::break_) { return; }
			}
			return;
		}

		void generator_nobreak_consumer_break_incorrect(tc::function_ref< tc::break_or_continue(int) noexcept > func) const& noexcept {
			for(int i = 1; i<11; ++i) {
				func(i);
			}
			return;
		}
	};

	struct consumer_break /*final*/ {
		consumer_break(tc::vector<int> const& v, std::size_t break_at = 0, bool expect_break = true) noexcept : m_mock(v, break_at, expect_break) {}

		tc::break_or_continue operator()(int i) const& noexcept { return m_mock(i); }
		private:
			all_called_mock m_mock;
	};

	struct consumer_nobreak /*final*/ {
		consumer_nobreak(tc::vector<int> const& v, std::size_t break_at = 0, bool expect_break = true) noexcept : m_mock(v, break_at, expect_break) {}
		~consumer_nobreak() {}

		void operator()(int i) const& noexcept { m_mock(i); }

		private:
			all_called_mock m_mock;
	};

//-----------------------------------------------------------------------------------------------------------------------------
UNITTESTDEF( break_behavior ) {
	TEST_init_hack(tc::vector, int, exp, {1,2,3,4,5,6,7,8,9,10});

	// call on various implicit ranges
	tc::for_each([](auto&& func) noexcept { return iterate().generator_break_consumer_break(std::forward<decltype(func)>(func)); }, consumer_break(exp, 4));
	tc::for_each([](auto&& func) noexcept { return iterate().generator_break_consumer_nobreak(std::forward<decltype(func)>(func)); }, consumer_nobreak(exp, 5, false));
	tc::for_each([](auto&& func) noexcept { iterate().generator_nobreak_consumer_nobreak(std::forward<decltype(func)>(func)); }, consumer_nobreak(exp, 6, false));

	// these two are undefined and must not compile
//	tc::for_each([](auto&& func) noexcept { iterate().generator_nobreak_consumer_break_correct(std::forward<decltype(func)>(func)); }, all_called_mock(exp, 7));
//	tc::for_each([](auto&& func) noexcept { iterate().generator_nobreak_consumer_break_incorrect(std::forward<decltype(func)>(func)); }, all_called_mock(exp, 8, false));
}

UNITTESTDEF(for_each_adjacent_tuple_deref) {
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

	tc::vector<int> vecn{0,0,0,0,0};
	tc::for_each_adjacent_tuple<3>(
		vecn,
		[](int& n0, int& n1, int& n2) noexcept {
			++n0;
			++n1;
			++n2;
		}
	);

	TEST_RANGE_EQUAL(
		vecn,
		MAKE_CONSTEXPR_ARRAY(1,2,3,2,1)
	);

	int nTransforms = 0;
	tc::for_each_adjacent_tuple<3>(
		tc::transform(
			vecn,
			[&](int n) noexcept {++nTransforms; return n;}
		),
		[](int n0, int n1, int n2) noexcept {
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

		TEST_RANGE_EQUAL(overloads.m_n, MAKE_CONSTEXPR_ARRAY(3,0,0));
	}

	{
		lr_overloads overloads;
		tc::for_each_adjacent_tuple<3>(
			tc::transform(vecn, [](int n) noexcept {return n;}),
			std::ref(overloads)
		);

		TEST_RANGE_EQUAL(overloads.m_n, MAKE_CONSTEXPR_ARRAY(0,2,1));
	}
}


UNITTESTDEF(for_each_ordered_pair) {
	tc::vector<int> vecn{1,2,3,4};
	tc::vector<std::pair<int, int>> vecpairnn;
	tc::for_each_ordered_pair(
		vecn,
		[&](auto&& _1, auto&& _2) noexcept { tc::cont_emplace_back(vecpairnn, std::forward<decltype(_1)>(_1), std::forward<decltype(_2)>(_2)); }
	);
	_ASSERTEQUAL(tc::size(vecpairnn), 6);
	_ASSERT(tc::find_unique<tc::return_bool>(vecpairnn, std::make_pair(1, 2)));
	_ASSERT(tc::find_unique<tc::return_bool>(vecpairnn, std::make_pair(1, 3)));
	_ASSERT(tc::find_unique<tc::return_bool>(vecpairnn, std::make_pair(1, 4)));
	_ASSERT(tc::find_unique<tc::return_bool>(vecpairnn, std::make_pair(2, 3)));
	_ASSERT(tc::find_unique<tc::return_bool>(vecpairnn, std::make_pair(2, 4)));
	_ASSERT(tc::find_unique<tc::return_bool>(vecpairnn, std::make_pair(3, 4)));

	tc::for_each_ordered_pair(
		std::initializer_list<int>({}),
		[](int, int) noexcept { _ASSERTFALSE; }
	);

	tc::for_each_ordered_pair(
		tc::single(1),
		[](int, int) noexcept { _ASSERTFALSE; }
	);
}

}
