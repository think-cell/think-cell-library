#include "../Range.h"
#include "range.t.h"

//---- for_each ---------------------------------------------------------------------------------------------------------------
namespace {
	using namespace RANGE_PROPOSAL_NAMESPACE;

	struct all_called_mock {
		all_called_mock() : m_expect(0), m_index(0), m_break_at(0), m_copyed_or_moved_from(false) {}
		all_called_mock(const std::vector<int>& v, std::size_t break_at = 0, bool expect_break = true) 
			: m_expect(v), 
			  m_index(0), 
			  m_break_at((break_at == 0) ? v.size() : break_at),
			  m_expect_break((break_at != 0) && expect_break),
			  m_copyed_or_moved_from(false)
		{}
		all_called_mock(all_called_mock const& copy) : 
			  m_expect(copy.m_expect), 
			  m_index(copy.m_index), 
			  m_break_at(copy.m_break_at),
			  m_expect_break(copy.m_expect_break),
			  m_copyed_or_moved_from(false)
		{
			copy.m_copyed_or_moved_from = true;
		}
		all_called_mock(all_called_mock&& move) : 
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

		void mock_reset(const std::vector<int>& v = std::vector<int>(), std::size_t break_at = 0, bool expect_break = true) {
			if(!m_copyed_or_moved_from && !(m_index == std::min(m_expect.size(), (m_expect_break) ? m_break_at + 1 : m_expect.size()))) { 
				TEST_OUTPUT( << "unexpectedly terminated before index " << m_index 
							 << " went to the expected index " << std::min(m_expect.size(), m_break_at + 1) << "\n");
				_ASSERT(m_index == std::min(m_expect.size(), m_break_at + 1));
			}
			m_index = 0;
			m_expect = v;
			m_break_at = (break_at == 0) ? v.size() : break_at;
			m_expect_break = (break_at != 0) && expect_break;
			m_copyed_or_moved_from = false;
		}

		break_or_continue operator()(int val) {
			if (m_copyed_or_moved_from) {
				TEST_OUTPUT(<< "used copyed or moved consumer for real work!\n");
			}
			if(!(m_index < m_expect.size())) { 
				TEST_OUTPUT( << "unexpectedly called for index " << m_index 
							 << " when expect has size " << m_expect.size() << "\n");
				_ASSERT(m_index < m_expect.size());
			}
			if (val != m_expect[m_index]) {
				TEST_OUTPUT( << "unexpected value " << val << " at index " << m_index 
							 << ", should be " << m_expect[m_index] << "\n");
				_ASSERT(val == m_expect[m_index]);
			}
			++m_index;
			return (m_index <= m_break_at)? continue_ : break_;
		}

		private:
			std::vector<int> m_expect;
			std::size_t m_index;
			std::size_t m_break_at;
			bool m_expect_break;
			mutable bool m_copyed_or_moved_from; // so that the copy ctor can mark the copyied from instance
	};

	all_called_mock g_mock;

	void foo(int i) { g_mock(i); }
		
//-----------------------------------------------------------------------------------------------------------------------------
UNITTESTDEF( for_each ) {
	using namespace RANGE_PROPOSAL_NAMESPACE;
	using std::placeholders::_1;

	TEST_init_hack(std::vector, int, v, {1,2,3,4,5,6,7,8,9,10});
	TEST_init_hack(std::vector, int, exp, {1,2,3,4,5,6,7,8,9,10});

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
	struct iterate {
		break_or_continue generator_break_consumer_break(function< break_or_continue(int) > func) const {
			for(int i = 1; i<11; ++i) {
				if(func(i) == break_) { return break_; }
			}
			return continue_; 
		}

		break_or_continue generator_break_consumer_nobreak(std::function<void (int)> func) const {
			for(int i = 1; i<11; ++i) {
				func(i);
			}
			return continue_; 
		}

		void generator_nobreak_consumer_nobreak(std::function<void (int)> func) const {
			for(int i = 1; i<11; ++i) {
				func(i);
			}
			return; 
		}

		void generator_nobreak_consumer_break_correct(function< break_or_continue(int) > func) const {
			for(int i = 1; i<11; ++i) {
				if(func(i) == break_) { return; }
			}
			return; 
		}

		void generator_nobreak_consumer_break_incorrect(function< break_or_continue(int) > func) const {
			for(int i = 1; i<11; ++i) {
				func(i);
			}
			return; 
		}
	};

	struct consumer_break {
		consumer_break(const std::vector<int>& v, std::size_t break_at = 0, bool expect_break = true) : m_mock(v, break_at, expect_break) {}

		break_or_continue operator()(int i) { return m_mock(i); }
		private:
			all_called_mock m_mock;
	};

	struct consumer_nobreak {
		consumer_nobreak(const std::vector<int>& v, std::size_t break_at = 0, bool expect_break = true) : m_mock(v, break_at, expect_break), m_accu(0) {}
		~consumer_nobreak() {}

		void operator()(int i) { m_mock(i); m_accu += i; }

		operator int() { return m_accu; }

		private:
			all_called_mock m_mock;
			int m_accu;
	};

//-----------------------------------------------------------------------------------------------------------------------------
UNITTESTDEF( break_behavior ) {
	using namespace RANGE_PROPOSAL_NAMESPACE;
	using std::placeholders::_1;

	TEST_init_hack(std::vector, int, exp, {1,2,3,4,5,6,7,8,9,10});

	// call on various implicit ranges
	for_each(std::bind(&iterate::generator_break_consumer_break, iterate(), _1), consumer_break(exp, 4));
	for_each(std::bind(&iterate::generator_break_consumer_nobreak, iterate(), _1), consumer_nobreak(exp, 5, false));
	for_each(std::bind(&iterate::generator_nobreak_consumer_nobreak, iterate(), _1), consumer_nobreak(exp, 6, false));

	// these two are undefined (Todo: break compile with nice msg)
	for_each(std::bind(&iterate::generator_nobreak_consumer_break_correct, iterate(), _1), all_called_mock(exp, 7));
	for_each(std::bind(&iterate::generator_nobreak_consumer_break_incorrect, iterate(), _1), all_called_mock(exp, 8, false));
}

}

