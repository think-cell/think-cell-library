/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman
	Copyright (c) 2001-2011 Hartmut Kaiser

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"
#include <boost/fusion/include/std_pair.hpp>

#include <map>
#include <unordered_map>
#include <boost/unordered_map.hpp>
#include <vector>
#include <list>
#include <deque>
#include <set>
#include <unordered_set>
#include <boost/unordered_set.hpp>
#include <string>

namespace x3 = boost::spirit::x3;
using boost::spirit::x3::lit;


// check if we did not break user defined specializations
namespace check_substitute {
template <typename T> struct foo {};
template <typename T> struct bar { using type = T; };
template <typename T> struct is_bar : std::false_type {};
template <typename T> struct is_bar<bar<T>> : std::true_type {};
}

namespace boost { namespace spirit { namespace x3 { namespace traits {
using namespace check_substitute;

template <typename T, typename U>
struct is_substitute<foo<T>, foo<U>> : is_substitute<T, U> {};

template <typename T, typename U>
struct is_substitute<T, U, std::enable_if_t<is_bar<T>::value && is_bar<U>::value>>
  : is_substitute<typename T::type, typename U::type> {};
}}}}

namespace check_substitute {
using x3::traits::is_substitute;
static_assert(is_substitute<foo<int>, foo<int>>::value, "is_substitute problem");
static_assert(!is_substitute<foo<int>, foo<long>>::value, "is_substitute problem");
static_assert(is_substitute<bar<int>, bar<int>>::value, "is_substitute problem");
static_assert(!is_substitute<bar<int>, bar<long>>::value, "is_substitute problem");
}


x3::rule<class pair_rule, std::pair<std::string,std::string>> const pair_rule("pair");
x3::rule<class string_rule, std::string> const string_rule("string");

auto const pair_rule_def = string_rule > x3::lit('=') > string_rule;
auto const string_rule_def = x3::lexeme[*x3::alnum];

BOOST_SPIRIT_DEFINE(pair_rule, string_rule)

template <typename Container>
void test_map_support()
{
	using spirit_test::test_attr;

	Container container;
	Container const compare {{"k1", "v1"}, {"k2", "v2"}};
	auto const rule = pair_rule % x3::lit(',');

	_ASSERT(test_attr("k1=v1,k2=v2,k2=v3", rule, container));
	_ASSERT(container.size() == 2);
	_ASSERT(container == compare);

	// test sequences parsing into containers
	auto const seq_rule = pair_rule >> lit(',') >> pair_rule >> lit(',') >> pair_rule;
	container.clear();
	_ASSERT(test_attr("k1=v1,k2=v2,k2=v3", seq_rule, container));

	// test parsing container into container
	auto const cic_rule = pair_rule >> +(lit(',') >> pair_rule);
	container.clear();
	_ASSERT(test_attr("k1=v1,k2=v2,k2=v3", cic_rule, container));
}

template <typename Container>
void test_multimap_support()
{
	using spirit_test::test_attr;

	Container container;
	Container const compare {{"k1", "v1"}, {"k2", "v2"}, {"k2", "v3"}};
	auto const rule = pair_rule % x3::lit(',');

	_ASSERT(test_attr("k1=v1,k2=v2,k2=v3", rule, container));
	_ASSERT(container.size() == 3);
	_ASSERT(container == compare);

	// test sequences parsing into containers
	auto const seq_rule = pair_rule >> lit(',') >> pair_rule >> lit(',') >> pair_rule;
	container.clear();
	_ASSERT(test_attr("k1=v1,k2=v2,k2=v3", seq_rule, container));

	// test parsing container into container
	auto const cic_rule = pair_rule >> +(lit(',') >> pair_rule);
	container.clear();
	_ASSERT(test_attr("k1=v1,k2=v2,k2=v3", cic_rule, container));
}

template <typename Container>
void test_sequence_support()
{
	using spirit_test::test_attr;

	Container container;
	Container const compare {"e1", "e2", "e2"};
	auto const rule = string_rule % x3::lit(',');

	_ASSERT(test_attr("e1,e2,e2", rule, container));
	_ASSERT(container.size() == 3);
	_ASSERT(container == compare);

	// test sequences parsing into containers
	auto const seq_rule = string_rule >> lit(',') >> string_rule >> lit(',') >> string_rule;
	container.clear();
	_ASSERT(test_attr("e1,e2,e2", seq_rule, container));

	// test parsing container into container
	auto const cic_rule = string_rule >> +(lit(',') >> string_rule);
	container.clear();
	_ASSERT(test_attr("e1,e2,e2", cic_rule, container));
}

template <typename Container>
void test_set_support()
{
	using spirit_test::test_attr;

	Container container;
	Container const compare {"e1", "e2"};
	auto const rule = string_rule % x3::lit(',');

	_ASSERT(test_attr("e1,e2,e2", rule, container));
	_ASSERT(container.size() == 2);
	_ASSERT(container == compare);

	// test sequences parsing into containers
	auto const seq_rule = string_rule >> lit(',') >> string_rule >> lit(',') >> string_rule;
	container.clear();
	_ASSERT(test_attr("e1,e2,e2", seq_rule, container));

	// test parsing container into container
	auto const cic_rule = string_rule >> +(lit(',') >> string_rule);
	container.clear();
	_ASSERT(test_attr("e1,e2,e2", cic_rule, container));
}

template <typename Container>
void test_multiset_support()
{
	using spirit_test::test_attr;

	Container container;
	Container const compare {"e1", "e2", "e2"};
	auto const rule = string_rule % x3::lit(',');

	_ASSERT(test_attr("e1,e2,e2", rule, container));
	_ASSERT(container.size() == 3);
	_ASSERT(container == compare);

	// test sequences parsing into containers
	auto const seq_rule = string_rule >> lit(',') >> string_rule >> lit(',') >> string_rule;
	container.clear();
	_ASSERT(test_attr("e1,e2,e2", seq_rule, container));

	// test parsing container into container
	auto const cic_rule = string_rule >> +(lit(',') >> string_rule);
	container.clear();
	_ASSERT(test_attr("e1,e2,e2", cic_rule, container));
}

template <typename Container>
void test_string_support()
{
	using spirit_test::test_attr;

	Container container;
	Container const compare {"e1e2e2"};
	auto const rule = string_rule % x3::lit(',');

	_ASSERT(test_attr("e1,e2,e2", rule, container));
	_ASSERT(container.size() == 6);
	_ASSERT(container == compare);

	// test sequences parsing into containers
	auto const seq_rule = string_rule >> lit(',') >> string_rule >> lit(',') >> string_rule;
	container.clear();
	_ASSERT(test_attr("e1,e2,e2", seq_rule, container));

	// test parsing container into container
	auto const cic_rule = string_rule >> +(lit(',') >> string_rule);
	container.clear();
	_ASSERT(test_attr("e1,e2,e2", cic_rule, container));
}

UNITTESTDEF(x3_test_container_support)
{
	using x3::traits::is_associative;

	// ------------------------------------------------------------------

	static_assert(is_associative<std::set<int>>::value, "is_associative problem");
	static_assert(is_associative<std::unordered_set<int>>::value, "is_associative problem");
	static_assert(is_associative<boost::unordered_set<int>>::value, "is_associative problem");
	static_assert(is_associative<std::multiset<int>>::value, "is_associative problem");
	static_assert(is_associative<std::unordered_multiset<int>>::value, "is_associative problem");
	static_assert(is_associative<boost::unordered_multiset<int>>::value, "is_associative problem");
	static_assert(is_associative<std::map<int,int>>::value, "is_associative problem");
	static_assert(is_associative<std::unordered_map<int,int>>::value, "is_associative problem");
	static_assert(is_associative<boost::unordered_map<int,int>>::value, "is_associative problem");
	static_assert(is_associative<std::multimap<int,int>>::value, "is_associative problem");
	static_assert(is_associative<std::unordered_multimap<int,int>>::value, "is_associative problem");
	static_assert(is_associative<boost::unordered_multimap<int,int>>::value, "is_associative problem");

	static_assert(!is_associative<std::vector<int>>::value, "is_associative problem");
	static_assert(!is_associative<std::string>::value, "is_associative problem");
	static_assert(!is_associative<std::deque<int>>::value, "is_associative problem");
	static_assert(!is_associative<std::list<int>>::value, "is_associative problem");

	// ------------------------------------------------------------------

	test_string_support<std::string>();

	test_sequence_support<std::vector<std::string>>();
	test_sequence_support<std::list<std::string>>();
	test_sequence_support<std::deque<std::string>>();

	test_set_support<std::set<std::string>>();
	test_set_support<std::unordered_set<std::string>>();
	test_set_support<boost::unordered_set<std::string>>();

	test_multiset_support<std::multiset<std::string>>();
	test_multiset_support<std::unordered_multiset<std::string>>();
	test_multiset_support<boost::unordered_multiset<std::string>>();

	test_map_support<std::map<std::string,std::string>>();
	test_map_support<std::unordered_map<std::string,std::string>>();
	test_map_support<boost::unordered_map<std::string,std::string>>();

	test_multimap_support<std::multimap<std::string,std::string>>();
	test_multimap_support<std::unordered_multimap<std::string,std::string>>();
	test_multimap_support<boost::unordered_multimap<std::string,std::string>>();
}
