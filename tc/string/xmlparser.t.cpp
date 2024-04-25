
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../base/ref.h"
#include "../unittest.h"

#include "xmlparser.h"
#include "xmltransform.h"

struct ExErrorHandled final {};

UNITTESTDEF(xmlparser_error_handling) {
	{
		struct SErrorHandler final : decltype(tc::xml::assert_no_error) {
			void child_expected(tc::span<char const> strInput, char const* itch, tc::span<char const> strName) const& THROW(ExErrorHandled) {
				_ASSERT(tc::equal(strInput, "<a></a>"));
				_ASSERT(tc::equal(tc::drop(strInput, itch), "</a>"));
				_ASSERT(tc::equal(strName, "b"));
				throw ExErrorHandled();
			}
		};
		auto parser = tc::xml::make_parser("<a></a>", SErrorHandler());
		parser.expect_child("a");
		try {
			parser.expect_child("b");
			_ASSERTFALSE;
		} catch (ExErrorHandled const&) {
		}
	}
	{
		struct SErrorHandler final : decltype(tc::xml::assert_no_error) {
			void attribute_expected(tc::span<char const> strInput, char const* itch, tc::span<char const> strTag, tc::span<char const> strName) const& THROW(ExErrorHandled) {
				_ASSERT(tc::equal(tc::drop(strInput, itch), strInput));
				_ASSERT(tc::equal(strTag, "tag"));
				_ASSERT(tc::equal(strName, "missing"));
				throw ExErrorHandled();
			}
		};
		auto parser = tc::xml::make_parser("<tag name=\"value\"></tag>", SErrorHandler());
		parser.expect_child("tag");
		try {
			tc::discard(parser.expect_attribute("missing"));
			_ASSERTFALSE;
		} catch (ExErrorHandled const&) {
		}
	}
	{
		struct SErrorHandler final : decltype(tc::xml::assert_no_error) {
			void parse_error(tc::unused, tc::unused, tc::unused) const& THROW(ExErrorHandled) {
				throw ExErrorHandled();
			}
			void parse_error(tc::unused, tc::unused, tc::unused, tc::unused) const& THROW(ExErrorHandled) {
				throw ExErrorHandled();
			}
		};
		auto parser = tc::xml::make_parser("<a>one</a>", SErrorHandler());
		_ASSERT(parser.child("a"));
		try {
			tc::discard(parser.expect_parse_characters(tc::xml::integer<int>));
			_ASSERTFALSE;
		} catch (ExErrorHandled const&) {
		}
	}
	{
		struct SErrorHandler final : decltype(tc::xml::assert_no_error) {
			void parse_error(tc::unused, tc::unused, tc::unused, tc::unused) const& THROW(ExErrorHandled) {
				throw ExErrorHandled();
			}
		};
		auto parser = tc::xml::make_parser("<a number=\"one\"></a>", SErrorHandler());
		_ASSERT(parser.child("a"));
		try {
			tc::discard(parser.expect_parse_attribute(tc::xml::integer<int>, "number"));
			_ASSERTFALSE;
		} catch (ExErrorHandled const&) {
		}
	}
}

namespace {
	constexpr char c_strMsoNamespace[] = {"http://schemas.microsoft.com/office/2009/07/customui"};
}

UNITTESTDEF(namespace_) {
	tc_static_auto_constexpr_litstr(asz, R"(
<mso:customUI xmlns:mso="http://schemas.microsoft.com/office/2009/07/customui">
	<mso:ribbon>
		<mso:qat/>
		<mso:tabs/>
	</mso:ribbon>
</mso:customUI>
)");
	auto parser=tc::xml::make_parser(asz, tc::xml::assert_no_error);
	auto const nsMSO = parser.register_namespace(c_strMsoNamespace);
	_ASSERT(parser.register_namespace("http://schemas.microsoft.com/office/2009/07/CustomUI") != nsMSO);
	_ASSERT(parser.register_namespace("http://schemas.microsoft.com/office/2009/&#x30;&#x37;/customui") == nsMSO);
	_ASSERT(parser.register_namespace("http://schemas.microsoft.com/office/2009/&#48;&#55;/customui") == nsMSO);
	NOEXCEPT(parser.expect_child(nsMSO, "customUI"));
		NOEXCEPT(parser.expect_child(nsMSO, "ribbon"));
			_ASSERT(!parser.child("qat"));
			_ASSERT(!parser.child("mso:qat"));
			NOEXCEPT(parser.expect_child(nsMSO, "qat"));
			NOEXCEPT(parser.skip_rest_of_element());
			NOEXCEPT(parser.expect_child(nsMSO, "tabs"));
}

UNITTESTDEF(default_namespace) {
	tc_static_auto_constexpr_litstr(asz, R"(
<customUI xmlns="http://schemas.microsoft.com/office/2009/07/customui">
	<ribbon>
		<qat/>
		<tabs/>
	</ribbon>
</customUI>
)");
	
	auto parser=tc::xml::make_parser(asz, tc::xml::assert_no_error);
	auto const nsMSO = parser.register_namespace(c_strMsoNamespace);
	NOEXCEPT(parser.expect_child(nsMSO, "customUI"));
		NOEXCEPT(parser.expect_child(nsMSO, "ribbon"));
			_ASSERT(!parser.child("qat")); // default namespace has a namespace value, so it must match
			NOEXCEPT(parser.expect_child(nsMSO, "qat"));
			NOEXCEPT(parser.skip_rest_of_element());
			NOEXCEPT(parser.expect_child(nsMSO, "tabs"));
}

UNITTESTDEF(namespace_prefix_undeclared) {
	tc_static_auto_constexpr_litstr(asz, R"(
<customUI xmlns:mso="">
	<ribbon>
		<mso:qat/>
		<mso:tabs xmlns:mso="test"/>
		<mso:tabs/>
	</ribbon>
</customUI>
)");
	
	struct SErrorHandler final : decltype(tc::xml::assert_no_error) {
		void invalid_namespace_prefix(tc::unused, tc::unused) const& THROW(ExErrorHandled) {
			throw ExErrorHandled();
		}
	};
	
	auto parser=tc::xml::make_parser(asz, SErrorHandler());
	try {
		NOEXCEPT(parser.expect_child("customUI")); // default namespace has no value
		NOEXCEPT(parser.expect_child("ribbon"));
		parser.expect_child("qat"); // Prefixes must be declared and not have empty value: https://www.w3.org/TR/2006/REC-xml-names11-20060816/#nsc-NSDeclared
		_ASSERTFALSE;
	} catch(ExErrorHandled const&) {
	}
}

UNITTESTDEF(namespace_override) {
	tc_static_auto_constexpr_litstr(asz, R"(
<customUI xmlns:mso="">
	<ribbon>
		<qat/>
		<mso:tabs xmlns:mso="test"/>
		<mso:tabs/>
	</ribbon>
</customUI>
)");
	
	auto parser=tc::xml::make_parser(asz, tc::xml::assert_no_error);
	auto const nsTEST = parser.register_namespace("test");
	
	NOEXCEPT(parser.expect_child("customUI")); // default namespace has no value
	NOEXCEPT(parser.expect_child("ribbon"));
	NOEXCEPT(parser.expect_child("qat"));
	NOEXCEPT(parser.skip_rest_of_element());
	NOEXCEPT(parser.expect_child(nsTEST, "tabs"));
}

UNITTESTDEF(default_namespace_override) {
	tc_static_auto_constexpr_litstr(asz, R"(
<customUI xmlns="test">
	<ribbon>
		<qat/>
		<tabs xmlns=""/>
		<tabs/>
	</ribbon>
</customUI>
)");
	
	auto parser=tc::xml::make_parser(asz, tc::xml::assert_no_error);
	auto const nsTEST = parser.register_namespace("test");
	
	NOEXCEPT(parser.expect_child(nsTEST, "customUI")); // default namespace has a value
	NOEXCEPT(parser.expect_child(nsTEST, "ribbon"));
	_ASSERT(!parser.child("qat"));
	NOEXCEPT(parser.expect_child(nsTEST, "qat"));
	NOEXCEPT(parser.skip_rest_of_element());
	_ASSERT(!parser.child(nsTEST, "tabs"));
	NOEXCEPT(parser.expect_child("tabs"));
}

UNITTESTDEF(xml_transform_append) {
	tc_static_auto_constexpr_litstr(asz, R"(
<customUI xmlns:mso="">
	<ribbon>
		<qat></qat>
		<mso:tabs xmlns:mso="test"/>
		<tabs/>
	</ribbon>
</customUI>
)");
	tc::string<char> strOutput;
	{
		auto transform=tc::xml::make_transform(asz, tc::xml::assert_no_error, strOutput);
		auto const nsTEST = transform.register_namespace("test");
			
		NOEXCEPT(transform.expect_child("customUI"));
		NOEXCEPT(transform.expect_child("ribbon"));
		NOEXCEPT(transform.expect_child("qat"));

		NOEXCEPT(tc::append(
			transform,
			tc::xml::opening_element("test"),
			tc::xml::closing_element("test")
		));
		
		NOEXCEPT(transform.skip_rest_of_element());
		NOEXCEPT(transform.expect_child(nsTEST, "tabs"));

		auto const strPrefix = transform.prefix_for_namespace(nsTEST);
		NOEXCEPT(tc::append(
			transform,
			tc::xml::opening_element(strPrefix, "test"),
			tc::xml::closing_element(strPrefix, "test"),
			tc::xml::opening_element(strPrefix, "test2"),
			tc::xml::closing_element(strPrefix, "test2")
		));
		NOEXCEPT(transform.expect_element_end());
		NOEXCEPT(transform.expect_child("tabs"));
	}
	
	{
		auto parser=tc::xml::make_parser(strOutput, tc::xml::assert_no_error);
		auto const nsTEST = parser.register_namespace("test");
			
		NOEXCEPT(parser.expect_child("customUI"));
			NOEXCEPT(parser.expect_child("ribbon"));
			NOEXCEPT(parser.expect_child("qat"));
			NOEXCEPT(parser.expect_child("test"));
				NOEXCEPT(parser.expect_element_end());
			NOEXCEPT(parser.expect_element_end());
			  
		NOEXCEPT(parser.expect_child(nsTEST, "tabs"));
			NOEXCEPT(parser.expect_child(nsTEST, "test"));
			NOEXCEPT(parser.expect_element_end());
			NOEXCEPT(parser.expect_child(nsTEST, "test2"));
			NOEXCEPT(parser.expect_element_end());
		NOEXCEPT(parser.expect_element_end());
	}
}


UNITTESTDEF(xml_transform_insert_and_parse) {
	tc_static_auto_constexpr_litstr(asz, R"(
<customUI xmlns:mso="">
	<ribbon>
		<qat></qat>
		<mso:tabs xmlns:mso="test"/>
		<tabs/>
	</ribbon>
</customUI>
)");
	tc::string<char> strOutput;
	{
		auto transform=tc::xml::make_transform(asz, tc::xml::assert_no_error, strOutput);
		auto const nsTEST = transform.register_namespace("test");
			
		NOEXCEPT(transform.expect_child("customUI"));
		NOEXCEPT(transform.expect_child("ribbon"));
		{
			auto const e = NOEXCEPT(transform.insert_elements_after(
				tc::concat( 
					tc::xml::opening_element("test"),
					tc::xml::opening_element("test2")
				),
				tc::concat( 
					tc::xml::closing_element("test2"),
					tc::xml::closing_element("test")
				)
			));

			NOEXCEPT(transform.expect_child("qat"));
			NOEXCEPT(transform.skip_rest_of_element());
		}

		NOEXCEPT(transform.expect_child(nsTEST, "tabs"));

		{
			auto const e = NOEXCEPT(transform.insert_elements_before(
				tc::concat( 
					tc::xml::opening_element("test"),
					tc::xml::opening_element("test2")
				),
				tc::concat( 
					tc::xml::closing_element("test2"),
					tc::xml::closing_element("test")
				)
			));
			NOEXCEPT(transform.expect_element_end());
			NOEXCEPT(transform.expect_child("tabs"));
			NOEXCEPT(transform.drop_element());
		}
	}
	
	{
		auto parser=tc::xml::make_parser(strOutput, tc::xml::assert_no_error);
		auto const nsTEST = parser.register_namespace("test");
			
		NOEXCEPT(parser.expect_child("customUI"));
			NOEXCEPT(parser.expect_child("ribbon"));
				NOEXCEPT(parser.expect_child("test"));
					NOEXCEPT(parser.expect_child("test2"));
						NOEXCEPT(parser.expect_child("qat"));
						NOEXCEPT(parser.expect_element_end());
					NOEXCEPT(parser.expect_element_end());
				NOEXCEPT(parser.expect_element_end());

				
				NOEXCEPT(parser.expect_child("test"));
					NOEXCEPT(parser.expect_child("test2"));
						NOEXCEPT(parser.expect_child(nsTEST, "tabs"));
						NOEXCEPT(parser.expect_element_end());
					NOEXCEPT(parser.expect_element_end());
				NOEXCEPT(parser.expect_element_end());
			NOEXCEPT(parser.expect_element_end());
		NOEXCEPT(parser.expect_element_end());
	}
}

UNITTESTDEF(xml_transform_modify_attributes) {
	tc_static_auto_constexpr_litstr(asz, R"(
<customUI>
	<ribbon foo="1" bar="something"/>
</customUI>
)");
	tc::string<char> strOutput;
	{
		auto transform=tc::xml::make_transform(asz, tc::xml::assert_no_error, strOutput);
		NOEXCEPT(transform.expect_child("customUI"));
		NOEXCEPT(transform.expect_child("ribbon"));
		NOEXCEPT(transform.modify_element_attributes(
			tc::xml::format_attributes(
				tc::filter(
					transform.attributes(),
					[](auto const& strKey, auto const& strValue) noexcept {
						return !tc::equal(strKey, "foo");
					}
				)
			),
			" foo=\"2\""
		));
	}
	
	{
		auto parser=tc::xml::make_parser(strOutput, tc::xml::assert_no_error);
			
		NOEXCEPT(parser.expect_child("customUI"));
		NOEXCEPT(parser.expect_child("ribbon"));
		_ASSERT(tc::equal(*parser.attribute("bar"), "something"));
		_ASSERT(tc::equal(*parser.attribute("foo"), "2"));
	}
}

UNITTESTDEF(xml_transform_insert_empty_child_before) {
	tc_static_auto_constexpr_litstr(asz, R"(
<customUI xmlns:mso="test">
	<ribbon/>
</customUI>
)");
	tc::string<char> strOutput;
	{
		auto transform=tc::xml::make_transform(asz, tc::xml::assert_no_error, strOutput);
		auto const nsTEST = transform.register_namespace("test");

		NOEXCEPT(transform.expect_child("customUI"));
		// lookup strPrefix here because we are doing insert_before()!
		// <ribbon> may change the prefix of namespace test
		auto const strPrefix = transform.prefix_for_namespace(nsTEST);

		NOEXCEPT(transform.expect_child("ribbon"));
		NOEXCEPT(tc::append(
			transform.insert_before(),
			tc::xml::empty_element(
				strPrefix,
				"element",
				" foo=\"bar\""
			)
		));
	}
	
	{
		auto parser=tc::xml::make_parser(strOutput, tc::xml::assert_no_error);
		auto const nsTEST = parser.register_namespace("test");
		NOEXCEPT(parser.expect_child("customUI"));
			NOEXCEPT(parser.expect_child(nsTEST, "element"));
			_ASSERT(tc::equal(*parser.attribute("foo"), "bar"));
			NOEXCEPT(parser.expect_element_end());

			NOEXCEPT(parser.expect_child("ribbon"));
			NOEXCEPT(parser.expect_element_end());
		NOEXCEPT(parser.expect_element_end());
	}
}


UNITTESTDEF(xml_transform_insert_before2) {
	tc_static_auto_constexpr_litstr(asz, R"(
<customUI xmlns:mso="test">
	<ribbon/>
</customUI>
)");
	tc::string<char> strOutput;
	{
		auto transform=tc::xml::make_transform(asz, tc::xml::assert_no_error, strOutput);
		auto ons = transform.register_namespace("test");
		NOEXCEPT(transform.expect_child("customUI"));
		NOEXCEPT(transform.expect_child("ribbon"));
		NOEXCEPT(transform.expect_element_end());

		auto const strPrefix = transform.prefix_for_namespace(ons);
		NOEXCEPT(tc::append(
			transform.insert_before(),
			tc::xml::opening_element(strPrefix, "element", " foo=\"bar\""),
			tc::xml::opening_element(strPrefix, "element2", " foo=\"bar\""),
			tc::xml::closing_element(strPrefix, "element2"),
			tc::xml::closing_element(strPrefix, "element")
		));
	}
	
	{
		auto parser=tc::xml::make_parser(strOutput, tc::xml::assert_no_error);
		auto ons = parser.register_namespace("test");
		NOEXCEPT(parser.expect_child("customUI"));
			NOEXCEPT(parser.expect_child("ribbon"));
				NOEXCEPT(parser.expect_child(ons, "element"));
					NOEXCEPT(parser.expect_child(ons, "element2"));
					NOEXCEPT(parser.expect_element_end());
				NOEXCEPT(parser.expect_element_end());
			NOEXCEPT(parser.expect_element_end());
		NOEXCEPT(parser.expect_element_end());
	}
}

UNITTESTDEF(xml_transform_insert_elements_before) {
	tc_static_auto_constexpr_litstr(asz, R"(
<customUI xmlns:mso="test">
	<ribbon/>
</customUI>
)");
	tc::string<char> strOutput;
	{
		auto transform=tc::xml::make_transform(asz, tc::xml::assert_no_error, strOutput);
		auto ons = transform.register_namespace("test");
		NOEXCEPT(transform.expect_child("customUI"));

		auto const strPrefix = transform.prefix_for_namespace(ons);
		NOEXCEPT(transform.expect_child("ribbon"));

		auto b = NOEXCEPT(transform.insert_elements_before(
			tc::concat(
				tc::xml::opening_element(strPrefix, "element", " foo=\"bar\""),
				tc::xml::opening_element(strPrefix, "element2", " foo=\"bar\"")
			),
			tc::concat(
				tc::xml::closing_element(strPrefix, "element2"),
				tc::xml::closing_element(strPrefix, "element")
			)
		));

		NOEXCEPT(transform.expect_element_end());
	}
	
	{
		auto parser=tc::xml::make_parser(strOutput, tc::xml::assert_no_error);
		auto ons = parser.register_namespace("test");
		NOEXCEPT(parser.expect_child("customUI"));
			NOEXCEPT(parser.expect_child(ons, "element"));
				NOEXCEPT(parser.expect_child(ons, "element2"));
					NOEXCEPT(parser.expect_child("ribbon"));
					NOEXCEPT(parser.expect_element_end());
				NOEXCEPT(parser.expect_element_end());
			NOEXCEPT(parser.expect_element_end());
		NOEXCEPT(parser.expect_element_end());
	}
}


UNITTESTDEF(xml_transform_insert_child_before3) {
	tc_static_auto_constexpr_litstr(asz, R"(
<customUI xmlns:mso="test">
	<ribbon/>
</customUI>
)");
	tc::string<char> strOutput;
	{
		auto transform=tc::xml::make_transform(asz, tc::xml::assert_no_error, strOutput);
		auto ons = transform.register_namespace("test");
		NOEXCEPT(transform.expect_child("customUI"));
		NOEXCEPT(transform.expect_child("ribbon"));

		auto const strPrefix = transform.prefix_for_namespace(ons);
		if(!NOEXCEPT(transform.child("element"))) {
			NOEXCEPT(tc::append(
				transform.insert_before(),
				tc::xml::opening_element(strPrefix, "element", " foo=\"bar\""),
				tc::xml::opening_element(strPrefix, "element2", " foo=\"bar\""),
				tc::xml::closing_element(strPrefix, "element2"),
				tc::xml::closing_element(strPrefix, "element")
			));
		}

		NOEXCEPT(transform.expect_element_end());
		NOEXCEPT(transform.expect_element_end());
	}
	
	{
		auto parser=tc::xml::make_parser(strOutput, tc::xml::assert_no_error);
		auto ons = parser.register_namespace("test");
		NOEXCEPT(parser.expect_child("customUI"));
			NOEXCEPT(parser.expect_child("ribbon"));
				NOEXCEPT(parser.expect_child(ons, "element"));
					NOEXCEPT(parser.expect_child(ons, "element2"));
					NOEXCEPT(parser.expect_element_end());
				NOEXCEPT(parser.expect_element_end());
			NOEXCEPT(parser.expect_element_end());
	}
}

UNITTESTDEF(xml_transform_drop_element) {
	tc_static_auto_constexpr_litstr(asz, R"(
<customUI>
	<ribbon>
		<qat></qat>
		<mso:tabs xmlns:mso="test"/>
		<mso:tabs/>
	</ribbon>
</customUI>
)");
	tc::string<char> strOutput;
	{
		auto transform=tc::xml::make_transform(asz, tc::xml::assert_no_error, strOutput);
		NOEXCEPT(transform.expect_child("customUI"));
		NOEXCEPT(transform.expect_child("ribbon"));
		NOEXCEPT(transform.drop_element());
	}
	
	{
		auto parser=tc::xml::make_parser(strOutput, tc::xml::assert_no_error);
		NOEXCEPT(parser.expect_child("customUI"));
		NOEXCEPT(parser.expect_element_end());
	}
}

namespace
{
	// MSVC doesn't like it at function scope for some reason.
	auto constexpr MatchOrInsertTest = [](tc::span<char const> strXml) noexcept {
		tc::string<char> strOutput;
		{
			auto transform=tc::xml::make_transform(strXml, tc::xml::assert_no_error, strOutput);
			NOEXCEPT(transform.expect_child("customUI"));
			auto on1 = NOEXCEPT(transform.match_or_insert_child("ribbon"));
			auto on2 = NOEXCEPT(transform.match_or_insert_child("tabs"));
		}
	
		{
			auto parser=tc::xml::make_parser(strOutput, tc::xml::assert_no_error);
			NOEXCEPT(parser.expect_child("customUI"));
				NOEXCEPT(parser.expect_child("ribbon"));
					NOEXCEPT(parser.expect_child("tabs"));
					NOEXCEPT(parser.expect_element_end());
				NOEXCEPT(parser.expect_element_end());
			NOEXCEPT(parser.expect_element_end());
		}
	};
}

UNITTESTDEF(xml_transform_match_or_insert_child) {
	MatchOrInsertTest(R"(
		<customUI>    
		</customUI>
	)");
	
	MatchOrInsertTest(R"(
		<customUI>
			<ribbon/>
		</customUI>
	)");
	
	MatchOrInsertTest(R"(
		<customUI>
			<ribbon></ribbon>
		</customUI>
	)");
	
	MatchOrInsertTest(R"(
		<customUI>
			<ribbon>
				<tabs/>
			</ribbon>
		</customUI>
	)");

	MatchOrInsertTest(R"(
		<customUI>
			<ribbon>
				<tabs>
				</tabs>
			</ribbon>
		</customUI>
	)");
}
