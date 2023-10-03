
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../unittest.h"
#include "jsonparser.h"
#include "../range/join_adaptor.h"
#include "../range/repeat_n.h"

#pragma push_macro("AS_ARRAY")
#define AS_ARRAY(str) tc::end_prev<tc::return_take>(tc::as_array(str))

namespace {
	bool Accepts(auto const& str) noexcept {
		struct ExFailure final {};
		bool bAccepted = true;
		try {
			tc::json::parser parser(
				str,
				tc::json::simple_error_handler([](tc::unused) THROW(ExFailure) {
					throw ExFailure();
				})
			);
			parser.skip_value(); // THROW(ExFailure)
			parser.expect_end(); // THROW(ExFailure)
		} catch (ExFailure const&) {
			bAccepted = false;
		}
		return bAccepted;
	};
}

UNITTESTDEF(JSONTestSuite) {
	// Create from https://github.com/nst/JSONTestSuite with the following code.
	//
	//	tc::temporaryfile tmpfile; // THROW(tc::file_failure)
	//	tc::for_each(
	//		tc::filesystem::file_range(FILESTR("/git/JSONTestSuite/test_parsing/")),
	//		[&](auto const& pathTestCase) noexcept {
	//			tc::readfile file(tc::as_c_str(pathTestCase)); // THROW(tc::file_failure)
	//			auto str = tc::make_str(tc::repeat_n(tc::size(file), '0'));
	//			tc::read(file, tc::ptr_begin(str), tc::size(str)); // THROW(tc::file_failure)
	//			tc_auto_cref(strFilename, FilenameWithoutPath<tc::return_drop>(pathTestCase));
	//			tc_auto_cref(chExpectation, tc::front(strFilename));
	//			tc::append(
	//				tc::make_typed_stream<char>(tmpfile),
	//				tc_conditional_range(
	//					FILESTR('i') != chExpectation,
	//					tc::concat(
	//						"_ASSERT(",
	//						tc_conditional_range(
	//							FILESTR('n') == chExpectation,
	//							"!"
	//						)
	//					)
	//				),
	//				"Accepts(AS_ARRAY(\"",
	//					tc_conditional_range(
	//						tc::all_of(tc::codepoint_range(str), tc_fn(tc::codepoint_value)),
	//						tc::as_cppstr(str),
	//						tc::join(tc::transform(str, [](char const ch) noexcept {
	//							return tc::concat("\\x", tc::as_padded_hex(tc::to_underlying(ch)));
	//						}))
	//					),
	//				"\"))",
	//				tc_conditional_range(
	//					FILESTR('i') != chExpectation,
	//					")"
	//				),
	//				"; // ", FileExtension<tc::return_take>(strFilename), "\n"
	//			);
	//		}
	//	);
	//	_ASSERTKNOWNFALSEPRINT("Wrote asserts to ", tmpfile.flush_and_close());
	Accepts(AS_ARRAY("[123.456e-789]")); // i_number_double_huge_neg_exp
	Accepts(AS_ARRAY("[0.4e00669999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999969999999006]")); // i_number_huge_exp
	Accepts(AS_ARRAY("[-1e+9999]")); // i_number_neg_int_huge_exp
	Accepts(AS_ARRAY("[1.5e+9999]")); // i_number_pos_double_huge_exp
	Accepts(AS_ARRAY("[-123123e100000]")); // i_number_real_neg_overflow
	Accepts(AS_ARRAY("[123123e100000]")); // i_number_real_pos_overflow
	Accepts(AS_ARRAY("[123e-10000000]")); // i_number_real_underflow
	Accepts(AS_ARRAY("[-123123123123123123123123123123]")); // i_number_too_big_neg_int
	Accepts(AS_ARRAY("[100000000000000000000]")); // i_number_too_big_pos_int
	Accepts(AS_ARRAY("[-237462374673276894279832749832423479823246327846]")); // i_number_very_big_negative_int
	Accepts(AS_ARRAY("{\"\\uDFAA\":0}")); // i_object_key_lone_2nd_surrogate
	Accepts(AS_ARRAY("[\"\\uDADA\"]")); // i_string_1st_surrogate_but_2nd_missing
	Accepts(AS_ARRAY("[\"\\uD888\\u1234\"]")); // i_string_1st_valid_surrogate_2nd_invalid
	Accepts(AS_ARRAY("[\"\\uD800\\uD800\\n\"]")); // i_string_incomplete_surrogates_escape_valid
	Accepts(AS_ARRAY("[\"\\uD800\\n\"]")); // i_string_incomplete_surrogate_and_escape_valid
	Accepts(AS_ARRAY("[\"\\uDd1ea\"]")); // i_string_incomplete_surrogate_pair
	Accepts(AS_ARRAY("[\"\\ud800\"]")); // i_string_invalid_lonely_surrogate
	Accepts(AS_ARRAY("[\"\\ud800abc\"]")); // i_string_invalid_surrogate
	Accepts(AS_ARRAY("\x5B\x22\xFF\x22\x5D")); // i_string_invalid_utf-8
	Accepts(AS_ARRAY("[\"\\uDd1e\\uD834\"]")); // i_string_inverted_surrogates_U+1D11E
	Accepts(AS_ARRAY("\x5B\x22\xE9\x22\x5D")); // i_string_iso_latin_1
	Accepts(AS_ARRAY("[\"\\uDFAA\"]")); // i_string_lone_second_surrogate
	Accepts(AS_ARRAY("\x5B\x22\x81\x22\x5D")); // i_string_lone_utf8_continuation_byte
	Accepts(AS_ARRAY("\x5B\x22\xF4\xBF\xBF\xBF\x22\x5D")); // i_string_not_in_unicode_range
	Accepts(AS_ARRAY("\x5B\x22\xC0\xAF\x22\x5D")); // i_string_overlong_sequence_2_bytes
	Accepts(AS_ARRAY("\x5B\x22\xFC\x83\xBF\xBF\xBF\xBF\x22\x5D")); // i_string_overlong_sequence_6_bytes
	Accepts(AS_ARRAY("\x5B\x22\xFC\x80\x80\x80\x80\x80\x22\x5D")); // i_string_overlong_sequence_6_bytes_null
	Accepts(AS_ARRAY("\x5B\x22\xE0\xFF\x22\x5D")); // i_string_truncated-utf-8
	Accepts(AS_ARRAY("\xFF\xFE\x5B\x00\x22\x00\xE9\x00\x22\x00\x5D\x00")); // i_string_UTF-16LE_with_BOM
	Accepts(AS_ARRAY("\x5B\x22\xE6\x97\xA5\xD1\x88\xFA\x22\x5D")); // i_string_UTF-8_invalid_sequence
	Accepts(AS_ARRAY("\x00\x5B\x00\x22\x00\xE9\x00\x22\x00\x5D")); // i_string_utf16BE_no_BOM
	Accepts(AS_ARRAY("\x5B\x00\x22\x00\xE9\x00\x22\x00\x5D\x00")); // i_string_utf16LE_no_BOM
	Accepts(AS_ARRAY("\x5B\x22\xED\xA0\x80\x22\x5D")); // i_string_UTF8_surrogate_U+D800
	Accepts(AS_ARRAY("\uFEFF{}")); // i_structure_UTF-8_BOM_empty_object
	_ASSERT(!Accepts(AS_ARRAY("[1 true]"))); // n_array_1_true_without_comma
	_ASSERT(!Accepts(AS_ARRAY("\x5B\x61\xE5\x5D"))); // n_array_a_invalid_utf8
	_ASSERT(!Accepts(AS_ARRAY("[\"\": 1]"))); // n_array_colon_instead_of_comma
	_ASSERT(!Accepts(AS_ARRAY("[\"\"],"))); // n_array_comma_after_close
	_ASSERT(!Accepts(AS_ARRAY("[,1]"))); // n_array_comma_and_number
	_ASSERT(!Accepts(AS_ARRAY("[1,,2]"))); // n_array_double_comma
	_ASSERT(!Accepts(AS_ARRAY("[\"x\",,]"))); // n_array_double_extra_comma
	_ASSERT(!Accepts(AS_ARRAY("[\"x\"]]"))); // n_array_extra_close
	_ASSERT(!Accepts(AS_ARRAY("[\"\",]"))); // n_array_extra_comma
	_ASSERT(!Accepts(AS_ARRAY("[\"x\""))); // n_array_incomplete
	_ASSERT(!Accepts(AS_ARRAY("[x"))); // n_array_incomplete_invalid_value
	_ASSERT(!Accepts(AS_ARRAY("[3[4]]"))); // n_array_inner_array_no_comma
	_ASSERT(!Accepts(AS_ARRAY("\x5B\xFF\x5D"))); // n_array_invalid_utf8
	_ASSERT(!Accepts(AS_ARRAY("[1:2]"))); // n_array_items_separated_by_semicolon
	_ASSERT(!Accepts(AS_ARRAY("[,]"))); // n_array_just_comma
	_ASSERT(!Accepts(AS_ARRAY("[-]"))); // n_array_just_minus
	_ASSERT(!Accepts(AS_ARRAY("[   , \"\"]"))); // n_array_missing_value
	_ASSERT(!Accepts(AS_ARRAY("[\"a\",\n4\n,1,"))); // n_array_newlines_unclosed
	_ASSERT(!Accepts(AS_ARRAY("[1,]"))); // n_array_number_and_comma
	_ASSERT(!Accepts(AS_ARRAY("[1,,]"))); // n_array_number_and_several_commas
	_ASSERT(!Accepts(AS_ARRAY("[\"\u000Ba\"\\f]"))); // n_array_spaces_vertical_tab_formfeed
	_ASSERT(!Accepts(AS_ARRAY("[*]"))); // n_array_star_inside
	_ASSERT(!Accepts(AS_ARRAY("[\"\""))); // n_array_unclosed
	_ASSERT(!Accepts(AS_ARRAY("[1,"))); // n_array_unclosed_trailing_comma
	_ASSERT(!Accepts(AS_ARRAY("[1,\n1\n,1"))); // n_array_unclosed_with_new_lines
	_ASSERT(!Accepts(AS_ARRAY("[{}"))); // n_array_unclosed_with_object_inside
	_ASSERT(!Accepts(AS_ARRAY("[fals]"))); // n_incomplete_false
	_ASSERT(!Accepts(AS_ARRAY("[nul]"))); // n_incomplete_null
	_ASSERT(!Accepts(AS_ARRAY("[tru]"))); // n_incomplete_true
	_ASSERT(!Accepts(AS_ARRAY("123\u0000"))); // n_multidigit_number_then_00
	_ASSERT(!Accepts(AS_ARRAY("[++1234]"))); // n_number_++
	_ASSERT(!Accepts(AS_ARRAY("[+1]"))); // n_number_+1
	_ASSERT(!Accepts(AS_ARRAY("[+Inf]"))); // n_number_+Inf
	_ASSERT(!Accepts(AS_ARRAY("[-01]"))); // n_number_-01
	_ASSERT(!Accepts(AS_ARRAY("[-1.0.]"))); // n_number_-1.0.
	_ASSERT(!Accepts(AS_ARRAY("[-2.]"))); // n_number_-2.
	_ASSERT(!Accepts(AS_ARRAY("[-NaN]"))); // n_number_-NaN
	_ASSERT(!Accepts(AS_ARRAY("[.-1]"))); // n_number_.-1
	_ASSERT(!Accepts(AS_ARRAY("[.2e-3]"))); // n_number_.2e-3
	_ASSERT(!Accepts(AS_ARRAY("[0.1.2]"))); // n_number_0.1.2
	_ASSERT(!Accepts(AS_ARRAY("[0.3e+]"))); // n_number_0.3e+
	_ASSERT(!Accepts(AS_ARRAY("[0.3e]"))); // n_number_0.3e
	_ASSERT(!Accepts(AS_ARRAY("[0.e1]"))); // n_number_0.e1
	_ASSERT(!Accepts(AS_ARRAY("[0e+]"))); // n_number_0e+
	_ASSERT(!Accepts(AS_ARRAY("[0e]"))); // n_number_0e
	_ASSERT(!Accepts(AS_ARRAY("[0E+]"))); // n_number_0_capital_E+
	_ASSERT(!Accepts(AS_ARRAY("[0E]"))); // n_number_0_capital_E
	_ASSERT(!Accepts(AS_ARRAY("[1.0e+]"))); // n_number_1.0e+
	_ASSERT(!Accepts(AS_ARRAY("[1.0e-]"))); // n_number_1.0e-
	_ASSERT(!Accepts(AS_ARRAY("[1.0e]"))); // n_number_1.0e
	_ASSERT(!Accepts(AS_ARRAY("[1eE2]"))); // n_number_1eE2
	_ASSERT(!Accepts(AS_ARRAY("[1 000.0]"))); // n_number_1_000
	_ASSERT(!Accepts(AS_ARRAY("[2.e+3]"))); // n_number_2.e+3
	_ASSERT(!Accepts(AS_ARRAY("[2.e-3]"))); // n_number_2.e-3
	_ASSERT(!Accepts(AS_ARRAY("[2.e3]"))); // n_number_2.e3
	_ASSERT(!Accepts(AS_ARRAY("[9.e+]"))); // n_number_9.e+
	_ASSERT(!Accepts(AS_ARRAY("[1+2]"))); // n_number_expression
	_ASSERT(!Accepts(AS_ARRAY("[0x1]"))); // n_number_hex_1_digit
	_ASSERT(!Accepts(AS_ARRAY("[0x42]"))); // n_number_hex_2_digits
	_ASSERT(!Accepts(AS_ARRAY("[Inf]"))); // n_number_Inf
	_ASSERT(!Accepts(AS_ARRAY("[Infinity]"))); // n_number_infinity
	_ASSERT(!Accepts(AS_ARRAY("[0e+-1]"))); // n_number_invalid+-
	_ASSERT(!Accepts(AS_ARRAY("[-123.123foo]"))); // n_number_invalid-negative-real
	_ASSERT(!Accepts(AS_ARRAY("\x5B\x31\x32\x33\xE5\x5D"))); // n_number_invalid-utf-8-in-bigger-int
	_ASSERT(!Accepts(AS_ARRAY("\x5B\x31\x65\x31\xE5\x5D"))); // n_number_invalid-utf-8-in-exponent
	_ASSERT(!Accepts(AS_ARRAY("\x5B\x30\xE5\x5D\x0A"))); // n_number_invalid-utf-8-in-int
	_ASSERT(!Accepts(AS_ARRAY("[-Infinity]"))); // n_number_minus_infinity
	_ASSERT(!Accepts(AS_ARRAY("[-foo]"))); // n_number_minus_sign_with_trailing_garbage
	_ASSERT(!Accepts(AS_ARRAY("[- 1]"))); // n_number_minus_space_1
	_ASSERT(!Accepts(AS_ARRAY("[NaN]"))); // n_number_NaN
	_ASSERT(!Accepts(AS_ARRAY("[-012]"))); // n_number_neg_int_starting_with_zero
	_ASSERT(!Accepts(AS_ARRAY("[-.123]"))); // n_number_neg_real_without_int_part
	_ASSERT(!Accepts(AS_ARRAY("[-1x]"))); // n_number_neg_with_garbage_at_end
	_ASSERT(!Accepts(AS_ARRAY("[1ea]"))); // n_number_real_garbage_after_e
	_ASSERT(!Accepts(AS_ARRAY("[1.]"))); // n_number_real_without_fractional_part
	_ASSERT(!Accepts(AS_ARRAY("\x5B\x31\x65\xE5\x5D"))); // n_number_real_with_invalid_utf8_after_e
	_ASSERT(!Accepts(AS_ARRAY("[.123]"))); // n_number_starting_with_dot
	_ASSERT(!Accepts(AS_ARRAY("[\uFF11]"))); // n_number_U+FF11_fullwidth_digit_one
	_ASSERT(!Accepts(AS_ARRAY("[1.2a-3]"))); // n_number_with_alpha
	_ASSERT(!Accepts(AS_ARRAY("[1.8011670033376514H-308]"))); // n_number_with_alpha_char
	_ASSERT(!Accepts(AS_ARRAY("[012]"))); // n_number_with_leading_zero
	_ASSERT(!Accepts(AS_ARRAY("[\"x\", truth]"))); // n_object_bad_value
	_ASSERT(!Accepts(AS_ARRAY("{[: \"x\"}\n"))); // n_object_bracket_key
	_ASSERT(!Accepts(AS_ARRAY("{\"x\", null}"))); // n_object_comma_instead_of_colon
	_ASSERT(!Accepts(AS_ARRAY("{\"x\"::\"b\"}"))); // n_object_double_colon
	_ASSERT(!Accepts(AS_ARRAY("{\U0001F1E8\U0001F1ED}"))); // n_object_emoji
	_ASSERT(!Accepts(AS_ARRAY("{\"a\":\"a\" 123}"))); // n_object_garbage_at_end
	_ASSERT(!Accepts(AS_ARRAY("{key: 'value'}"))); // n_object_key_with_single_quotes
	_ASSERT(!Accepts(AS_ARRAY("\x7B\x22\xB9\x22\x3A\x22\x30\x22\x2C\x7D"))); // n_object_lone_continuation_byte_in_key_and_trailing_comma
	_ASSERT(!Accepts(AS_ARRAY("{\"a\" b}"))); // n_object_missing_colon
	_ASSERT(!Accepts(AS_ARRAY("{:\"b\"}"))); // n_object_missing_key
	_ASSERT(!Accepts(AS_ARRAY("{\"a\" \"b\"}"))); // n_object_missing_semicolon
	_ASSERT(!Accepts(AS_ARRAY("{\"a\":"))); // n_object_missing_value
	_ASSERT(!Accepts(AS_ARRAY("{\"a\""))); // n_object_no-colon
	_ASSERT(!Accepts(AS_ARRAY("{1:1}"))); // n_object_non_string_key
	_ASSERT(!Accepts(AS_ARRAY("{9999E9999:1}"))); // n_object_non_string_key_but_huge_number_instead
	_ASSERT(!Accepts(AS_ARRAY("{null:null,null:null}"))); // n_object_repeated_null_null
	_ASSERT(!Accepts(AS_ARRAY("{\"id\":0,,,,,}"))); // n_object_several_trailing_commas
	_ASSERT(!Accepts(AS_ARRAY("{'a':0}"))); // n_object_single_quote
	_ASSERT(!Accepts(AS_ARRAY("{\"id\":0,}"))); // n_object_trailing_comma
	_ASSERT(!Accepts(AS_ARRAY("{\"a\":\"b\"}/**/"))); // n_object_trailing_comment
	_ASSERT(!Accepts(AS_ARRAY("{\"a\":\"b\"}/**//"))); // n_object_trailing_comment_open
	_ASSERT(!Accepts(AS_ARRAY("{\"a\":\"b\"}//"))); // n_object_trailing_comment_slash_open
	_ASSERT(!Accepts(AS_ARRAY("{\"a\":\"b\"}/"))); // n_object_trailing_comment_slash_open_incomplete
	_ASSERT(!Accepts(AS_ARRAY("{\"a\":\"b\",,\"c\":\"d\"}"))); // n_object_two_commas_in_a_row
	_ASSERT(!Accepts(AS_ARRAY("{a: \"b\"}"))); // n_object_unquoted_key
	_ASSERT(!Accepts(AS_ARRAY("{\"a\":\"a"))); // n_object_unterminated-value
	_ASSERT(!Accepts(AS_ARRAY("{ \"foo\" : \"bar\", \"a\" }"))); // n_object_with_single_string
	_ASSERT(!Accepts(AS_ARRAY("{\"a\":\"b\"}#"))); // n_object_with_trailing_garbage
	_ASSERT(!Accepts(AS_ARRAY(" "))); // n_single_space
	_ASSERT(!Accepts(AS_ARRAY("[\"\\uD800\\\"]"))); // n_string_1_surrogate_then_escape
	_ASSERT(!Accepts(AS_ARRAY("[\"\\uD800\\u\"]"))); // n_string_1_surrogate_then_escape_u
	_ASSERT(!Accepts(AS_ARRAY("[\"\\uD800\\u1\"]"))); // n_string_1_surrogate_then_escape_u1
	_ASSERT(!Accepts(AS_ARRAY("[\"\\uD800\\u1x\"]"))); // n_string_1_surrogate_then_escape_u1x
	_ASSERT(!Accepts(AS_ARRAY("[\u00E9]"))); // n_string_accentuated_char_no_quotes
	_ASSERT(!Accepts(AS_ARRAY("[\"\\\u0000\"]"))); // n_string_backslash_00
	_ASSERT(!Accepts(AS_ARRAY("[\"\\\\\\\"]"))); // n_string_escaped_backslash_bad
	_ASSERT(!Accepts(AS_ARRAY("[\"\\\t\"]"))); // n_string_escaped_ctrl_char_tab
	_ASSERT(!Accepts(AS_ARRAY("[\"\\\U0001F300\"]"))); // n_string_escaped_emoji
	_ASSERT(!Accepts(AS_ARRAY("[\"\\x00\"]"))); // n_string_escape_x
	_ASSERT(!Accepts(AS_ARRAY("[\"\\\"]"))); // n_string_incomplete_escape
	_ASSERT(!Accepts(AS_ARRAY("[\"\\u00A\"]"))); // n_string_incomplete_escaped_character
	_ASSERT(!Accepts(AS_ARRAY("[\"\\uD834\\uDd\"]"))); // n_string_incomplete_surrogate
	_ASSERT(!Accepts(AS_ARRAY("[\"\\uD800\\uD800\\x\"]"))); // n_string_incomplete_surrogate_escape_invalid
	_ASSERT(!Accepts(AS_ARRAY("\x5B\x22\x5C\x75\xE5\x22\x5D"))); // n_string_invalid-utf-8-in-escape
	_ASSERT(!Accepts(AS_ARRAY("[\"\\a\"]"))); // n_string_invalid_backslash_esc
	_ASSERT(!Accepts(AS_ARRAY("[\"\\uqqqq\"]"))); // n_string_invalid_unicode_escape
	_ASSERT(!Accepts(AS_ARRAY("\x5B\x22\x5C\xE5\x22\x5D"))); // n_string_invalid_utf8_after_escape
	_ASSERT(!Accepts(AS_ARRAY("[\\u0020\"asd\"]"))); // n_string_leading_uescaped_thinspace
	_ASSERT(!Accepts(AS_ARRAY("[\\n]"))); // n_string_no_quotes_with_bad_escape
	_ASSERT(!Accepts(AS_ARRAY("\""))); // n_string_single_doublequote
	_ASSERT(!Accepts(AS_ARRAY("['single quote']"))); // n_string_single_quote
	_ASSERT(!Accepts(AS_ARRAY("abc"))); // n_string_single_string_no_double_quotes
	_ASSERT(!Accepts(AS_ARRAY("[\"\\"))); // n_string_start_escape_unclosed
	_ASSERT(!Accepts(AS_ARRAY("[\"a\u0000a\"]"))); // n_string_unescaped_ctrl_char
	_ASSERT(!Accepts(AS_ARRAY("[\"new\nline\"]"))); // n_string_unescaped_newline
	_ASSERT(!Accepts(AS_ARRAY("[\"\t\"]"))); // n_string_unescaped_tab
	_ASSERT(!Accepts(AS_ARRAY("\"\\UA66D\""))); // n_string_unicode_CapitalU
	_ASSERT(!Accepts(AS_ARRAY("\"\"x"))); // n_string_with_trailing_garbage
	_ASSERT(!Accepts(AS_ARRAY("<.>"))); // n_structure_angle_bracket_.
	_ASSERT(!Accepts(AS_ARRAY("[<null>]"))); // n_structure_angle_bracket_null
	_ASSERT(!Accepts(AS_ARRAY("[1]x"))); // n_structure_array_trailing_garbage
	_ASSERT(!Accepts(AS_ARRAY("[1]]"))); // n_structure_array_with_extra_array_close
	_ASSERT(!Accepts(AS_ARRAY("[\"asd]"))); // n_structure_array_with_unclosed_string
	_ASSERT(!Accepts(AS_ARRAY("a\u00E5"))); // n_structure_ascii-unicode-identifier
	_ASSERT(!Accepts(AS_ARRAY("[True]"))); // n_structure_capitalized_True
	_ASSERT(!Accepts(AS_ARRAY("1]"))); // n_structure_close_unopened_array
	_ASSERT(!Accepts(AS_ARRAY("{\"x\": true,"))); // n_structure_comma_instead_of_closing_brace
	_ASSERT(!Accepts(AS_ARRAY("[][]"))); // n_structure_double_array
	_ASSERT(!Accepts(AS_ARRAY("]"))); // n_structure_end_array
	_ASSERT(!Accepts(AS_ARRAY("\xEF\xBB\x7B\x7D"))); // n_structure_incomplete_UTF8_BOM
	_ASSERT(!Accepts(AS_ARRAY("\xE5"))); // n_structure_lone-invalid-utf-8
	_ASSERT(!Accepts(AS_ARRAY("["))); // n_structure_lone-open-bracket
	_ASSERT(!Accepts(AS_ARRAY(""))); // n_structure_no_data
	_ASSERT(!Accepts(AS_ARRAY("[\u0000]"))); // n_structure_null-byte-outside-string
	_ASSERT(!Accepts(AS_ARRAY("2@"))); // n_structure_number_with_trailing_garbage
	_ASSERT(!Accepts(AS_ARRAY("{}}"))); // n_structure_object_followed_by_closing_object
	_ASSERT(!Accepts(AS_ARRAY("{\"\":"))); // n_structure_object_unclosed_no_value
	_ASSERT(!Accepts(AS_ARRAY("{\"a\":/*comment*/\"b\"}"))); // n_structure_object_with_comment
	_ASSERT(!Accepts(AS_ARRAY("{\"a\": true} \"x\""))); // n_structure_object_with_trailing_garbage
	_ASSERT(!Accepts(AS_ARRAY("['"))); // n_structure_open_array_apostrophe
	_ASSERT(!Accepts(AS_ARRAY("[,"))); // n_structure_open_array_comma
	_ASSERT(!Accepts(AS_ARRAY("[{"))); // n_structure_open_array_open_object
	_ASSERT(!Accepts(AS_ARRAY("[\"a"))); // n_structure_open_array_open_string
	_ASSERT(!Accepts(AS_ARRAY("[\"a\""))); // n_structure_open_array_string
	_ASSERT(!Accepts(AS_ARRAY("{"))); // n_structure_open_object
	_ASSERT(!Accepts(AS_ARRAY("{]"))); // n_structure_open_object_close_array
	_ASSERT(!Accepts(AS_ARRAY("{,"))); // n_structure_open_object_comma
	_ASSERT(!Accepts(AS_ARRAY("{["))); // n_structure_open_object_open_array
	_ASSERT(!Accepts(AS_ARRAY("{\"a"))); // n_structure_open_object_open_string
	_ASSERT(!Accepts(AS_ARRAY("{'a'"))); // n_structure_open_object_string_with_apostrophes
	_ASSERT(!Accepts(AS_ARRAY("[\"\\{[\"\\{[\"\\{[\"\\{"))); // n_structure_open_open
	_ASSERT(!Accepts(AS_ARRAY("\xE9"))); // n_structure_single_eacute
	_ASSERT(!Accepts(AS_ARRAY("*"))); // n_structure_single_star
	_ASSERT(!Accepts(AS_ARRAY("{\"a\":\"b\"}#{}"))); // n_structure_trailing_#
	_ASSERT(!Accepts(AS_ARRAY("[\u2060]"))); // n_structure_U+2060_word_joined
	_ASSERT(!Accepts(AS_ARRAY("[\\u000A\"\"]"))); // n_structure_uescaped_LF_before_string
	_ASSERT(!Accepts(AS_ARRAY("[1"))); // n_structure_unclosed_array
	_ASSERT(!Accepts(AS_ARRAY("[ false, nul"))); // n_structure_unclosed_array_partial_null
	_ASSERT(!Accepts(AS_ARRAY("[ true, fals"))); // n_structure_unclosed_array_unfinished_false
	_ASSERT(!Accepts(AS_ARRAY("[ false, tru"))); // n_structure_unclosed_array_unfinished_true
	_ASSERT(!Accepts(AS_ARRAY("{\"asd\":\"asd\""))); // n_structure_unclosed_object
	_ASSERT(!Accepts(AS_ARRAY("\u00E5"))); // n_structure_unicode-identifier
	_ASSERT(!Accepts(AS_ARRAY("\uFEFF"))); // n_structure_UTF8_BOM_no_data
	_ASSERT(!Accepts(AS_ARRAY("[\u000C]"))); // n_structure_whitespace_formfeed
	_ASSERT(!Accepts(AS_ARRAY("[\u2060]"))); // n_structure_whitespace_U+2060_word_joiner
	_ASSERT(Accepts(AS_ARRAY("[[]   ]"))); // y_array_arraysWithSpaces
	_ASSERT(Accepts(AS_ARRAY("[\"\"]"))); // y_array_empty-string
	_ASSERT(Accepts(AS_ARRAY("[]"))); // y_array_empty
	_ASSERT(Accepts(AS_ARRAY("[\"a\"]"))); // y_array_ending_with_newline
	_ASSERT(Accepts(AS_ARRAY("[false]"))); // y_array_false
	_ASSERT(Accepts(AS_ARRAY("[null, 1, \"1\", {}]"))); // y_array_heterogeneous
	_ASSERT(Accepts(AS_ARRAY("[null]"))); // y_array_null
	_ASSERT(Accepts(AS_ARRAY("[1\n]"))); // y_array_with_1_and_newline
	_ASSERT(Accepts(AS_ARRAY(" [1]"))); // y_array_with_leading_space
	_ASSERT(Accepts(AS_ARRAY("[1,null,null,null,2]"))); // y_array_with_several_null
	_ASSERT(Accepts(AS_ARRAY("[2] "))); // y_array_with_trailing_space
	_ASSERT(Accepts(AS_ARRAY("[123e65]"))); // y_number
	_ASSERT(Accepts(AS_ARRAY("[0e+1]"))); // y_number_0e+1
	_ASSERT(Accepts(AS_ARRAY("[0e1]"))); // y_number_0e1
	_ASSERT(Accepts(AS_ARRAY("[ 4]"))); // y_number_after_space
	_ASSERT(Accepts(AS_ARRAY("[-0.000000000000000000000000000000000000000000000000000000000000000000000000000001]\n"))); // y_number_double_close_to_zero
	_ASSERT(Accepts(AS_ARRAY("[20e1]"))); // y_number_int_with_exp
	_ASSERT(Accepts(AS_ARRAY("[-0]"))); // y_number_minus_zero
	_ASSERT(Accepts(AS_ARRAY("[-123]"))); // y_number_negative_int
	_ASSERT(Accepts(AS_ARRAY("[-1]"))); // y_number_negative_one
	_ASSERT(Accepts(AS_ARRAY("[-0]"))); // y_number_negative_zero
	_ASSERT(Accepts(AS_ARRAY("[1E22]"))); // y_number_real_capital_e
	_ASSERT(Accepts(AS_ARRAY("[1E-2]"))); // y_number_real_capital_e_neg_exp
	_ASSERT(Accepts(AS_ARRAY("[1E+2]"))); // y_number_real_capital_e_pos_exp
	_ASSERT(Accepts(AS_ARRAY("[123e45]"))); // y_number_real_exponent
	_ASSERT(Accepts(AS_ARRAY("[123.456e78]"))); // y_number_real_fraction_exponent
	_ASSERT(Accepts(AS_ARRAY("[1e-2]"))); // y_number_real_neg_exp
	_ASSERT(Accepts(AS_ARRAY("[1e+2]"))); // y_number_real_pos_exponent
	_ASSERT(Accepts(AS_ARRAY("[123]"))); // y_number_simple_int
	_ASSERT(Accepts(AS_ARRAY("[123.456789]"))); // y_number_simple_real
	_ASSERT(Accepts(AS_ARRAY("{\"asd\":\"sdf\", \"dfg\":\"fgh\"}"))); // y_object
	_ASSERT(Accepts(AS_ARRAY("{\"asd\":\"sdf\"}"))); // y_object_basic
	_ASSERT(Accepts(AS_ARRAY("{\"a\":\"b\",\"a\":\"c\"}"))); // y_object_duplicated_key
	_ASSERT(Accepts(AS_ARRAY("{\"a\":\"b\",\"a\":\"b\"}"))); // y_object_duplicated_key_and_value
	_ASSERT(Accepts(AS_ARRAY("{}"))); // y_object_empty
	_ASSERT(Accepts(AS_ARRAY("{\"\":0}"))); // y_object_empty_key
	_ASSERT(Accepts(AS_ARRAY("{\"foo\\u0000bar\": 42}"))); // y_object_escaped_null_in_key
	_ASSERT(Accepts(AS_ARRAY("{ \"min\": -1.0e+28, \"max\": 1.0e+28 }"))); // y_object_extreme_numbers
	_ASSERT(Accepts(AS_ARRAY("{\"x\":[{\"id\": \"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\"}], \"id\": \"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\"}"))); // y_object_long_strings
	_ASSERT(Accepts(AS_ARRAY("{\"a\":[]}"))); // y_object_simple
	_ASSERT(Accepts(AS_ARRAY("{\"title\":\"\\u041f\\u043e\\u043b\\u0442\\u043e\\u0440\\u0430 \\u0417\\u0435\\u043c\\u043b\\u0435\\u043a\\u043e\\u043f\\u0430\" }"))); // y_object_string_unicode
	_ASSERT(Accepts(AS_ARRAY("{\n\"a\": \"b\"\n}"))); // y_object_with_newlines
	_ASSERT(Accepts(AS_ARRAY("[\"\\u0060\\u012a\\u12AB\"]"))); // y_string_1_2_3_bytes_UTF-8_sequences
	_ASSERT(Accepts(AS_ARRAY("[\"\\uD801\\udc37\"]"))); // y_string_accepted_surrogate_pair
	_ASSERT(Accepts(AS_ARRAY("[\"\\ud83d\\ude39\\ud83d\\udc8d\"]"))); // y_string_accepted_surrogate_pairs
	_ASSERT(Accepts(AS_ARRAY("[\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"]"))); // y_string_allowed_escapes
	_ASSERT(Accepts(AS_ARRAY("[\"\\\\u0000\"]"))); // y_string_backslash_and_u_escaped_zero
	_ASSERT(Accepts(AS_ARRAY("[\"\\\"\"]"))); // y_string_backslash_doublequotes
	_ASSERT(Accepts(AS_ARRAY("[\"a/*b*/c/*d//e\"]"))); // y_string_comments
	_ASSERT(Accepts(AS_ARRAY("[\"\\\\a\"]"))); // y_string_double_escape_a
	_ASSERT(Accepts(AS_ARRAY("[\"\\\\n\"]"))); // y_string_double_escape_n
	_ASSERT(Accepts(AS_ARRAY("[\"\\u0012\"]"))); // y_string_escaped_control_character
	_ASSERT(Accepts(AS_ARRAY("[\"\\uFFFF\"]"))); // y_string_escaped_noncharacter
	_ASSERT(Accepts(AS_ARRAY("[\"asd\"]"))); // y_string_in_array
	_ASSERT(Accepts(AS_ARRAY("[ \"asd\"]"))); // y_string_in_array_with_leading_space
	_ASSERT(Accepts(AS_ARRAY("[\"\\uDBFF\\uDFFF\"]"))); // y_string_last_surrogates_1_and_2
	_ASSERT(Accepts(AS_ARRAY("[\"new\\u00A0line\"]"))); // y_string_nbsp_uescaped
	_ASSERT(Accepts(AS_ARRAY("[\"\U0010FFFF\"]"))); // y_string_nonCharacterInUTF-8_U+10FFFF
	_ASSERT(Accepts(AS_ARRAY("[\"\uFFFF\"]"))); // y_string_nonCharacterInUTF-8_U+FFFF
	_ASSERT(Accepts(AS_ARRAY("[\"\\u0000\"]"))); // y_string_null_escape
	_ASSERT(Accepts(AS_ARRAY("[\"\\u002c\"]"))); // y_string_one-byte-utf-8
	_ASSERT(Accepts(AS_ARRAY("[\"\u03C0\"]"))); // y_string_pi
	_ASSERT(Accepts(AS_ARRAY("[\"\U0001BFFF\"]"))); // y_string_reservedCharacterInUTF-8_U+1BFFF
	_ASSERT(Accepts(AS_ARRAY("[\"asd \"]"))); // y_string_simple_ascii
	_ASSERT(Accepts(AS_ARRAY("\" \""))); // y_string_space
	_ASSERT(Accepts(AS_ARRAY("[\"\\uD834\\uDd1e\"]"))); // y_string_surrogates_U+1D11E_MUSICAL_SYMBOL_G_CLEF
	_ASSERT(Accepts(AS_ARRAY("[\"\\u0821\"]"))); // y_string_three-byte-utf-8
	_ASSERT(Accepts(AS_ARRAY("[\"\\u0123\"]"))); // y_string_two-byte-utf-8
	_ASSERT(Accepts(AS_ARRAY("[\"\u2028\"]"))); // y_string_u+2028_line_sep
	_ASSERT(Accepts(AS_ARRAY("[\"\u2029\"]"))); // y_string_u+2029_par_sep
	_ASSERT(Accepts(AS_ARRAY("[\"\\u0061\\u30af\\u30EA\\u30b9\"]"))); // y_string_uEscape
	_ASSERT(Accepts(AS_ARRAY("[\"new\\u000Aline\"]"))); // y_string_uescaped_newline
	_ASSERT(Accepts(AS_ARRAY("[\"\u007F\"]"))); // y_string_unescaped_char_delete
	_ASSERT(Accepts(AS_ARRAY("[\"\\uA66D\"]"))); // y_string_unicode
	_ASSERT(Accepts(AS_ARRAY("[\"\\u005C\"]"))); // y_string_unicodeEscapedBackslash
	_ASSERT(Accepts(AS_ARRAY("[\"\u2342\u3234\u2342\"]"))); // y_string_unicode_2
	_ASSERT(Accepts(AS_ARRAY("[\"\\u0022\"]"))); // y_string_unicode_escaped_double_quote
	_ASSERT(Accepts(AS_ARRAY("[\"\\uDBFF\\uDFFE\"]"))); // y_string_unicode_U+10FFFE_nonchar
	_ASSERT(Accepts(AS_ARRAY("[\"\\uD83F\\uDFFE\"]"))); // y_string_unicode_U+1FFFE_nonchar
	_ASSERT(Accepts(AS_ARRAY("[\"\\u200B\"]"))); // y_string_unicode_U+200B_ZERO_WIDTH_SPACE
	_ASSERT(Accepts(AS_ARRAY("[\"\\u2064\"]"))); // y_string_unicode_U+2064_invisible_plus
	_ASSERT(Accepts(AS_ARRAY("[\"\\uFDD0\"]"))); // y_string_unicode_U+FDD0_nonchar
	_ASSERT(Accepts(AS_ARRAY("[\"\\uFFFE\"]"))); // y_string_unicode_U+FFFE_nonchar
	_ASSERT(Accepts(AS_ARRAY("[\"\u20AC\U0001D11E\"]"))); // y_string_utf8
	_ASSERT(Accepts(AS_ARRAY("[\"a\u007Fa\"]"))); // y_string_with_del_character
	_ASSERT(Accepts(AS_ARRAY("false"))); // y_structure_lonely_false
	_ASSERT(Accepts(AS_ARRAY("42"))); // y_structure_lonely_int
	_ASSERT(Accepts(AS_ARRAY("-0.1"))); // y_structure_lonely_negative_real
	_ASSERT(Accepts(AS_ARRAY("null"))); // y_structure_lonely_null
	_ASSERT(Accepts(AS_ARRAY("\"asd\""))); // y_structure_lonely_string
	_ASSERT(Accepts(AS_ARRAY("true"))); // y_structure_lonely_true
	_ASSERT(Accepts(AS_ARRAY("\"\""))); // y_structure_string_empty
	_ASSERT(Accepts(AS_ARRAY("[\"a\"]\n"))); // y_structure_trailing_newline
	_ASSERT(Accepts(AS_ARRAY("[true]"))); // y_structure_true_in_array
	_ASSERT(Accepts(AS_ARRAY(" [] "))); // y_structure_whitespace_array
}

UNITTESTDEF(JSONTestSuiteStress) {
	Accepts(tc::concat(tc::join(tc::repeat_n(500, "[")), tc::join(tc::repeat_n(500, "]")))); // i_structure_500_nested_arrays
	_ASSERT(!Accepts(tc::join(tc::repeat_n(100000, "[")))); // n_structure_100000_opening_arrays
	_ASSERT(!Accepts(tc::join(tc::repeat_n(50000, "[{\"\":")))); // n_structure_open_array_object
}

UNITTESTDEF(JSONDecode) {
	static auto constexpr Test = [](tc::span<char const> strJson, tc::span<char const> strExpected) noexcept {
		auto const MakeParser = [&]() noexcept {
			return tc::json::parser(tc::concat("\"", strJson, "\""), tc::json::simple_error_handler(tc::never_called()));
		};
		_ASSERT(tc::equal(tc::as_lvalue(MakeParser()).expect_string(), tc::make_generator_range(strExpected)));
		_ASSERT(tc::equal(tc::make_generator_range(tc::as_lvalue(MakeParser()).expect_string()), strExpected));
	};
	Test(
		/*   */R"(abc 1\n\u0000\u20AC\uD834\uDD1E\t23 \u0022\" \\\b\f\r\/)",
		AS_ARRAY("abc 1\n\0"  "\u20AC\U0001D11E""\t23 \""  "\" \\\b\f\r/")
	);
	Test("\u20AC", "\u20AC");
	Test("\U0001D11E", "\U0001D11E");
	Test(R"(\uD800\uDBFF_\uDC00\uDFFF)", "\uFFFD\uFFFD_\uFFFD\uFFFD");
	Test(R"(\uD800\uD834\uDD1E\uD800\u20AC)", "\uFFFD\U0001D11E\uFFFD\u20AC");
}

#pragma pop_macro("AS_ARRAY")

UNITTESTDEF(JSONArray_Manual) {
	auto parser = tc::json::parser("[1, 2, 3]", tc::json::simple_error_handler(tc::never_called()));

	parser.expect_array();

	parser.expect_element();
	_ASSERTEQUAL(parser.expect_number<int>(), 1);

	parser.expect_element();
	_ASSERTEQUAL(parser.expect_number<int>(), 2);

	parser.expect_element();
	_ASSERTEQUAL(parser.expect_number<int>(), 3);

	parser.expect_array_end();
}
UNITTESTDEF(JSONArray_ManualLoop) {
	auto parser = tc::json::parser("[1, 2, 3]", tc::json::simple_error_handler(tc::never_called()));

	parser.expect_array();
	for (auto i = 1; parser.element(); ++i) {
		_ASSERTEQUAL(parser.expect_number<int>(), i);
	}
}
UNITTESTDEF(JSONArray_FuncIdx) {
	auto parser = tc::json::parser("[1, 2, 3]", tc::json::simple_error_handler(tc::never_called()));

	auto rng = parser.expect_array([&](std::size_t const idx) noexcept {
		return std::pair(tc::explicit_cast<int>(idx), parser.expect_number<int>());
	});
	TEST_RANGE_EQUAL(rng, TC_FWD(tc::literal_range_of<std::pair(0, 1), std::pair(1, 2), std::pair(2, 3)>));
}
UNITTESTDEF(JSONArray_Func) {
	auto parser = tc::json::parser("[1, 2, 3]", tc::json::simple_error_handler(tc::never_called()));

	auto rng = parser.expect_array([&] {
		return parser.expect_number<int>();
	});
	TEST_RANGE_EQUAL(rng, TC_FWD(tc::literal_range_of<1, 2, 3>));
}

UNITTESTDEF(JSONObject_ManualLoop) {
	auto parser = tc::json::parser(R"({"a": 1, "b": 2, "c": 3})", tc::json::simple_error_handler(tc::never_called()));

	parser.expect_object();
	while (auto const key = parser.key()) {
		auto const expected_number = [&]{
			if (tc::equal(*key, "a")) return 1;
			if (tc::equal(*key, "b")) return 2;
			if (tc::equal(*key, "c")) return 3;
			_ASSERTFALSE;
			return -1;
		}();
		VERIFYEQUAL(parser.expect_number<int>(), expected_number);
	}
}
UNITTESTDEF(JSONObject_Members) {
	auto parser = tc::json::parser(R"({"a": 1, "b": 2, "c": 3})", tc::json::simple_error_handler(tc::never_called()));

	int a, b, c, d;
	d = -1;

	auto const assign = [&](int& var) noexcept {
		return [&] {
			var = parser.expect_number<int>();
		};
	};
	parser.expect_object(
		tc::json::required(tc::named<"b">(assign(b))),
		tc::json::required(tc::named<"a">(assign(a)), tc::named<"A">(assign(a))),
		tc::json::optional(tc::named<"c">(assign(c))),
		tc::json::optional(tc::named<"d">(assign(d)))
	);

	_ASSERTEQUAL(a, 1);
	_ASSERTEQUAL(b, 2);
	_ASSERTEQUAL(c, 3);
	_ASSERTEQUAL(d, -1);
}
