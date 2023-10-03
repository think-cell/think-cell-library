
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../interval.h"
#include "char.h"
#include "named.h"
#include "spirit_algorithm.h"
#include "parserbase.h"

#include <boost/predef/architecture.h>
#if BOOST_ARCH_X86
# define TC_JSON_SIMD 1
# include <emmintrin.h>
#elif BOOST_ARCH_ARM
# define TC_JSON_SIMD 1
# ifdef TC_WIN
#  include <arm64_neon.h> // https://developercommunity.visualstudio.com/t/ARM64EC-should-be-considered-in-arm_neon/1477300
# else
#  include <arm_neon.h>
# endif
#else
# define TC_JSON_SIMD 0
#endif

namespace tc::json {
	namespace no_adl {
		template<typename Func>
		struct simple_error_handler {
			explicit simple_error_handler(Func func) noexcept
				: m_func(tc_move(func))
			{}

			template<typename... Args>
			void semantic_error(auto const& strJson, auto const& itch, Args&&...) const& MAYTHROW {
				m_func(strJson);
			}
			void end_unexpected(auto const& strJson, auto const& itch) const& MAYTHROW {
				m_func(strJson);
			}
			void end_expected(auto const& strJson, auto const& itch) const& MAYTHROW {
				m_func(strJson);
			}
			void char_expected(auto const& strJson, auto const& itch, tc::char_ascii const ch) const& MAYTHROW {
				m_func(strJson);
			}
			void null_expected(auto const& strJson, auto const& itch) const& MAYTHROW {
				m_func(strJson);
			}
			void boolean_expected(auto const& strJson, auto const& itch) const& MAYTHROW {
				m_func(strJson);
			}
			void number_expected(auto const& strJson, auto const& itch) const& MAYTHROW {
				m_func(strJson);
			}
			void object_expected(auto const& strJson, auto const& itch) const& MAYTHROW {
				m_func(strJson);
			}
			void key_expected(auto const& strJson, auto const& itch) const& MAYTHROW {
				m_func(strJson);
			}
			void array_expected(auto const& strJson, auto const& itch) const& MAYTHROW {
				m_func(strJson);
			}
			void element_expected(auto const& strJson, auto const& itch) const& MAYTHROW {
				m_func(strJson);
			}
			void value_expected(auto const& strJson, auto const& itch) const& MAYTHROW {
				m_func(strJson);
			}
			void unescaped_control_character(auto const& strJson, auto const& itch) const& MAYTHROW {
				m_func(strJson);
			}
			void invalid_escape(auto const& strJson, auto const& itch) const& MAYTHROW {
				m_func(strJson);
			}
			void invalid_encoding(auto const& strJson, auto const& itch) const& MAYTHROW {
				m_func(strJson);
			}
			void nesting_too_deep(auto const& strJson, auto const& itch) const& MAYTHROW {
				m_func(strJson);
			}
			void must_contain_one_of_keys(auto const& strJson, auto const& itch, auto const& astrNames) const& MAYTHROW {
				m_func(strJson);
			}
			void may_contain_only_one_of_keys(auto const& strJson, auto const& itch, auto const& astrNames) const& MAYTHROW {
				m_func(strJson);
			}

		private:
			Func m_func;
		};

		template<typename Index, typename Char>
		struct SDecodeAdaptorIndex final {
			Index m_baseidx;

			friend bool operator==(SDecodeAdaptorIndex const& lhs, SDecodeAdaptorIndex const& rhs) noexcept = default;
		};

		template<typename Index>
		struct SDecodeAdaptorIndex<Index, char> final {
			Index m_baseidx;
			int m_nCodeUnitIndex;

			friend bool operator==(SDecodeAdaptorIndex const& lhs, SDecodeAdaptorIndex const& rhs) noexcept = default;
		};

		template< typename Rng >
		struct [[nodiscard]] decode_adaptor
			: tc::range_iterator_from_index<
				decode_adaptor<Rng>,
				SDecodeAdaptorIndex<tc::index_t<std::remove_reference_t<Rng>>, tc::range_value_t<Rng>>
			>
			, tc::range_adaptor_base_range<Rng>
		{
			constexpr decode_adaptor() = default;
			using tc::range_adaptor_base_range<Rng>::range_adaptor_base_range;
			using typename decode_adaptor::range_iterator_from_index::tc_index;

			static constexpr bool c_bHasStashingIndex=tc::has_stashing_index<std::remove_reference_t<Rng>>::value;

		private:
			using this_type = decode_adaptor;
			using Char=tc::range_value_t<Rng>;

			template<bool bIncrement>
			tc::char16 ParseUTF16CodeUnit(tc::index_t<std::remove_reference_t<Rng>>& baseidx) const& MAYTHROW {
				_ASSERT(!tc::at_end_index(this->base_range(), baseidx));
				int n;
				auto it = tc::make_iterator(this->base_range(), baseidx);
				VERIFY(x3::parse(it, tc::end(this->base_range()), x3::uint_parser<int, 16, 4, 4>(), n ));
				if constexpr(bIncrement) {
					baseidx = tc::iterator2index<Rng>(it);
				}
				return static_cast<tc::char16>(n);
			}

			template<bool bIncrement, typename SinkChar, typename SinkCodepoint>
			auto ProcessEscaped(tc::index_t<std::remove_reference_t<Rng>>& baseidx, SinkChar sinkch, SinkCodepoint sinkcp) const& MAYTHROW {
				 // read_string should have ensured well-formed escaping, so we can _ASSERT for it.
				auto& base=this->base_range();
				tc::increment_index(base, baseidx);
				_ASSERT(!tc::at_end_index(base, baseidx));
				auto const ch=tc::dereference_index(base, baseidx);
				switch_no_default(ch) {
				case tc::explicit_cast<Char>('"'):
				case tc::explicit_cast<Char>('\\'):
				case tc::explicit_cast<Char>('/'):
					if constexpr(bIncrement) tc::increment_index(base, baseidx);
					return sinkch(ch);
				case tc::explicit_cast<Char>('b'):
					if constexpr(bIncrement) tc::increment_index(base, baseidx);
					return sinkch(tc::explicit_cast<Char>('\b'));
				case tc::explicit_cast<Char>('f'):
					if constexpr(bIncrement) tc::increment_index(base, baseidx);
					return sinkch(tc::explicit_cast<Char>('\f'));
				case tc::explicit_cast<Char>('n'):
					if constexpr(bIncrement) tc::increment_index(base, baseidx);
					return sinkch(tc::explicit_cast<Char>('\n'));
				case tc::explicit_cast<Char>('r'):
					if constexpr(bIncrement) tc::increment_index(base, baseidx);
					return sinkch(tc::explicit_cast<Char>('\r'));
				case tc::explicit_cast<Char>('t'):
					if constexpr(bIncrement) tc::increment_index(base, baseidx);
					return sinkch(tc::explicit_cast<Char>('\t'));
				case tc::explicit_cast<Char>('u'):
					tc::increment_index(base, baseidx);
					{
						if constexpr(std::same_as<Char, tc::char16>) {
							return sinkch(ParseUTF16CodeUnit<bIncrement>(baseidx));
						} else {
							char32_t ch32 = U'\uFFFD'; // REPLACEMENT CHARACTER
							tc::char16 const ch0 = ParseUTF16CodeUnit<true>(baseidx);
							if(tc::is_leading_codeunit(ch0)) {
								if(!tc::at_end_index(base, baseidx) && tc::char_ascii('\\') == tc::dereference_index(base, baseidx)) {
									std::conditional_t<bIncrement, tc::decay_t<decltype(baseidx)>, decltype(baseidx)> baseidxPeek = baseidx;
									tc::increment_index(base, baseidxPeek);
									_ASSERT(!tc::at_end_index(base, baseidxPeek));
									if('u'_tc == tc::dereference_index(base, baseidxPeek)) {
										tc::increment_index(base, baseidxPeek);
										tc::char16 const ch1 = ParseUTF16CodeUnit<bIncrement>(baseidxPeek);
										if(tc::is_trailing_codeunit(ch1)) {
											if constexpr(bIncrement) baseidx = tc_move(baseidxPeek); // only consume subsequent UTF-16 escape sequence, if a valid UTF-16 code unit sequence is formed.
											ch32 = tc::surrogate_pair_value(ch0, ch1);
										}
									}
								}
							} else if(!tc::is_trailing_codeunit(ch0)) {
								tc::assign_explicit_cast(ch32, ch0);
							}
							if constexpr(std::same_as<Char, char32_t>) {
								return sinkch(ch32);
							} else {
								return sinkcp(ch32);
							}
						}
					}
				}
			}


			STATIC_FINAL(begin_index)() const& return_MAYTHROW(
				tc_index{this->base_begin_index()} // m_nCodeUnitIndex zero-initialized, if present
			)

			STATIC_FINAL(end_index)() const& MAYTHROW requires tc::has_end_index<Rng> {
				return tc_index{this->base_end_index()}; // m_nCodeUnitIndex zero-initialized, if present
			}

			STATIC_FINAL(at_end_index)(tc_index const& idx) const& return_MAYTHROW(
				tc::at_end_index(this->base_range(), idx.m_baseidx)
			)

			STATIC_FINAL(dereference_index)(tc_index const& idx) const& MAYTHROW {
				auto& base=this->base_range();
				auto t=tc::dereference_index(base, idx.m_baseidx); // we return characters, which we can always return by value
				if(tc::char_ascii('\\')==t) {
					return ProcessEscaped<false>(
						tc::as_lvalue(tc::decay_copy(idx.m_baseidx)),
						/*sinkch*/tc::identity(),
						/*sinkcp*/[&](auto const ch32) noexcept {
							STATICASSERTSAME(decltype(ch32), char32_t const);
							return tc::codepoint_codeunit_at<Char>(static_cast<unsigned int>(ch32), idx.m_nCodeUnitIndex);
						}
					);
				} else {
					return t;
				}
			}

			STATIC_FINAL(increment_index)(tc_index& idx) const& MAYTHROW -> void {
				auto& base=this->base_range();
				if(tc::char_ascii('\\')==tc::dereference_index(base, idx.m_baseidx)) {
					auto baseidxOriginal = idx.m_baseidx;
					ProcessEscaped<true>(
						idx.m_baseidx,
						/*sinkch*/tc::noop(),
						/*sinkcp*/[&](auto const ch32) noexcept {
							if constexpr(std::same_as<Char, char>) {
								if(++idx.m_nCodeUnitIndex == tc::codepoint_codeunit_count<char>(static_cast<unsigned int>(ch32))) {
									idx.m_nCodeUnitIndex = 0;
								} else {
									idx.m_baseidx = tc_move(baseidxOriginal);
								}
							}
						}
					);
				} else {
					tc::increment_index(base, idx.m_baseidx);
					_ASSERTDEBUGEQUAL(idx.m_nCodeUnitIndex, 0);
				}
			}

		public:
			static constexpr auto border_base_index(tc_index const& idx) noexcept {
				if constexpr( std::same_as<Char, char> ) {
					_ASSERTEQUAL(idx.m_nCodeUnitIndex, 0);
				}
				return idx.m_baseidx;
			}

			template<ENABLE_SFINAE, typename Sink>
			auto operator()(Sink sink) const& MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(sink, tc::dereference_index(SFINAE_VALUE(this)->base_range(), std::declval<tc_index const&>))), tc::constant<tc::continue_>>{
				auto& base=this->base_range();
				auto idx=this->base_begin_index();
				for(;;) {
					if(tc::at_end_index(base, idx)) return tc::constant<tc::continue_>();
					if(tc::char_ascii('\\')!=tc::dereference_index(base, idx)) {
						auto idxBegin=idx;
						do {
							tc::increment_index(base, idx);
							if(tc::at_end_index(base, idx)) {
								return tc::for_each(tc::slice(base, idxBegin, idx), sink);
							}
						} while(tc::char_ascii('\\')!=tc::dereference_index(base, idx));
						tc_return_if_break( tc::for_each(tc::slice(base, idxBegin, idx), sink) );
					}
					tc_return_if_break( ProcessEscaped<true>(
						idx,
						[&](auto const ch) MAYTHROW {
							return tc::continue_if_not_break(sink, ch);
						},
						[&](auto const ch32) MAYTHROW {
							return tc::for_each(tc::convert_enc<Char>(tc::single(tc::decay_copy(ch32))), sink);
						}
					) );
				}
			}
		};
	}
}

namespace tc {
	template<typename Rng>
	constexpr auto enable_stable_index_on_move<tc::json::no_adl::decode_adaptor<Rng>> = tc::stable_index_on_move<Rng>;
}

namespace tc::json {
	template<typename Rng>
	constexpr auto decode(Rng&& rng)
		return_ctor_noexcept( no_adl::decode_adaptor<Rng>, (aggregate_tag, std::forward<Rng>(rng)) )

	using no_adl::simple_error_handler;

	template<typename T>
	auto const assert_and_throw=tc::json::simple_error_handler([](tc::unused) THROW(T) {
		_ASSERTNOTIFYFALSE; throw T();
	});

	namespace no_adl {
		template<bool c_bRequired, typename... Keys>
		struct group final {
		private:
			tc::tuple<Keys...> m_tplkey;

			using ResultT = tc::common_type_t<decltype(std::declval<Keys>().m_t())...>;
			using OptionalResultT = std::conditional_t<std::is_void<ResultT>::value, bool, std::optional<ResultT>>;
			OptionalResultT m_oresult = {}; // value initialize: bool to false, optional to nullopt

		public:
			static constexpr auto c_bHasResult = !std::is_void<ResultT>::value;
			static constexpr tc::span<tc::char_ascii const> c_astrKeys[]= { Keys::c_strName... };

			explicit group(Keys... keys) noexcept : m_tplkey{keys...} {}

			bool parse(auto& parser, auto const& strParsedName) & MAYTHROW {
				return tc::any_of(m_tplkey, [&](auto const& key) MAYTHROW {
					if (!tc::equal(strParsedName, key.c_strName)) return false;

					if (m_oresult) {
						parser.template may_contain_only_one_of_keys<c_astrKeys>(); // MAYTHROW
					} else if constexpr (c_bHasResult) {
						m_oresult.emplace(key.m_t());
					} else {
						key.m_t();
						m_oresult = true;
					}
					return true;
				});
			}

			void finalize(auto& parser) const& MAYTHROW {
				if constexpr (c_bRequired) {
					if (!m_oresult) {
						parser.template must_contain_one_of_keys<c_astrKeys>(); // MAYTHROW
					}
				}
			}

			auto result() && noexcept(std::is_nothrow_move_constructible<ResultT>::value) {
				if constexpr (c_bRequired) {
					if constexpr (c_bHasResult) {
						return *tc_move(m_oresult); // result is ResultT
					} else {
						return; // result is void
					}
				} else {
					return tc_move(m_oresult); // result is std::optional<ResultT> / bool
				}
			}
		};
	}
	template<typename... Keys>
	constexpr auto required(Keys... keys) noexcept {
		return no_adl::group<true,Keys...>(tc_move(keys)...);
	}
	template<typename... Keys>
	constexpr auto optional(Keys... keys) noexcept {
		return no_adl::group<false,Keys...>(tc_move(keys)...);
	}

	namespace no_adl {

		struct skip_exception {};

		template<typename String, typename ErrorHandler>
		struct [[nodiscard]] parser : parser_base<String, ErrorHandler> {
			using typename parser_base<String, ErrorHandler>::char_type;

			explicit parser(String&& strInput, ErrorHandler errorhandler) MAYTHROW
				: parser_base<String, ErrorHandler>(tc_move_if_owned(strInput), tc_move(errorhandler))
			{
				this->skip_whitespace(); // guarantees !end()
			}

			//-------------------------------------------------------------------------------------------------------------------------
			// Primitive values
			bool null() & MAYTHROW {
				switch (this->unchecked_peek()) {
				case 'n':
					this->unchecked_increment();
					this->expect_literal("ull"_tc);
					this->skip_whitespace_maybe_end();
					return true;

				default:
					return false;
				}
			}
			void expect_null() & MAYTHROW {
				if (!null()) {
					this->template error<tc_mem_fn(.null_expected)>(); // MAYTHROW
				}
			}

			std::optional<bool> boolean() & MAYTHROW {
				switch (this->unchecked_peek()) {
				case 't':
					this->unchecked_increment();
					this->expect_literal("rue"_tc);
					this->skip_whitespace_maybe_end();
					return true;

				case 'f':
					this->unchecked_increment();
					this->expect_literal("alse"_tc);
					this->skip_whitespace_maybe_end();
					return false;

				default:
					return std::nullopt;
				}
			}
			bool expect_boolean() & MAYTHROW {
				if (auto ob = boolean()) {
					return *ob;
				} else {
					this->template error<tc_mem_fn(.boolean_expected)>(); // MAYTHROW
				}
			}

			template<typename T>
			std::optional<T> number() & MAYTHROW {
				static_assert(tc::common_range<String>, "Spirit requires common ranges"); // TODO?
				if ( auto const ch = this->unchecked_peek(); '-'_tc != ch && !tc::make_interval('0'_tc, '9'_tc).contains_inclusive(ch) ) {
					return std::nullopt;
				}

				// TODO: the x3 parsers don't follow the exact same grammar as JSON numbers
				std::optional<T> ot(std::in_place);
				if constexpr( std::floating_point<T> ) {
					auto it = this->position();
					if (!tc::parse_iterator(it, this->end_position(), x3::real_parser<T>(), *ot)) {
						ot.reset();
					}
					this->set_position(it);
					this->skip_whitespace_maybe_end();
				} else {
					static_assert( tc::actual_integer<T> );
					auto it = this->position();
					if (!tc::parse_iterator(it, this->end_position(), x3::int_parser<T>(), *ot)) {
						ot.reset();
					}
					this->set_position(it);
					this->skip_whitespace_maybe_end();
				}
				return ot;
			}
			template<typename T>
			T expect_number() & MAYTHROW {
				if (auto ot = number<T>()) {
					return *tc_move(ot);
				} else {
					this->template error<tc_mem_fn(.number_expected)>(); // MAYTHROW
				}
			}
			template<typename T>
			void assign_expect_number(T& t) & MAYTHROW {
				t = expect_number<T>();
			}

			auto number() & MAYTHROW {
				using Result = std::optional<decltype(tc::slice(this->input(), this->position(), this->position()))>;

				static auto constexpr digits = "0123456789"_tc;
				auto const skip_one_or_more_digits = [&]() MAYTHROW {
					if (!this->one_of(digits)) {
						this->template error<tc_mem_fn(.number_expected)>(); // MAYTHROW
					}
					while (this->one_of(digits)) {}
				};

				auto const itchBegin = this->position();
				auto bCommitted = false;

				// Sign and integer.
				integer: switch(this->unchecked_peek()) {
				case '0':
					this->unchecked_increment();
					break;

				case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
					this->unchecked_increment();
					for (;;) {
						if (this->end()) goto end;
						switch (auto const ch = this->unchecked_peek()) {
						case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
							this->unchecked_increment();
							break;
						case '.':
							this->unchecked_increment();
							goto fraction;
						case 'e': case 'E':
							this->unchecked_increment();
							goto exponent;
						default:
							goto end;
						}
					}
					_ASSERTFALSE;

				case '-':
					if (tc::change(bCommitted, true)) {
						this->unchecked_increment();
						this->expect_not_end();
						goto integer;
					}
					// fallthrough
				default:
					if (bCommitted) {
						this->template error<tc_mem_fn(.number_expected)>(); // MAYTHROW
					} else {
						return Result();
					}
				}

				// Fraction and exponent.
				if (!this->end()) {
					switch (this->unchecked_peek()) {
					case '.':
						this->unchecked_increment();
					fraction:
						skip_one_or_more_digits();
						if (this->one_of("eE"_tc)) {
							goto exponent;
						}
						break;

					case 'e': case 'E':
						this->unchecked_increment();
					exponent:
						tc::discard(this->one_of("+-"_tc));
						skip_one_or_more_digits();
						break;
					}
				}

			end:
				auto const itchEnd = this->position();
				this->skip_whitespace_maybe_end();
				return Result(tc::slice(this->input(), itchBegin, itchEnd));
			}
			auto expect_number() & MAYTHROW {
				if (auto on = number()) {
					return *on;
				} else {
					this->template error<tc_mem_fn(.number_expected)>(); // MAYTHROW
				}
			}

			auto string() & MAYTHROW {
				using Result = std::optional<decltype(read_string())>;
				switch(this->unchecked_peek()) {
				case '"': {
					this->unchecked_increment();
					auto result = read_string(); // MAYTHROW
					this->skip_whitespace_maybe_end();
					return Result(result);
				}

				default:
					return Result();
				}
			}
			auto expect_string() & MAYTHROW {
				this->expect_literal("\""_tc);
				auto str = read_string();
				this->skip_whitespace_maybe_end();
				return str;
			}
			auto expect_string_or_null() & MAYTHROW {
				if (null()) {
					return decode(tc::slice(this->input(), this->position(), this->position()));
				} else {
					return expect_string();
				}
			}
			auto expect_non_empty_string() & MAYTHROW {
				auto str = expect_string();
				if (tc::empty(str)) this->semantic_error();
				return str;
			}

			//-------------------------------------------------------------------------------------------------------------------------
			// Array
			bool array()& MAYTHROW {
				switch (this->unchecked_peek()) {
				case '[':
					this->unchecked_increment();
					VERIFY(tc::change(this->m_bAtArrayOrObjectStart, true));
					this->skip_whitespace(); // MAYTHROW
					return true;

				default:
					return false;
				}
			}
			void expect_array() & MAYTHROW {
				if (!array()) {
					this->template error<tc_mem_fn(.array_expected)>(); // MAYTHROW
				}
			}

			bool element() & MAYTHROW {
				// After some value (null/boolean/number/string/array/object) ends, we allow EOI by calling skip_whitespace_maybe_end()
				// because it may be the root value. If it is not, another element() or key() must follow, so there, we must check for EOI.
				this->expect_not_end(); // MAYTHROW

				auto const ch = this->unchecked_peek();
				if (']' == ch) {
					this->unchecked_increment();
					this->m_bAtArrayOrObjectStart = false;
					this->skip_whitespace_maybe_end(); // MAYTHROW
					return false;
				} else {
					if (!tc::change(this->m_bAtArrayOrObjectStart, false)) {
						if (',' == ch) {
							this->unchecked_increment();
							this->skip_whitespace();
						} else {
							this->template error<tc_mem_fn(.char_expected)>(tc::char_ascii(',')); // MAYTHROW
						}
					}
					return true;
				}
			}
			void expect_element() & MAYTHROW {
				if(!element()) {
					this->template error<tc_mem_fn(.element_expected)>(); // MAYTHROW
				}
			}

			template <typename Func> requires tc::invocable<Func&, std::size_t>
			auto elements(Func func) & MAYTHROW {
				using result_t = decltype(func(std::size_t(0)));
				return tc::generator_range_output<result_t&&>([this, func=tc_move(func)](auto sink) MAYTHROW {
					auto const invoke_sink = [&](std::size_t const idx) MAYTHROW {
						try
						{
							return tc::continue_if_not_break(sink, func(idx));
						} catch (skip_exception const&) {
							while (element()) skip_value();
							throw;
						}
					};

					static auto constexpr bAlwaysBreaks = std::is_same<decltype(invoke_sink(0)), tc::constant<tc::break_>>::value;
					static auto constexpr bNeverBreaks = std::is_same<decltype(invoke_sink(0)), tc::constant<tc::continue_>>::value;
					if constexpr (bAlwaysBreaks) {
						if (element()) {
							tc::discard(invoke_sink(0));
							while (element()) skip_value();
							return tc::break_;
						} else {
							return tc::continue_;
						}
					} else if constexpr (bNeverBreaks) {
						for (std::size_t idx = 0; element(); ++idx) {
							tc::discard(invoke_sink(idx));
						}
						return tc::constant<tc::continue_>{};
					} else {
						for (std::size_t idx = 0; element(); ++idx) {
							if (tc::break_ == invoke_sink(idx)) {
								while (element()) skip_value();
								return tc::break_;
							}
						}
						return tc::continue_;
					}
				});
			}
			template<typename Func> requires tc::invocable<Func&> && (!tc::invocable<Func&, std::size_t>)
			auto elements(Func func) & MAYTHROW {
				return elements([func = tc_move(func)](std::size_t) return_decltype_xvalue_by_ref_MAYTHROW(func()));
			}
			template<typename Func> requires tc::invocable<Func&> || tc::invocable<Func&, std::size_t>
			auto expect_array(Func func) & MAYTHROW {
				expect_array();
				return elements(tc_move(func));
			}

			void expect_array_end() & MAYTHROW {
				this->expect_literal("]"_tc);
				this->skip_whitespace_maybe_end();
			}

			template <typename Sink> requires tc::invocable<Sink const&, std::size_t>
			auto for_each_element(Sink const sink) & MAYTHROW 
				-> tc::common_type_t<decltype(tc::continue_if_not_break(std::declval<Sink const&>(), std::size_t(0))), tc::constant<tc::continue_>> {
				for (std::size_t idx = 0; element(); ++idx) {
					try {
						tc_yield(sink, idx);
					} catch (skip_exception const&) {
						while (element()) skip_value();
						throw;
					}
				}
				return tc::constant<tc::continue_>();
			}
			template <typename Sink> requires tc::invocable<tc::decay_t<Sink> const&> && (!tc::invocable<tc::decay_t<Sink> const&, std::size_t>)
			auto for_each_element(Sink&& sink) & MAYTHROW {
				return for_each_element([sink = tc_move_if_owned(sink)](std::size_t) return_decltype_MAYTHROW(sink()));
			}

			template<typename Func>
			auto optional_single_element(Func func) & MAYTHROW {
				expect_array();
				using T = decltype(func());
				using OptT = std::conditional_t<std::is_void<T>::value, bool, std::optional<T>>;
				OptT ot{}; // if bool, value-initialize to false, otherwise to std::nullopt
				if (element()) {
					try {
						if constexpr (std::is_void<T>::value) {
							func();
							ot = true;
						} else {
							ot.emplace(func());
						}
					} catch (skip_exception const&) {
						while (element()) skip_value();
						throw;
					}
					expect_array_end();
				}
				return ot;
			}
			template<typename Func>
			auto required_single_element(Func func) & MAYTHROW {
				expect_array();
				expect_element();
				try {
					if constexpr (std::is_void<decltype(func())>::value) {
						func(); // MAYTHROW
						expect_array_end();
					} else {
						auto t = func(); // MAYTHROW
						expect_array_end();
						return t;
					}
				} catch (skip_exception const&) {
					while(element()) skip_value();
					throw;
				}
			}

			//-------------------------------------------------------------------------------------------------------------------------
			// Object
			bool object() & MAYTHROW {
				switch (this->unchecked_peek()) {
				case '{':
					this->unchecked_increment();
					VERIFY(tc::change(this->m_bAtArrayOrObjectStart, true));
					this->skip_whitespace(); // MAYTHROW
					return true;

				default:
					return false;
				}
			}
			void expect_object() & MAYTHROW {
				if (!object()) {
					this->template error<tc_mem_fn(.object_expected)>(); // MAYTHROW
				}
			}

			auto key() & MAYTHROW {
				// After some value (object/array/number/boolean/...) ends, we allow EOI by calling skip_whitespace_maybe_end()
				// because it may be the root value. If it is not, another element() or key() must follow, so there, we must check for EOI.
				this->expect_not_end();

				using Result = std::optional<decltype(read_string())>;
				auto const ch = this->unchecked_peek();
				if ('}' == ch) {
					this->unchecked_increment();
					this->m_bAtArrayOrObjectStart = false;
					this->skip_whitespace_maybe_end();
					return Result{};
				} else {
					if (!tc::change(this->m_bAtArrayOrObjectStart, false)) {
						if (',' == ch) {
							this->unchecked_increment();
							this->skip_whitespace();
						} else {
							this->template error<tc_mem_fn(.char_expected)>(tc::char_ascii(',')); // MAYTHROW
						}
					}

					if (!this->literal("\""_tc)) {
						this->template error<tc_mem_fn(.key_expected)>(); // MAYTHROW
					}
					auto strKey = read_string();

					this->skip_whitespace();
					this->expect_literal(":"_tc);
					this->skip_whitespace();

					return tc::explicit_cast<Result>(tc_move(strKey));
				}
			}

			template <typename... Groups> requires( 0<sizeof...(Groups) )
			void members(Groups&&... group) & MAYTHROW {
				static_assert(((std::is_lvalue_reference<Groups>::value || !std::remove_reference_t<Groups>::c_bHasResult) && ...),
					"at least one member returns a value that we're discarding");

				while (tc_auto_cref(ostr, key())) {
					try {
						if (!(group.parse(*this, *ostr) || ...)) {
							skip_value();
						}
					} catch (skip_exception const&) {
						while(key()) skip_value();
						throw;
					}
				}
				(group.finalize(*this), ...);
			}
			template <typename... Groups> requires( 0<sizeof...(Groups) )
			void expect_object(Groups&&... group) & MAYTHROW {
				expect_object();
				members(tc_move_if_owned(group)...);
			}

			template <typename Group> requires std::is_rvalue_reference<Group&&>::value
			[[nodiscard]] auto expect_single_member_object(Group&& group) & MAYTHROW {
				expect_object(group);
				return tc_move(group).result();
			}

			template<auto const& c_astrNames>
			[[noreturn]] void must_contain_one_of_keys() & MAYTHROW {
				this->template error<[](auto& error, auto const& rng, auto const& it) MAYTHROW {
					error.must_contain_one_of_keys(rng, it, c_astrNames);
				}>(); // MAYTHROW
			}
			template<auto const& c_astrNames>
			[[noreturn]] void may_contain_only_one_of_keys() & MAYTHROW {
				this->template error<[](auto& error, auto const& rng, auto const& it) MAYTHROW {
					error.may_contain_only_one_of_keys(rng, it, c_astrNames);
				}>(); // MAYTHROW
			}

			//-------------------------------------------------------------------------------------------------------------------------
			// Any value.
			void skip_value() & MAYTHROW {
				std::bitset<256> bitsetIsArray; // (unnecessarily) zero-initialized.
				std::size_t nNestingLevel = 0;
				for(;;) {
					// Optimization: we do one switch here to dispatch to the appropriate member function, instead of individual branches in each member function.
					// After inlining of trivial functions like null or boolean, this is faster.
					switch (this->unchecked_peek()) {
					case 'n':
						VERIFY(null()); // MAYTHROW
						break;
					case 't':
					case 'f':
						VERIFY(boolean()); // MAYTHROW
						break;
					case '-':
					case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
						VERIFY(number()); // MAYTHROW
						break;
					case '"':
						VERIFY(string()); // MAYTHROW
						break;

					case '[':
						if (bitsetIsArray.size() <= nNestingLevel) {
							this->template error<tc_mem_fn(.nesting_too_deep)>(); // MAYTHROW
						}
						VERIFY(array()); // MAYTHROW
						bitsetIsArray.set(nNestingLevel++);
						break;
					case '{':
						if (bitsetIsArray.size() <= nNestingLevel) {
							this->template error<tc_mem_fn(.nesting_too_deep)>(); // MAYTHROW
						}
						VERIFY(object()); // MAYTHROW
						bitsetIsArray.reset(nNestingLevel++);
						break;

					default:
						this->template error<tc_mem_fn(.value_expected)>(); // MAYTHROW
						break;
					}
					for(;;) {
						if(0==nNestingLevel) return;
						if(bitsetIsArray.test(nNestingLevel - 1) ? element() : tc::explicit_cast<bool>(key())) break;
						--nNestingLevel;
					}
				}
			}

			using parser_base<String, ErrorHandler>::expect_end;

		private:
			// Precondition: We have just consumed the opening '"'.
			auto read_string() & MAYTHROW {
				auto itchBegin = this->position();

				#if TC_JSON_SIMD
				if constexpr (sizeof(char_type) == 1 && tc::contiguous_range<String> && tc::common_range<String>) {
					// Quickly fast forward to first occurrence of byte values 0-0x1F (control characters), 0x22 ("), 0x5C (\) or 0x80-0xFF (non-ascii).
					auto it = this->position();
					auto const end = this->end_position();
					for(;;) {
						if (16 <= end - it) {
						#if BOOST_ARCH_X86
							auto const m128Chunk = _mm_loadu_si128(reinterpret_cast<__m128i const*>(std::to_address(it)));
							auto const nSpecialCaseMask = static_cast<std::uint16_t>(_mm_movemask_epi8( // get 16 most significant bits as bitmask
								_mm_or_si128(
									_mm_or_si128(
										_mm_cmpeq_epi8(m128Chunk, _mm_set1_epi8('"')), // sets lane to 0xFF, if quote
										_mm_cmpeq_epi8(m128Chunk, _mm_set1_epi8('\\')) // sets lane to 0xFF, if backslash
									),
									_mm_cmplt_epi8(m128Chunk, _mm_set1_epi8(0x20)) // sets lane to 0xFF, if control character or non-ascii
								)
							));
						#else
							auto const s8x16Chunk = vld1q_s8(reinterpret_cast<std::int8_t const*>(std::to_address(it)));
							// https://community.arm.com/arm-community-blogs/b/infrastructure-solutions-blog/posts/porting-x86-vector-bitmask-optimizations-to-arm-neon
							auto const nSpecialCaseMask = vget_lane_u64(
								vreinterpret_u64_u8( // treat 8 bytes as 64 bit word
									vshrn_n_u16( // shift 16bit word right by 4, s.t., the low byte in each word contain a nibble of the original low byte and the original high byte, and return the 8 resulting low bytes
										vreinterpretq_u16_u8(vorrq_u8(
											vorrq_u8(
												vceqq_s8(s8x16Chunk, vdupq_n_s8('"')), // sets lane to 0xFF, if quote
												vceqq_s8(s8x16Chunk, vdupq_n_s8('\\')) // sets lane to 0xFF, if backslash
											),
											vcltq_s8(s8x16Chunk, vdupq_n_s8(0x20)) // sets lane to 0xFF, if control character or non-ascii
										)),
										4
									)
								),
								0
							);
						#endif
							if(0 != nSpecialCaseMask) {
								it += tc::index_of_least_significant_bit(nSpecialCaseMask) / (BOOST_ARCH_ARM ? 4 : 1);
								break;
							} else {
								it += 16;
							}
						} else /*not near EOI most of the time*/[[unlikely]] {
							break;
						}
					}
					this->set_position(it);
				}
				#endif

				for (;;) {
					this->expect_not_end();
					switch (auto const ch = this->unchecked_peek()) {
					case '"': {
						auto itchEnd = this->position();
						this->unchecked_increment();

						return decode(tc::slice(this->input(), itchBegin, itchEnd));
					}

					case '\\':
						this->unchecked_increment();
						this->expect_not_end();
						switch (this->unchecked_peek()) {
						case '"':
						case '\\':
						case '/':
						case 'b':
						case 'f':
						case 'n':
						case 'r':
						case 't':
							this->unchecked_increment();
							break;

						case 'u':
							this->unchecked_increment();
							for (auto i = 0; i < 4; ++i) {
								if (!this->char_class(tc_fn(tc::isasciixdigit))) {
									this->template error<tc_mem_fn(.invalid_escape)>(); // MAYTHROW
								}
							}
							break;

						default:
							this->template error<tc_mem_fn(.invalid_escape)>(); // MAYTHROW
						}
						break;

					default:
						if (tc::make_interval(0x00, 0x1F).contains_inclusive(ch)) {
							// The control characters (U+0000 through U+001F) must be escaped (RFC 8259). U+007F is fine.
							this->template error<tc_mem_fn(.unescaped_control_character)>(); // MAYTHROW
						} else {
							this->unchecked_increment();
							if constexpr (sizeof(char_type) == 1) {
								this->skip_utf8_code_point(ch);
							}
						}
						break;
					}
				}
			}

			bool m_bAtArrayOrObjectStart = false;
		};

		template<typename String, typename ErrorHandler>
		parser(String&&, ErrorHandler) -> parser<String, ErrorHandler>;
	}
	using no_adl::parser;
	using no_adl::skip_exception;

	// Json uses only UTF-8 encoding: https://tools.ietf.org/html/rfc8259#section-8.1
	// ' JSON text exchanged between systems that are not part of a closed ecosystem MUST be encoded using UTF-8 [RFC3629].'
	template<typename Rng>
	decltype(auto) remove_bom(Rng&& rngbyte) {
		return tc::starts_with<tc::return_drop_or_all>(tc_move_if_owned(rngbyte), tc_as_constexpr(tc::make_array<unsigned char>(tc::aggregate_tag, 0xef, 0xbb, 0xbf)));
	}
}
