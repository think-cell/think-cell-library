
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "base/assert_defs.h"
#include "base/conditional.h"
#include "base/utility.h"
#include "base/as_lvalue.h"
#include "base/explicit_cast_fwd.h"
#include "base/scope.h"
#include "range/meta.h"
#include "storage_for.h"
#include <optional>

namespace tc {
	// TODO implement fully functional tc::optional

	// In most std::optional implementations, the "holding a value" flag is false during con- and destruction.
	// For them it actually doesn't matter, because querying the status from inside is undefined according to 16.4.6.9 [reentrancy]
	// > Except where explicitly specified in this document, it is implementation-defined which functions in the C++ standard library may be recursively reentered.
	// We explicitly want optionals to be accessible during con- and destruction so we implement this custom type
	namespace no_adl {
		template<typename T>
		struct optional {
			using value_type = T;
	
			constexpr optional() noexcept : m_bValue(false) {
			}
			constexpr optional(std::nullopt_t) noexcept : m_bValue(false) {
			}
			constexpr optional(optional const& rhs) noexcept(noexcept(construct(*rhs.m_oValue)))
			: m_bValue(rhs.m_bValue) {
				if (m_bValue) {
					construct(*rhs.m_oValue);
				} 
			}
			template<typename... Args>
			optional(std::in_place_t, Args&& ... args) noexcept(noexcept(construct(tc_move_if_owned(args)...)))
			: m_bValue(true) {
				construct(tc_move_if_owned(args)...);
			}
			~optional() {
				reset();
			}
	
			void reset() & noexcept {
				if (tc::change(m_bValue, false)) { // allow querying the flag to prevent double-destruction
#ifdef _CHECKS
					_ASSERT(!m_bInsideDtor);
					tc_scoped_assign(m_bInsideDtor, true);
#endif
					m_oValue.dtor();
				}
			}
			template<typename... Args>
			T& emplace(Args&& ... args) & noexcept(noexcept(construct(tc_move_if_owned(args)...))) {
				reset();
				VERIFY(tc::change(m_bValue, true));
				construct(tc_move_if_owned(args)...);
				return *m_oValue;
			}
	
			optional& operator=(std::nullopt_t) & noexcept {
				reset();
				return *this;
			}
	
			constexpr bool has_value() const& noexcept {
				return m_bValue;
			}
			constexpr explicit operator bool() const& noexcept {
				return has_value();
			}
	
			T const* operator->() const& noexcept {
				_ASSERT(*this || m_bInsideDtor);
				return m_oValue.operator->();
			}
			T* operator->() & noexcept {
				_ASSERT(*this || m_bInsideDtor);
				return m_oValue.operator->();
			}
			T const& operator*() const& noexcept {
				_ASSERT(*this || m_bInsideDtor);
				return *m_oValue;
			}
			T const&& operator*() const&& noexcept {
				_ASSERT(*this || m_bInsideDtor);
				return *tc_move(m_oValue);
			}
			T& operator*() & noexcept {
				_ASSERT(*this || m_bInsideDtor);
				return *m_oValue;
			}
			T&& operator*() && noexcept {
				_ASSERT(*this || m_bInsideDtor);
				return *tc_move(m_oValue);
			}
	
		private:
			bool m_bValue;
#ifdef _CHECKS
			// The constructor is allowed to be re-entrant, the destructor is not
			bool m_bInsideDtor = false;
#endif
			tc::storage_for<T> m_oValue;
	
			template<typename... Args>
			void construct(Args&& ... args) & noexcept(noexcept(std::declval<storage_for<T>&>().ctor(tc_move_if_owned(args)...))) {
				_ASSERT(m_bValue); // allow querying the flag to prevent double-construction
				_ASSERT(!m_bInsideDtor);
				if constexpr (noexcept(m_oValue.ctor(tc_move_if_owned(args)...))) {
					m_oValue.ctor(tc_move_if_owned(args)...);
				} else {
					try {	
						m_oValue.ctor(tc_move_if_owned(args)...); // MAYTHROW
					} catch( ... ) {
						m_bValue = false;
						throw; // MSVC and gcc: throw in catch block of a noexcept function triggers warning even if try block never throws
					}
				}
			}
		};

		template<tc::empty_type TEmpty>
		struct optional<TEmpty> {
			constexpr optional() noexcept = default;
			constexpr optional(std::nullopt_t) noexcept {}

			template<typename... Args>
			constexpr optional(std::in_place_t, Args&& ... args) noexcept(std::is_nothrow_constructible_v<TEmpty, Args&&...>) : m_bValue(true) {
				tc::discard(TEmpty(tc_move_if_owned(args)...));
			}

			constexpr void reset() & noexcept {
				m_bValue = false;
			}
			template<typename... Args>
			constexpr TEmpty emplace(Args&& ... args) & noexcept(std::is_nothrow_constructible_v<TEmpty, Args&&...>) {
				TEmpty result(tc_move_if_owned(args)...);
				m_bValue = true;
				return result;
			}
	
			constexpr optional& operator=(std::nullopt_t) & noexcept {
				reset();
				return *this;
			}
	
			constexpr bool has_value() const& noexcept {
				return m_bValue;
			}
			constexpr explicit operator bool() const& noexcept {
				return has_value();
			}
	
		private:
			struct pointer final {
				TEmpty m_t;
				constexpr TEmpty* operator->() && noexcept { return std::addressof(m_t); }
			};
		public:
			constexpr auto operator->() const& noexcept {
				_ASSERT(*this);
				return pointer();
			}
			constexpr TEmpty operator*() const& noexcept {
				_ASSERT(*this);
				return TEmpty();
			}

		private:
			bool m_bValue = false;
		};

		template<typename TRef> requires std::is_reference<TRef>::value
		struct optional<TRef> {
			template<typename U>
			friend struct optional;

			constexpr optional() noexcept = default;
			constexpr optional(std::nullopt_t) noexcept {}

			template<typename U> requires tc::safely_constructible_from<TRef, U&&>
			constexpr optional(U&& u) noexcept(noexcept(static_cast<TRef>(tc_move_if_owned(u))))
				: m_pt(std::addressof(tc::as_lvalue(static_cast<TRef>(tc_move_if_owned(u))))) // tc::safely_constructible_from makes sure no reference to temporary is created
			{}

			template<typename TOptional> requires
				tc::safely_constructible_from<TRef, decltype(*std::declval<TOptional>()) >
			constexpr optional(TOptional&& rhs) noexcept
				: m_pt([&]() noexcept {
					return rhs ? std::addressof(tc::as_lvalue(*tc_move_if_owned(rhs))) : nullptr;
				}())
			{}

			constexpr optional& operator=(std::nullopt_t) /*no &*/ noexcept {
				this->reset();
				return *this;
			}

			template<typename U> requires tc::safely_constructible_from<TRef, U&&>
			constexpr TRef emplace(U&& u) /*no &*/ noexcept(noexcept(static_cast<TRef>(tc_move_if_owned(u)))) {
				m_pt = std::addressof(tc::as_lvalue(static_cast<TRef>(tc_move_if_owned(u)))); // tc::safely_constructible_from makes sure no reference to temporary is created
				return *m_pt;
			}

			constexpr void reset() /*no &*/ noexcept {
				m_pt=nullptr;
			}

			constexpr bool has_value() const& noexcept {
				return m_pt;
			}
			constexpr explicit operator bool() const& noexcept {
				return has_value();
			}

			constexpr std::remove_reference_t<TRef>& operator*() const& noexcept {
				_ASSERT(*this);
				return *m_pt;
			}

			constexpr TRef operator*() const&& noexcept {
				_ASSERT(*this);
				return static_cast<TRef>(*m_pt);
			}

			constexpr auto operator->() const& noexcept {
				_ASSERT(*this);
				return m_pt;
			}

		private:
			std::remove_reference_t<TRef>* m_pt = nullptr;
		};
	}
	using no_adl::optional;

	template <typename T>
	using optional_reference_or_value = std::conditional_t<
		tc::empty_type<std::remove_cvref_t<T>> || std::is_rvalue_reference<T&&>::value,
		tc::optional<std::remove_cvref_t<T>>,
		tc::optional<T>
	>;

	template <typename T> requires (!tc::empty_type<T>)
	[[nodiscard]] constexpr optional_reference_or_value<T&> make_optional_reference_or_value(T& ref) noexcept {
		return ref;
	}
	template <typename T> requires tc::empty_type<std::remove_cvref_t<T>> || std::is_rvalue_reference<T&&>::value
	[[nodiscard]] constexpr optional_reference_or_value<T> make_optional_reference_or_value(T&& ref) noexcept {
		return {std::in_place, tc_move_if_owned(ref)};
	}
}

namespace tc {
	template<typename Optional, typename T>
	[[nodiscard]] constexpr decltype(auto) value_or( Optional&& optional, T&& t ) MAYTHROW {
		if constexpr( tc::instance_or_derived<std::remove_reference_t<T>, tc::make_lazy> ) {
			static_assert( !tc::has_actual_common_reference<decltype(*std::declval<Optional>()), T&&> ); // Should value_or(std::optional<make_lazy<T>>(), make_lazy<T>()) return make_lazy<T>&& or T?
			return tc_conditional_prvalue_as_val(optional, *tc_move_if_owned(optional), tc_move_if_owned(t)());
		} else {
			return tc_conditional_rvalue_as_ref(optional, *tc_move_if_owned(optional), tc_move_if_owned(t));
		}
	}

	template<typename Func>
	auto not_singleton_or(auto&& t, Func func) noexcept->decltype(func()) {
		if( tc::explicit_cast<bool>(t) ) {
			return tc_move_if_owned(t);
		} else {
			return func();
		}
	}

	template<typename Exception>
	decltype(auto) throw_if_null(auto&& t) THROW(Exception) {
		if( tc::explicit_cast<bool>(t) ) {
			return tc_move_if_owned(t);
		} else {
			throw Exception();
		}
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// and_then

	namespace and_then_detail {
		template<typename T, typename TypeList, typename=void>
		struct and_then_result_impl /*not final*/ {};

		template<typename T>
		struct and_then_result_impl<T, tc::type::list<>> /*not final*/: tc::type::identity<tc::decay_t<T>> {};

		template<>
		struct and_then_result_impl<void, tc::type::list<>> /*not final*/: tc::type::identity<bool> {};

		TC_HAS_EXPR(dereference_operator, (T), *std::declval<T>())

		template<typename Func>
		constexpr auto invoke(Func&& func, tc::unused) return_decltype_xvalue_by_ref_MAYTHROW(
			tc::invoke(tc_move_if_owned(func)) // MAYTHROW
		)

		template<typename Func, has_dereference_operator T>
		constexpr auto invoke(Func&& func, T&& t) return_decltype_xvalue_by_ref_MAYTHROW(
			tc::invoke(tc_move_if_owned(func), *tc_move_if_owned(t)) // MAYTHROW
		)

		template<typename T, typename Func, typename ...FuncTail>
		struct and_then_result_impl<
			T,
			tc::type::list<Func, FuncTail...>,
			decltype(tc::and_then_detail::invoke(std::declval<std::remove_reference_t<Func>>(), std::declval<T>()), void())
		> /*not final*/: and_then_result_impl<
			decltype(tc::and_then_detail::invoke(std::declval<std::remove_reference_t<Func>>(), std::declval<T>())),
			tc::type::list<FuncTail...>
		> {};

		template<typename T, typename Func, typename... FuncTail>
		struct and_then_result final: and_then_result_impl<T, tc::type::list<Func, FuncTail...>> {};
	}

	namespace and_then_adl {
		DEFINE_ADL_TAG_TYPE(adl_tag);

		template<typename T, typename Func>
		auto and_then_impl(adl_tag_t, T&& t, Func func) noexcept(noexcept(tc::and_then_detail::invoke(tc_move(func), tc_move_if_owned(t))))
			-> typename tc::and_then_detail::and_then_result<T, Func>::type
		{
			if(t) {
				if constexpr(std::is_void<decltype(tc::and_then_detail::invoke(tc_move(func), tc_move_if_owned(t)))>::value) {
					tc::and_then_detail::invoke(tc_move(func), tc_move_if_owned(t)); // MAYTHROW
					return true;
				} else {
					return tc::and_then_detail::invoke(tc_move(func), tc_move_if_owned(t)); // MAYTHROW
				}
			} else {
				return {};
			}
		}

		template<typename T, typename Func, typename ...FuncTail>
		auto and_then_impl(adl_tag_t, T&& t, Func func, FuncTail&& ...funcTail) noexcept(noexcept(
			and_then_impl(adl_tag, tc::and_then_detail::invoke(tc_move(func), tc_move_if_owned(t)), tc_move_if_owned(funcTail)...)
		)) // noexcept operator cannot see and_then_impl itself without ADL
			-> typename tc::and_then_detail::and_then_result<T, Func, FuncTail...>::type
		{
			if(t) {
				return and_then_impl( // MAYTHROW
					adl_tag,
					tc::and_then_detail::invoke(tc_move(func), tc_move_if_owned(t)), // MAYTHROW
					tc_move_if_owned(funcTail)...
				);
			} else {
				return {};
			}
		}
	}

	template<typename T, typename... Func>
	auto and_then(T&& t, Func&&... func) return_decltype_MAYTHROW(
		tc::and_then_adl::and_then_impl(tc::and_then_adl::adl_tag, tc_move_if_owned(t), tc_move_if_owned(func)...)
	)

	// For `tc::and_then(opt, tc::chained(fn_make_optional, f))`.
	DEFINE_FN2(std::make_optional, fn_make_optional);
}

namespace tc::begin_end_adl {
	template<typename T>
	constexpr T* begin(begin_end_tag_t, std::optional<T>& ot) noexcept {
		return ot ? std::addressof(*ot) : nullptr;
	}
	template<typename T>
	constexpr T const* begin(begin_end_tag_t, std::optional<T> const& ot) noexcept {
		return ot ? std::addressof(*ot) : nullptr;
	}
	template<typename T>
	constexpr T* end(begin_end_tag_t, std::optional<T>& ot) noexcept {
		return ot ? std::addressof(*ot)+1 : nullptr;
	}
	template<typename T>
	constexpr T const* end(begin_end_tag_t, std::optional<T> const& ot) noexcept {
		return ot ? std::addressof(*ot)+1 : nullptr;
	}
}
