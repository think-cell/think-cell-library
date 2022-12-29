
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
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
			optional(std::in_place_t, Args&& ... args) noexcept(noexcept(construct(std::forward<Args>(args)...)))
			: m_bValue(true) {
				construct(std::forward<Args>(args)...);
			}
			~optional() {
				reset();
			}
	
			void reset() & noexcept {
				if (tc::change(m_bValue, false)) { // allow querying the flag to prevent double-destruction
#ifdef _CHECKS
					_ASSERT(!m_bInsideDtor);
					scoped_assign(m_bInsideDtor, true);
#endif
					m_oValue.dtor();
				}
			}
			template<typename... Args>
			T& emplace(Args&& ... args) & noexcept(noexcept(construct(std::forward<Args>(args)...))) {
				reset();
				VERIFY(tc::change(m_bValue, true));
				construct(std::forward<Args>(args)...);
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
				_ASSERT(*this);
				return m_oValue.operator->();
			}
			T* operator->() & noexcept {
				_ASSERT(*this);
				return m_oValue.operator->();
			}
			T const& operator*() const& noexcept {
				_ASSERT(*this);
				return *m_oValue;
			}
			T const&& operator*() const&& noexcept {
				_ASSERT(*this);
				return *tc_move(m_oValue);
			}
			T& operator*() & noexcept {
				_ASSERT(*this);
				return *m_oValue;
			}
			T&& operator*() && noexcept {
				_ASSERT(*this);
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
			void construct(Args&& ... args) & noexcept(noexcept(std::declval<storage_for<T>&>().ctor(std::forward<Args>(args)...))) {
				_ASSERT(m_bValue); // allow querying the flag to prevent double-construction
				try {
					_ASSERT(!m_bInsideDtor);
					m_oValue.ctor(std::forward<Args>(args)...);
				} catch( ... ) {
					m_bValue = false;
					throw;
				}
			}
		};

		template<typename TRef> requires std::is_reference<TRef>::value
		struct optional<TRef> {
			template<typename U>
			friend struct optional;

			constexpr optional() noexcept = default;
			constexpr optional(std::nullopt_t) noexcept {}

			template<typename U> requires tc::is_safely_constructible<TRef, U&&>::value
			explicit(!tc::is_safely_convertible<U&&, TRef>::value) constexpr optional(U&& u) noexcept(noexcept(static_cast<TRef>(std::forward<U>(u))))
				: m_pt(std::addressof(tc::as_lvalue(static_cast<TRef>(std::forward<U>(u))))) // tc::is_safely_constructible makes sure no reference to temporary is created
			{}

			template<typename TOptional> requires
				tc::is_safely_constructible<TRef, tc::apply_cvref_t<tc::type::only_t<typename tc::is_instance<tc::no_adl::optional, std::remove_reference_t<TOptional>>::arguments>, TOptional&&>>::value
			explicit(!tc::is_safely_convertible<tc::apply_cvref_t<tc::type::only_t<typename tc::is_instance<tc::no_adl::optional, std::remove_reference_t<TOptional>>::arguments>, TOptional&&>, TRef>::value)
			constexpr optional(TOptional&& rhs) noexcept
				: m_pt([&]() noexcept {
					return rhs ? rhs.m_pt : nullptr;
				}())
			{}

			template<typename TOptional> requires
				tc::is_safely_constructible<TRef, tc::apply_cvref_t<tc::type::only_t<typename tc::is_instance<std::optional, std::remove_reference_t<TOptional>>::arguments>, TOptional&&>>::value
			explicit(!tc::is_safely_convertible<tc::apply_cvref_t<tc::type::only_t<typename tc::is_instance<std::optional, std::remove_reference_t<TOptional>>::arguments>, TOptional&&>, TRef>::value)
			constexpr optional(TOptional&& rhs) noexcept
				: m_pt([&]() noexcept {
					return rhs ? std::addressof(*rhs) : nullptr;
				}())
			{}

			constexpr optional& operator=(std::nullopt_t) /*no &*/ noexcept {
				this->reset();
				return *this;
			}

			template<typename U> requires tc::is_safely_constructible<TRef, U&&>::value
			constexpr TRef emplace(U&& u) /*no &*/ noexcept(noexcept(static_cast<TRef>(std::forward<U>(u)))) {
				m_pt = std::addressof(tc::as_lvalue(static_cast<TRef>(std::forward<U>(u)))); // tc::is_safely_constructible makes sure no reference to temporary is created
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
}

namespace tc {
	template<typename Optional, typename T>
	[[nodiscard]] constexpr decltype(auto) optional_value_or( Optional&& optional, T&& t ) noexcept {
		if constexpr( tc::is_instance_or_derived<tc::make_lazy, T>::value ) {
			static_assert( !tc::has_actual_common_reference<decltype(*std::declval<Optional>()), T&&> ); // Should optional_value_or(std::optional<make_lazy<T>>(), make_lazy<T>()) return make_lazy<T>&& or T?
			return CONDITIONAL_PRVALUE_AS_VAL(optional, *std::forward<Optional>(optional), std::forward<T>(t)());
		} else {
			return CONDITIONAL_RVALUE_AS_REF(optional, *std::forward<Optional>(optional), std::forward<T>(t));
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
}

namespace boost {
	template<typename T>
	struct range_mutable_iterator<std::optional<T>> {
		using type = T*;
	};
	template<typename T>
	struct range_const_iterator<std::optional<T>> {
		using type = T const*;
	};
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
