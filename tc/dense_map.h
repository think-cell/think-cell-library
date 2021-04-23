
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "array.h"
#include "enum.h"
#include "counting_range.h"
#include "tag_type.h"
#include "as_lvalue.h"
#include "zip_range.h"

#ifdef TC_PRIVATE
#include "Library/Persistence/types.h"
#endif

namespace tc {
	namespace no_adl {
		template<>
		struct [[nodiscard]] all_values<bool> final {
			static auto constexpr c_rngsete = tc::transform(tc::iota(0, 2), tc::fn_static_cast<bool>());

		public:
			static constexpr auto begin() noexcept {
				return tc::begin(c_rngsete);
			}
			static constexpr auto end() noexcept {
				return tc::end(c_rngsete);
			}

			using const_iterator = decltype(tc::begin(c_rngsete));
			using iterator = const_iterator;

			static constexpr std::size_t index_of(bool b) noexcept {
				return tc::underlying_cast(b);
			}
		};

		template<>
		struct constexpr_size_base<all_values<bool>> : std::integral_constant<std::size_t, 2> {};
	}

	// all_values are views
	namespace dense_map_adl {
#ifdef TC_PRIVATE
		template< typename Key, typename Value >
		struct dense_map;

		template< typename Key, typename Value >
		void LoadType_impl( dense_map<Key, Value>& dm, CXmlReader& loadhandler ) THROW(ExLoadFail);
#endif
		template< typename Key, typename Value >
		struct dense_map : tc::equality_comparable<dense_map<Key, Value>> {
		private:
			template<typename KeyOther, typename ValueOther>
			friend struct dense_map;

			template<typename ValueOther>
			using other_dense_map = dense_map<Key, ValueOther>;

			using Array = tc::array<Value,  tc::constexpr_size<tc::all_values<Key>>::value >;
			Array m_a;
		public:
			using dense_map_key_type = Key;

			// ctors - VS compiler 19.15.26726 didn't sfinae correctly on Array, using decltype(m_a) instead
			template<ENABLE_SFINAE, std::enable_if_t<std::is_default_constructible<SFINAE_TYPE(decltype(m_a))>::value>* = nullptr>
			dense_map() noexcept(std::is_nothrow_default_constructible<decltype(m_a)>::value) {}

			template<ENABLE_SFINAE, std::enable_if_t<std::is_constructible<SFINAE_TYPE(decltype(m_a)), boost::container::default_init_t>::value>* = nullptr>
			explicit dense_map(boost::container::default_init_t) noexcept(std::is_nothrow_constructible<decltype(m_a), boost::container::default_init_t>::value)
				: m_a(boost::container::default_init) {}

			template<typename... Args, std::enable_if_t<0 < sizeof...(Args)>* = nullptr> // dense_map(fill_tag) could exist, but it should be explicit
			constexpr dense_map( tc::fill_tag_t, Args&&... val ) noexcept
			: m_a(tc::fill_tag, std::forward<Args>(val)...) {} // TODO: tc::explicit_cast to support std::array once copy elision works reliably

			template<typename Rng>
			dense_map(tc::range_tag_t, Rng&& rng) noexcept : MEMBER_INIT_CAST( m_a, std::forward<Rng>(rng) ) {}

			// make sure forwarding ctor has at least two parameters, so no ambiguity with filling ctor and implicit copy/move ctors
			template< typename First, typename Second, typename... Args,
				std::enable_if_t<
					tc::econstructionIMPLICIT == tc::elementwise_construction_restrictiveness<Value, First&&, Second&&, Args&&...>::value
					&& !std::is_same<tc::remove_cvref_t<First>, tc::fill_tag_t>::value
				>* =nullptr
			>
			constexpr dense_map(First&& first, Second&& second, Args&& ...args) noexcept(std::is_nothrow_constructible<Array, tc::aggregate_tag_t,  First&&, Second&&, Args&&...>::value)
			: m_a(tc::aggregate_tag, std::forward<First>(first), std::forward<Second>(second), std::forward<Args>(args)...) {} // TODO: tc::explicit_cast to support std::aray once copy elision works reliably

			template< typename First, typename Second, typename... Args,
				std::enable_if_t<
					tc::econstructionEXPLICIT == tc::elementwise_construction_restrictiveness<Value, First&&, Second&&, Args&&...>::value
					&& !std::is_same<tc::remove_cvref_t<First>, tc::fill_tag_t>::value
				>* =nullptr
			>
			constexpr explicit dense_map(First&& first, Second&& second, Args&& ...args) noexcept(std::is_nothrow_constructible<Array, tc::aggregate_tag_t, First&&, Second&&, Args&&...>::value)
				: MEMBER_INIT_CAST( m_a, tc::aggregate_tag, std::forward<First>(first), std::forward<Second>(second), std::forward<Args>(args)... )
			{}

			template< typename FuncTag, typename Func, std::enable_if_t<std::conjunction<
				std::is_same<FuncTag, tc::func_tag_t>,
				tc::is_invocable<Func&, Key>
			>::value>* = nullptr >
			constexpr dense_map(FuncTag, Func func) MAYTHROW
				: MEMBER_INIT_CAST( m_a, tc::func_tag, [&func](std::size_t n) MAYTHROW -> Value { // force return of Value
					STATICASSERTSAME(decltype(tc_at_nodebug(tc::all_values<Key>(), n))&&, Key&&);
					static_assert(
						std::is_same<Value, decltype(func(std::declval<Key>()))>::value || // guaranteed copy elision, Value does not need to be copy/move constructible
						tc::is_safely_constructible<Value, decltype(func(std::declval<Key>()))>::value
					);
					return func(tc_at_nodebug(tc::all_values<Key>(), n));
				} )
			{}

		public:
			template< typename FuncTag, typename Func, std::enable_if_t<std::conjunction<
				std::is_same<FuncTag, tc::func_tag_t>,
				std::negation<tc::is_invocable<Func&, Key>>
			>::value>* = nullptr >
			constexpr dense_map(FuncTag, Func func) MAYTHROW
				: dense_map(tc::func_tag, [&](auto const key) MAYTHROW -> Value {
					return {tc::func_tag, [&](auto const... keys) return_decltype_MAYTHROW( func(key, keys...) )};
				})
			{}

			template< typename Value2 >
			explicit dense_map(Key keyPri, other_dense_map<Value2> const& mapOther) noexcept(std::is_nothrow_constructible<dense_map, Value2 const&, Value2 const&>::value)
				: dense_map(mapOther[keyPri], mapOther[~keyPri])
			{}

			template< typename Value2 >
			explicit dense_map(Key keyPri, other_dense_map<Value2>&& mapOther) noexcept(std::is_nothrow_constructible<dense_map, Value2, Value2>::value)
				: dense_map(tc_move_always(mapOther[keyPri]), tc_move_always(mapOther[~keyPri]))
			{}

			template< typename Func, typename Value2 >
			constexpr dense_map(transform_tag_t, other_dense_map<Value2> const& mapOther, Func&& func) MAYTHROW
				: MEMBER_INIT_CAST( m_a, tc::transform(mapOther.m_a, std::forward<Func>(func)) )
			{}

			template< typename Func, typename Value2 >
			constexpr dense_map(transform_tag_t, other_dense_map<Value2>&& mapOther, Func func) MAYTHROW
				: MEMBER_INIT_CAST( m_a,
					// TODO rvalue elements for rvalue ranges: tc::transform(tc_move(mapOther.m_a), std::forward<Func>(func))
					tc::transform(mapOther.m_a, [&](auto& elem) MAYTHROW -> decltype(auto) {
						return func(static_cast<Value2&&>(elem));
					})
				)
			{}

			DEFINE_MEMBER_TRANSFORM(other_dense_map, tc::type::deducible_identity_t, Value)

			template< typename ValuePri, typename ValueSec >
			constexpr dense_map(Key keyPri, ValuePri&& valPri, ValueSec&& valSec) MAYTHROW :
				dense_map(
					CONDITIONAL_RVALUE_AS_REF(tc_front_nodebug(tc::all_values<Key>()) == keyPri, std::forward<ValuePri>(valPri), std::forward<ValueSec>(valSec)),
					CONDITIONAL_RVALUE_AS_REF(tc_front_nodebug(tc::all_values<Key>()) == keyPri, std::forward<ValueSec>(valSec), std::forward<ValuePri>(valPri))
				)
			{}
			template <typename Value2,
				std::enable_if_t<tc::econstructionIMPLICIT==tc::construction_restrictiveness<Value, Value2 const&>::value>* =nullptr
			>
			constexpr dense_map(other_dense_map<Value2> const& mapOther) noexcept(std::is_nothrow_constructible<Value, Value2 const&>::value)
				: MEMBER_INIT_CAST( m_a, mapOther.m_a )
			{}

			template <typename Value2,
				std::enable_if_t<tc::econstructionEXPLICIT==tc::construction_restrictiveness<Value, Value2 const&>::value>* =nullptr
			>
			constexpr explicit dense_map(other_dense_map<Value2> const& mapOther) noexcept(std::is_nothrow_constructible<dense_map, transform_tag_t, decltype(mapOther), tc::fn_explicit_cast_with_rounding<Value>>::value)
				: dense_map(transform_tag, mapOther, tc::fn_explicit_cast_with_rounding<Value>()) // TODO: replace with tc::fn_explicit_cast as soon as geometry no longer relies on it
			{}

			template <typename Value2,
				std::enable_if_t<tc::econstructionIMPLICIT==tc::construction_restrictiveness<Value, Value2&&>::value>* =nullptr
			>
			constexpr dense_map(other_dense_map<Value2>&& mapOther) noexcept(std::is_nothrow_constructible<Value, Value2&&>::value)
				: MEMBER_INIT_CAST( m_a, tc_move(mapOther).m_a )
			{}

			template <typename Value2,
				std::enable_if_t<tc::econstructionEXPLICIT==tc::construction_restrictiveness<Value, Value2&&>::value>* =nullptr
			>
			constexpr explicit dense_map(other_dense_map<Value2>&& mapOther) noexcept(std::is_nothrow_constructible<dense_map, transform_tag_t, decltype(tc_move(mapOther)), tc::fn_explicit_cast_with_rounding<Value>>::value)
				: dense_map(transform_tag, tc_move(mapOther), tc::fn_explicit_cast_with_rounding<Value>()) // TODO: replace with tc::fn_explicit_cast as soon as geometry no longer relies on it
			{}

			template <typename Value2,
				std::enable_if_t<tc::is_safely_assignable<Value&, Value2 const&>::value>* =nullptr
			>
			dense_map& operator=(other_dense_map<Value2> const& rhs) & noexcept(std::is_nothrow_assignable<Array, typename other_dense_map<Value2>::Array const&>::value) {
				m_a=rhs.m_a;
				return *this;
			}

			template <typename Value2,
				std::enable_if_t<tc::is_safely_assignable<Value&, Value2&&>::value>* =nullptr
			>
			dense_map& operator=(other_dense_map<Value2>&& rhs) & noexcept(std::is_nothrow_assignable<Array, typename other_dense_map<Value2>::Array&&>::value) {
				tc::cont_assign(m_a, tc_move(rhs.m_a));
				return *this;
			}

			// access
			[[nodiscard]] constexpr Value& operator[](Key key) & noexcept {
				if constexpr (std::is_reference<Value>::value) {
					return m_a[tc::all_values<Key>::index_of(key)];
				} else {
					return tc::at(m_a, tc::all_values<Key>::index_of(key));
				}
			}
			[[nodiscard]] constexpr Value const& operator[](Key key) const& noexcept {
				if constexpr (std::is_reference<Value>::value) {
					return m_a[tc::all_values<Key>::index_of(key)];
				} else {
					return tc::at(m_a, tc::all_values<Key>::index_of(key));
				}
			}
			[[nodiscard]] constexpr Value&& operator[](Key key) && noexcept {
				return static_cast<Value &&>((*this)[key]);
			}
			[[nodiscard]] constexpr Value const&& operator[](Key key) const&& noexcept {
				return static_cast<Value const&&>((*this)[key]);
			}

		private:
			template<typename Self>
			static constexpr auto enumerate_(Self&& self) noexcept {
				if constexpr( std::is_move_constructible<Self>::value ) {
					return tc::zip(tc::all_values<Key>(), std::forward<Self>(self));
				} else {
					static_assert( !std::is_lvalue_reference<Self>::value );
					// Cannot static_assert(dependent_false<Self>::value) here.
					// tc::zip(..., NonMovable()) does not compile, but this overload gets instantiated even if not used.
				}
			}

		public:
			RVALUE_THIS_OVERLOAD_MOVABLE_MUTABLE_REF(enumerate)

			// comparison
			template<typename Key_, typename LHS, typename RHS>
			friend constexpr bool operator==( dense_map<Key_, LHS> const& lhs, dense_map<Key_, RHS> const& rhs ) noexcept;

			// iterators
			using iterator = typename boost::range_iterator< Array >::type;
			using const_iterator = typename boost::range_iterator< Array const >::type;
			constexpr const_iterator begin() const& noexcept {
				return tc::begin(m_a);
			}
			constexpr const_iterator end() const& noexcept {
				return tc::end(m_a);
			}
			constexpr iterator begin() & noexcept {
				return tc::begin(m_a);
			}
			constexpr iterator end() & noexcept {
				return tc::end(m_a);
			}
			
#ifdef _DEBUG
			friend void uninitialize_impl(dense_map& dm) noexcept {
				UNINITIALIZED(dm.m_a);
			}
#endif

#ifdef TC_PRIVATE
			// persistence
			friend void LoadType_impl<>( dense_map<Key, Value>& dm, CXmlReader& loadhandler ) THROW(ExLoadFail);
			void DoSave(CSaveHandler& savehandler) const& MAYTHROW;

			// error reporting
			friend constexpr Array const& debug_output_impl(dense_map const& dm) noexcept {
				return dm.m_a;
			}

		private:
			template<typename HashAlgorithm>
			void hash_append_impl(HashAlgorithm& h) const& noexcept;

			template<typename HashAlgorithm>
			friend void hash_append_impl(HashAlgorithm& h, dense_map const& dm) noexcept {
				dm.hash_append_impl(h);
			}
#endif
		};

		template<typename Key_, typename LHS, typename RHS>
		[[nodiscard]] constexpr bool operator==(dense_map<Key_, LHS> const& lhs, dense_map<Key_, RHS> const& rhs) noexcept {
			return EQUAL_MEMBERS(m_a);
		}

	} // namespace dense_map_adl
	using dense_map_adl::dense_map;

	namespace no_adl {
		template<typename DenseMapOrDerived>
		struct constexpr_size_base<DenseMapOrDerived, std::enable_if_t<tc::is_instance_or_derived<dense_map, DenseMapOrDerived>::value>>
			: constexpr_size<tc::all_values<tc::type::front_t<typename is_instance_or_derived<dense_map, DenseMapOrDerived>::arguments>>>
		{};
	} // namespace no_adl

	BOOST_MPL_HAS_XXX_TRAIT_DEF(dense_map_key_type)
	
	template <typename Map, typename T, typename Enable = void>
	struct is_dense_map_with_key final : std::false_type {};
	
	template <typename Map, typename T>
	struct is_dense_map_with_key<Map, T, std::enable_if_t<tc::has_dense_map_key_type<Map>::value>> final
		: std::bool_constant<std::is_same<typename Map::dense_map_key_type, T>::value>
	{};

	////////////////////
	// special RangeReturns for dense_maps

	struct return_element_key final {
		static constexpr bool requires_iterator = true;

		template<typename It, typename DenseMap, typename... Ref>
		static auto pack_element(It&& it, DenseMap&& rng, Ref&&...) noexcept {
			return tc_at_nodebug(tc::all_values<typename std::remove_reference_t<DenseMap>::dense_map_key_type>(), it - tc::begin(rng));
		}
		template<typename DenseMap>
		static auto pack_no_element(DenseMap&&) noexcept {
			_ASSERTFALSE;
			return tc_front_nodebug(tc::all_values<typename std::remove_reference_t<DenseMap>::dense_map_key_type>());
		}
	};

	struct return_element_key_or_none final {
		static constexpr bool requires_iterator = true;

		template<typename It, typename DenseMap, typename... Ref>
		static std::optional<typename std::remove_reference_t<DenseMap>::dense_map_key_type> pack_element(It&& it, DenseMap&& rng, Ref&&...) noexcept {
			return tc_at_nodebug(tc::all_values<typename std::remove_reference_t<DenseMap>::dense_map_key_type>(), it - tc::begin(rng));
		}
		template<typename DenseMap>
		static std::optional<typename std::remove_reference_t<DenseMap>::dense_map_key_type> pack_no_element(DenseMap&&) noexcept {
			return std::nullopt;
		}
	};

	namespace no_adl {
		///////////////////
		// all_values specialization for 2-key dense_map, TODO: generalize
		template< typename Key, typename Value >
		struct [[nodiscard]] all_values<tc::dense_map<Key,Value> > final {
			using reference = tc::dense_map<Key, Value>;
			using const_reference = reference;

			template< typename Func >
			auto operator()(Func func) const& MAYTHROW {
				return tc::for_each( tc::all_values<Value>(), [&]( Value value1 ) MAYTHROW {
					return tc::for_each( tc::all_values<Value>(), [&]( Value value2 ) MAYTHROW {
						return func( tc::dense_map<Key,Value>(value1,tc_move(value2)) );
					});
				});
			}
		};

		template<typename Key, typename Value>
		struct constexpr_size_base<tc::all_values<tc::dense_map<Key,Value>>, void>
			: std::integral_constant<std::size_t, tc::pow(tc::constexpr_size<tc::all_values<Value>>::value, tc::constexpr_size<tc::all_values<Key>>::value)>
		{};
	}

	template <typename Key, typename Func>
	[[nodiscard]] auto make_dense_map(tc::func_tag_t, Func&& func) return_ctor_MAYTHROW(
		tc::dense_map<Key BOOST_PP_COMMA() tc::decay_t<decltype(tc::as_lvalue(tc::decay_copy(func))(tc_front_nodebug(tc::all_values<Key>())))>>, ( tc::func_tag, std::forward<Func>(func) )
	)

	template <typename Key, /*always deduce Ts, otherwise use dense_map<Key, T>*/int = 0, typename... Ts>
	[[nodiscard]] constexpr auto make_dense_map(tc::fill_tag_t, Ts&&... ts) noexcept {
		return tc::dense_map<Key, tc::common_type_t<Ts...>>(tc::fill_tag, std::forward<Ts>(ts)...);
	}

	template <typename Key, /*always deduce Ts, otherwise use dense_map<Key, T>*/int = 0, typename... Ts>
	[[nodiscard]] constexpr auto make_dense_map(Ts&&... ts) noexcept {
		return tc::dense_map<Key, tc::common_type_t<Ts...>>(std::forward<Ts>(ts)...);
	}

	namespace less_key_adl {
		template<typename Key, typename ValueLhs, typename ValueRhs>
		bool less_key_helper(less_key_tag_t, tc::dense_map<Key, ValueLhs> const& lhs, tc::dense_map<Key, ValueRhs> const& rhs) noexcept {
			return tc::order::less==tc::lexicographical_compare_3way(lhs, rhs);
		}
	}
}

#define TC_DENSE_MAP_SUPPORT_1(class_name) \
	TC_DENSE_MAP_SUPPORT_2(class_name, tc::type::deducible_identity_t)

#define TC_DENSE_MAP_SUPPORT_2(class_name, value_template) \
	template <ENABLE_SFINAE, std::enable_if_t<std::is_default_constructible<SFINAE_TYPE(base_)>::value>* = nullptr> \
	class_name() noexcept(std::is_nothrow_default_constructible<base_>::value) \
		: base_() {} \
	\
	template <ENABLE_SFINAE, std::enable_if_t<std::is_constructible<SFINAE_TYPE(base_), boost::container::default_init_t>::value>* = nullptr> \
	explicit class_name(boost::container::default_init_t) noexcept(std::is_nothrow_constructible<base_, boost::container::default_init_t>::value) \
		: base_(boost::container::default_init) {} \
	\
	/* inherit default ctor and constructors with at least two arguments from dense_map*/ \
	template <typename A0, typename A1, typename ...Args, \
		std::enable_if_t< tc::econstructionIMPLICIT==tc::construction_restrictiveness<base_, A0&&, A1&&, Args&&...>::value>* =nullptr \
	> \
	constexpr class_name(A0&& a0, A1&& a1, Args&& ... args) noexcept(std::is_nothrow_constructible<base_, A0, A1, Args...>::value) \
		: base_(std::forward<A0>(a0), std::forward<A1>(a1), std::forward<Args>(args)...) \
	{} \
	\
	template <typename A0, typename A1, typename ...Args, \
		std::enable_if_t< tc::econstructionEXPLICIT==tc::construction_restrictiveness<base_, A0&&, A1&&, Args&&...>::value>* =nullptr \
	> \
	constexpr explicit class_name(A0&& a0, A1&& a1, Args&& ... args) noexcept(std::is_nothrow_constructible<base_, A0, A1, Args...>::value) \
		: base_(std::forward<A0>(a0), std::forward<A1>(a1), std::forward<Args>(args)...) \
	{} \
	\
	/* inherit implicit copy and move constructors from dense_map (only if argument is actual class_name<T>)*/ \
	template <typename T2, \
		std::enable_if_t<tc::econstructionIMPLICIT==tc::construction_restrictiveness<base_, typename class_name<T2>::base_ const&>::value>* =nullptr \
	> \
	class_name(class_name<T2> const& other) noexcept(std::is_nothrow_constructible<base_, typename class_name<T2>::base_ const&>::value) \
		: base_(tc::base_cast<typename class_name<T2>::base_>(other)) \
	{} \
	\
	template <typename T2, \
		std::enable_if_t<tc::econstructionIMPLICIT==tc::construction_restrictiveness<base_, typename class_name<T2>::base_&&>::value>* =nullptr \
	> \
	class_name(class_name<T2>&& other) noexcept(std::is_nothrow_constructible<base_, typename class_name<T2>::base_&&>::value) \
		: base_(tc::base_cast<typename class_name<T2>::base_>(tc_move(other))) \
	{} \
	\
	/* inherit implicit and explicit copy and move constructors from dense_map (only if argument is actual dense_map<base_::dense_map_key_type, T>) as explict constructors*/ \
	template <typename K, typename T2, \
		std::enable_if_t<tc::is_explicit_castable<base_, tc::dense_map<K, T2> const&>::value>* =nullptr \
	> \
	explicit class_name(tc::dense_map<K, T2> const& other) noexcept(std::is_nothrow_constructible<base_, tc::dense_map<K, T2> const&>::value) \
		: base_(other) \
	{ \
		STATICASSERTSAME(K, typename base_::dense_map_key_type); /*Should use tc::dense_map<dense_map_key_type, T2> for parameter above, but MSVC turns this into dense_map<int, T2>*/ \
	} \
	template <typename K, typename T2, \
		std::enable_if_t<tc::is_explicit_castable<base_, tc::dense_map<K, T2>&&>::value>* =nullptr \
	> \
	explicit class_name(tc::dense_map<K, T2>&& other) noexcept(std::is_nothrow_constructible<base_, tc::dense_map<K, T2>&&>::value) \
		: base_(other) \
	{ \
		STATICASSERTSAME(K, typename base_::dense_map_key_type); /*Should use tc::dense_map<dense_map_key_type, T2> for parameter above, but MSVC turns this into dense_map<int, T2>*/ \
	} \
	\
	/* inherit assignment */ \
	template <typename T2, \
		std::enable_if_t<tc::is_safely_assignable<base_&, typename class_name<T2>::base_ const&>::value>* =nullptr \
	> \
	class_name& operator=(class_name<T2> const& rhs) & noexcept(std::is_nothrow_assignable<base_, typename class_name<T2>::base_ const&>::value) { \
		tc::base_cast<base_>(*this)=tc::base_cast<typename class_name<T2>::base_>(rhs); \
		return *this; \
	} \
	\
	template <typename T2, \
		std::enable_if_t<tc::is_safely_assignable<base_&, typename class_name<T2>::base_&&>::value>* =nullptr \
	> \
	class_name& operator=(class_name<T2>&& rhs) & noexcept(std::is_nothrow_assignable<base_, typename class_name<T2>::base_&&>::value) { \
		tc::base_cast<base_>(*this)=tc::base_cast<typename class_name<T2>::base_>(tc_move(rhs)); \
		return *this; \
	} \
	\
	/* inherit transform */ \
	DEFINE_MEMBER_TRANSFORM(class_name, value_template)

#define TC_DENSE_MAP_SUPPORT(...) \
	TC_EXPAND(BOOST_PP_OVERLOAD(TC_DENSE_MAP_SUPPORT_, __VA_ARGS__)(__VA_ARGS__))

#define TC_DENSE_MAP_DEDUCTION_GUIDES_1(template_name) \
	TC_DENSE_MAP_DEDUCTION_GUIDES_2(template_name, tc::type::deducible_identity_t)

#define TC_DENSE_MAP_DEDUCTION_GUIDES_2(template_name, type_function) \
	template< typename Arg > \
	template_name(tc::fill_tag_t, Arg&&) -> template_name< type_function< tc::decay_t<Arg> > >; \
	template< typename Arg, typename... Args> \
	template_name(Arg&&, Args&&...) -> template_name< type_function< \
		std::enable_if_t<std::conjunction< std::is_same<tc::decay_t<Arg>, tc::decay_t<Args>>... >::value, tc::decay_t<Arg>> \
	> >; \
	template< typename Rng > \
	template_name(tc::range_tag_t, Rng&&) -> template_name< type_function< tc::range_value_t<Rng> > >;

#define TC_DENSE_MAP_DEDUCTION_GUIDES(...) \
	TC_EXPAND(BOOST_PP_OVERLOAD(TC_DENSE_MAP_DEDUCTION_GUIDES_, __VA_ARGS__)(__VA_ARGS__))
