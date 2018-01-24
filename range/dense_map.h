//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
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

#pragma once
#include "array.h"
#include "enum.h"
#include "counting_range.h"
#ifdef TC_PRIVATE
#include "Library/Persistence/PersistentType.h"
#include "Library/Persistence/PersistentStruct.h"
#endif
#include <boost/iterator/transform_iterator.hpp>

namespace tc {
	struct transform_tag final {};

	template< typename Enum >
	struct all_values {
		using const_iterator = tc::counting_iterator<Enum>;
		using iterator = const_iterator;
		iterator begin() const& noexcept {
			return iterator( tc::contiguous_enum<Enum>::begin() );
		}
		iterator end() const& noexcept {
			return iterator( tc::contiguous_enum<Enum>::end() );
		}
	};

	template<>
	struct all_values<bool> {
		using const_iterator = boost::transform_iterator< fn_static_cast<bool>, tc::counting_iterator<unsigned char> >;
		using iterator = const_iterator;
		iterator begin() const& noexcept {
			return iterator( 0 );
		}
		iterator end() const& noexcept {
			return iterator( 2 );
		}
	};

	// all_values are views
	namespace is_view_adl_barrier {
		template< typename T >
		struct is_view_impl<all_values<T>> : std::true_type {};
	}

	namespace dense_map_adl_barrier {
		template< typename Key, typename Value >
		struct dense_map : tc::equality_comparable<dense_map<Key, Value>> {
		private:
			template<typename KeyOther, typename ValueOther>
			friend struct dense_map;

			using Array = tc::array<Value,  enum_count<Key>::value >;
			Array m_a;
		public:
			using dense_map_key_type = Key;

			// ctors
			dense_map() noexcept {}
			dense_map(boost::container::default_init_t) noexcept : m_a(boost::container::default_init_t{}) {}
			
			template<typename... Args>
			dense_map( tc::fill_tag, Args&&... val ) noexcept
			: m_a( tc::fill_tag(), std::forward<Args>(val)... ) {}

			template<typename Rng>
			dense_map(tc::range_tag, Rng&& rng) noexcept
			: m_a(std::forward<Rng>(rng)) {}

			// make sure forwarding ctor has at least two parameters, so no ambiguity with filling ctor and implicit copy/move ctors
			template< typename First, typename Second, typename... Args,
				std::enable_if_t<
					implicit_construction == elementwise_construction_restrictiveness<Value, First, Second, Args...>::value
					&& !std::is_same<tc::remove_cvref_t<First>, tc::fill_tag>::value
				>* =nullptr
			>
			constexpr dense_map(First&& first, Second&& second, Args&& ...args) noexcept(std::is_nothrow_constructible<Array, First&&, Second&&, Args&&...>::value)
			: m_a(tc::aggregate_tag(), std::forward<First>(first), std::forward<Second>(second), std::forward<Args>(args)...) {}

			template< typename First, typename Second, typename... Args,
				std::enable_if_t<
					explicit_construction == elementwise_construction_restrictiveness<Value, First, Second, Args...>::value
					&& !std::is_same<tc::remove_cvref_t<First>, tc::fill_tag>::value
				>* =nullptr
			>
			constexpr explicit dense_map(First&& first, Second&& second, Args&& ...args) noexcept(std::is_nothrow_constructible<Array, First&&, Second&&, Args&&...>::value)
			: m_a(tc::aggregate_tag(), std::forward<First>(first), std::forward<Second>(second), std::forward<Args>(args)...) {}

			template< typename Func >
			dense_map(func_tag, Func func) MAYTHROW
				: m_a(tc::func_tag(), [&func](std::size_t n) MAYTHROW -> Value { // force return of Value
					static_assert(tc::creates_no_reference_to_temporary<decltype(func(tc::contiguous_enum<Key>::begin() + n)), Value>::value, "for dense_map of references, func must return a reference to Value or derived type");
					return func(tc::contiguous_enum<Key>::begin() + n);
				})
			{}

			template< typename Value2 >
			explicit dense_map(Key keyPri, dense_map<Key, Value2> const& mapOther) noexcept(std::is_nothrow_constructible<dense_map, Value2 const&, Value2 const&>::value)
				: dense_map(mapOther[keyPri], mapOther[~keyPri])
			{}

			template< typename Value2 >
			explicit dense_map(Key keyPri, dense_map<Key, Value2>&& mapOther) noexcept(std::is_nothrow_constructible<dense_map, Value2, Value2>::value)
				: dense_map(tc_move_always(mapOther[keyPri]), tc_move_always(mapOther[~keyPri]))
			{}

			template< typename Func, typename Value2 >
			dense_map(transform_tag, dense_map<Key, Value2> const& mapOther, Func&& func) MAYTHROW
				: m_a(tc::transform(mapOther.m_a, std::forward<Func>(func)))
			{}

			template< typename Func, typename Value2 >
			dense_map(transform_tag, dense_map<Key, Value2>&& mapOther, Func&& func) MAYTHROW
				: m_a(tc::transform(tc_move(mapOther.m_a), std::forward<Func>(func)))
			{}

			template< typename ValuePri, typename ValueSec >
			constexpr dense_map(Key keyPri, ValuePri&& valPri, ValueSec&& valSec) MAYTHROW :
				dense_map(
					tc::contiguous_enum<Key>::begin() == keyPri ? std::forward<ValuePri>(valPri) : std::forward<ValueSec>(valSec),
					tc::contiguous_enum<Key>::begin() == keyPri ? std::forward<ValueSec>(valSec) : std::forward<ValuePri>(valPri)
				)
			{}
			template <typename Value2,
				std::enable_if_t<construction_restrictiveness<Value, Value2 const&>::value == implicit_construction>* =nullptr
			>
			dense_map(dense_map<Key, Value2> const& mapOther) MAYTHROW
				: dense_map(transform_tag{}, mapOther, tc::fn_explicit_cast<Value>())
			{}

			template <typename Value2,
				std::enable_if_t<construction_restrictiveness<Value, Value2 const&>::value == explicit_construction>* =nullptr
			>
			explicit dense_map(dense_map<Key, Value2> const& mapOther) MAYTHROW
				: dense_map(transform_tag{}, mapOther, tc::fn_explicit_cast_with_rounding<Value>()) // TODO: replace with tc::fn_explicit_cast as soon as geometry no longer relies on it
			{}

			template <typename Value2,
				std::enable_if_t<construction_restrictiveness<Value, Value2&&>::value == implicit_construction>* =nullptr
			>
			dense_map(dense_map<Key, Value2>&& mapOther) MAYTHROW
				: dense_map(transform_tag{}, tc_move(mapOther), tc::fn_explicit_cast<Value>())
			{}

			template <typename Value2,
				std::enable_if_t<construction_restrictiveness<Value, Value2&&>::value == explicit_construction>* =nullptr
			>
			explicit dense_map(dense_map<Key, Value2>&& mapOther) MAYTHROW
				: dense_map(transform_tag{}, tc_move(mapOther), tc::fn_explicit_cast_with_rounding<Value>()) // TODO: replace with tc::fn_explicit_cast as soon as geometry no longer relies on it
			{}

#if defined(__clang__) || (defined(_MSC_FULL_VER) && 191025102<=_MSC_FULL_VER)
			template <typename Value2,
				std::enable_if_t<!tc::is_safely_constructible<Value, Value2&&>::value>* =nullptr
			>
			dense_map(dense_map<Key, Value2> const&&) noexcept = delete;
#endif

			template <typename Value2,
				std::enable_if_t<tc::is_safely_assignable<Value&, Value2 const&>::value>* =nullptr
			>
			dense_map& operator=(dense_map<Key, Value2> const& rhs) & noexcept(std::is_nothrow_assignable<Array, typename dense_map<Key, Value2>::Array const&>::value) {
				m_a=rhs.m_a;
				return *this;
			}

			template <typename Value2,
				std::enable_if_t<tc::is_safely_assignable<Value&, Value2&&>::value>* =nullptr
			>
			dense_map& operator=(dense_map<Key, Value2>&& rhs) & noexcept(std::is_nothrow_assignable<Array, typename dense_map<Key, Value2>::Array&&>::value) {
				m_a=tc_move(rhs.m_a);
				return *this;
			}

			template< typename Func >
			dense_map< Key, tc::decayed_result_of_t< Func(Value) > > transform(Func&& func) const& MAYTHROW {
				return dense_map< Key, tc::decayed_result_of_t< Func(Value) > >(transform_tag{}, *this, std::forward<Func>(func));
			}

			// access
			Value& operator[](Key key) & noexcept {
				return m_a[key-tc::contiguous_enum<Key>::begin()];
			}
			Value const& operator[](Key key) const& noexcept {
				return m_a[key-tc::contiguous_enum<Key>::begin()];
			}
			Value&& operator[](Key key) && noexcept {
				return tc_move(m_a)[key-tc::contiguous_enum<Key>::begin()];
			}
			Value const&& operator[](Key key) const&& noexcept {
				return static_cast<Value const&&>((*this)[key]);
			}

			// comparison
			friend bool operator==( dense_map const& lhs, dense_map const& rhs ) noexcept {
				return lhs.m_a == rhs.m_a;
			}

			// iterators
			using iterator = typename boost::range_iterator< Array >::type;
			using const_iterator = typename boost::range_iterator< Array const >::type;
			const_iterator begin() const& noexcept {
				return boost::begin(m_a);
			}
			const_iterator end() const& noexcept {
				return boost::end(m_a);
			}
			iterator begin() & noexcept {
				return boost::begin(m_a);
			}
			iterator end() & noexcept {
				return boost::end(m_a);
			}
			// persistence
#ifdef TC_PRIVATE
			friend void LoadType( dense_map& dm, CXmlReader& loadhandler ) THROW(ExLoadFail) {
				LoadType( dm.m_a, loadhandler ); // THROW(ExLoadFail)
			}
			friend void SaveType( dense_map const& dm, CSaveHandler& savehandler) MAYTHROW {
				SaveType( dm.m_a, savehandler );
			}

			// error reporting
			friend SReportStream& operator<<(SReportStream& rs, dense_map const& dm) noexcept {
				return rs << dm.m_a;
			}
#endif

			template<typename HashAlgorithm>
			friend void hash_append(HashAlgorithm& h, dense_map const& dm) noexcept {
				tc::hash_append(h, dm.m_a);
			}
		};

		template< typename Value >
		struct dense_map<bool,Value> : tc::equality_comparable<dense_map<bool,Value>> {
		private:
			template<typename KeyOther, typename ValueOther>
			friend struct dense_map;

			using Array = tc::array<Value,2>;
			Array m_a;
		public:
			using dense_map_key_type=bool;

			// ctors
			dense_map() noexcept {}
			dense_map(boost::container::default_init_t) noexcept : m_a(boost::container::default_init_t{}) {}

			template<typename... Args>
			dense_map( tc::fill_tag, Args&&... val ) noexcept
				:	m_a(tc::fill_tag{}, std::forward<Args>(val)...)
			{}

			template< typename Value0, typename Value1,
				std::enable_if_t<
					implicit_construction == elementwise_construction_restrictiveness<Value, Value0, Value1>::value
					&& !std::is_same<tc::remove_cvref_t<Value0>, tc::fill_tag>::value
				>* =nullptr
			>
			constexpr dense_map( Value0&& val0, Value1&& val1 ) noexcept(std::is_nothrow_constructible<Array, Value0&&, Value1&&>::value)
			:	m_a(tc::aggregate_tag(), std::forward<Value0>(val0),std::forward<Value1>(val1))
			{}

			template< typename Value0, typename Value1,
				std::enable_if_t<
					explicit_construction == elementwise_construction_restrictiveness<Value, Value0, Value1>::value
					&& !std::is_same<tc::remove_cvref_t<Value0>, tc::fill_tag>::value
				>* =nullptr
			>
			constexpr explicit dense_map( Value0&& val0, Value1&& val1 ) noexcept(std::is_nothrow_constructible<Array, Value0&&, Value1&&>::value)
			:	m_a(tc::aggregate_tag(),std::forward<Value0>(val0),std::forward<Value1>(val1))
			{}

			template< typename Func >
			dense_map(func_tag, Func func) noexcept
				: m_a(tc::func_tag(), [&func](std::size_t n) noexcept ->Value { // force return of Value
					static_assert(tc::creates_no_reference_to_temporary<decltype(func(0!=n)), Value>::value, "for dense_map of references, func must return a reference to Value or derived type");
					return func(0!=n);
				})
			{}

			template< typename Func, typename Value2 >
			dense_map(transform_tag, dense_map<bool, Value2> const& mapOther, Func&& func) noexcept
				: m_a(tc::transform(mapOther.m_a, std::forward<Func>(func)))
			{}

			template< typename ValuePri, typename ValueSec >
			constexpr dense_map(bool keyPri, ValuePri&& valPri, ValueSec&& valSec) noexcept :
				dense_map(
					keyPri ? std::forward<ValueSec>(valSec) : std::forward<ValuePri>(valPri),
					keyPri ? std::forward<ValuePri>(valPri) : std::forward<ValueSec>(valSec)
				)
			{}

			template< typename Func >
			dense_map< bool, tc::decayed_result_of_t< Func(Value) > > transform(Func func) const& MAYTHROW {
				return dense_map< bool, tc::decayed_result_of_t< Func(Value) > >(transform_tag{}, *this, std::forward<Func>(func));
			}

			// access
			Value & operator[]( bool key ) & noexcept {
				return m_a[key];
			}
			Value const& operator[]( bool key ) const& noexcept {
				return m_a[key];
			}

			// comparison
			friend bool operator==( dense_map const& lhs, dense_map const& rhs ) noexcept {
				return lhs.m_a == rhs.m_a;
			}

			// iterators
			using iterator = typename boost::range_iterator< Array >::type;
			using const_iterator = typename boost::range_iterator< Array const >::type;
			const_iterator begin() const& noexcept {
				return boost::begin(m_a);
			}
			const_iterator end() const& noexcept {
				return boost::end(m_a);
			}
			iterator begin() & noexcept {
				return boost::begin(m_a);
			}
			iterator end() & noexcept {
				return boost::end(m_a);
			}

#ifdef TC_PRIVATE
			// persistence
			friend void LoadType( dense_map& dm, CXmlReader& loadhandler ) THROW(ExLoadFail) {
				LoadType( dm.m_a, loadhandler ); // THROW(ExLoadFail)
			}
			friend void SaveType( dense_map const& dm, CSaveHandler& savehandler) MAYTHROW {
				SaveType( dm.m_a, savehandler );
			}

			// error reporting
			friend SReportStream& operator<<(SReportStream& rs, dense_map const& dm) noexcept {
				return rs << dm.m_a;
			}
#endif
		};
	}
	using dense_map_adl_barrier::dense_map;

	BOOST_MPL_HAS_XXX_TRAIT_DEF(dense_map_key_type)

	template<typename Map, typename Func, std::enable_if_t<has_dense_map_key_type<std::remove_reference_t<std::remove_cv_t<Map>>>::value>* = nullptr>
	auto map_for_each(Map && map, Func func) MAYTHROW {
		using Key = typename std::remove_reference_t<Map>::dense_map_key_type;
		return tc::for_each(tc::all_values<Key>(), [&](Key const key) MAYTHROW->decltype(auto) {
			return func(key, std::forward<Map>(map)[key]);
		});
	}

	////////////////////
	// special RangeReturns for dense_maps

	template< typename DenseMap >
	struct return_element_key final {
		using type = typename std::remove_reference_t<DenseMap>::dense_map_key_type;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<DenseMap> >::type it, DenseMap&& rng, Ref&&) noexcept {
			return tc::contiguous_enum<type>::begin() + (it - boost::begin(rng));
		}
		static type pack_no_element(DenseMap&&) noexcept {
			_ASSERTFALSE;
			return tc::contiguous_enum<type>::begin();
		}
	};

	template< typename DenseMap >
	struct return_element_key_or_end final {
		using type = typename std::remove_reference_t<DenseMap>::dense_map_key_type;

		template<typename Ref>
		static type pack_element(typename boost::range_iterator< std::remove_reference_t<DenseMap> >::type it, DenseMap&& rng, Ref&&) noexcept {
			return tc::contiguous_enum<type>::begin() + (it - boost::begin(rng));
		}
		static type pack_no_element(DenseMap&&) noexcept {
			return tc::contiguous_enum<type>::end();
		}
	};

	///////////////////
	// all_values specialization for 2-key dense_map, TODO: generalize
	template< typename Key, typename Value >
	struct all_values<tc::dense_map<Key,Value> > final {
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

	template <typename Key, typename Value>
	struct decay<tc::dense_map<Key, Value>> {
		using type = tc::dense_map<Key, tc::decay_t<Value>>;
	};
}

#define TC_DENSE_MAP_SUPPORT(class_name) \
	class_name() noexcept : base_(/*boost::container::default_init_t{}*/) {}; /*leave fundamental Value uninitialized*/ \
	\
	/* inherit default ctor and constructors with at least two arguments from dense_map*/ \
	template <typename A0, typename A1, typename ...Args, \
		std::enable_if_t< implicit_construction==construction_restrictiveness<base_, A0, A1, Args...>::value>* =nullptr \
	> \
	constexpr class_name(A0&& a0, A1&& a1, Args&& ... args) noexcept(std::is_nothrow_constructible<base_, A0, A1, Args...>::value) \
		: base_(std::forward<A0>(a0), std::forward<A1>(a1), std::forward<Args>(args)...) \
	{} \
	\
	template <typename A0, typename A1, typename ...Args, \
		std::enable_if_t< explicit_construction==construction_restrictiveness<base_, A0, A1, Args...>::value>* =nullptr \
	> \
	constexpr explicit class_name(A0&& a0, A1&& a1, Args&& ... args) noexcept(std::is_nothrow_constructible<base_, A0, A1, Args...>::value) \
		: base_(std::forward<A0>(a0), std::forward<A1>(a1), std::forward<Args>(args)...) \
	{} \
	\
	/* inherit implicit copy and move constructors from dense_map (only if argument is actual class_name<T>)*/ \
	template <typename T2, \
		std::enable_if_t<implicit_construction==construction_restrictiveness<base_, typename class_name<T2>::base_ const&>::value>* =nullptr \
	> \
	class_name(class_name<T2> const& other) noexcept(std::is_nothrow_constructible<base_, typename class_name<T2>::base_ const&>::value) \
		: base_(tc::base_cast<typename class_name<T2>::base_>(other)) \
	{} \
	\
	template <typename T2, \
		std::enable_if_t<implicit_construction==construction_restrictiveness<base_, typename class_name<T2>::base_&&>::value>* =nullptr \
	> \
	class_name(class_name<T2>&& other) noexcept(std::is_nothrow_constructible<base_, typename class_name<T2>::base_&&>::value) \
		: base_(tc::base_cast<typename class_name<T2>::base_>(tc_move(other))) \
	{} \
	\
	template <typename T2, \
		std::enable_if_t<!tc::is_safely_constructible<base_, typename class_name<T2>::base_ const&&>::value>* =nullptr \
	> \
	class_name(class_name<T2> const&& other) = delete; /*explicitly delete this constructor, otherwise class_name(class_name<T2> const&) above may be chosen*/ \
	\
	/* inherit implicit and explicit copy and move constructors from dense_map (only if argument is actual dense_map<base_::dense_map_key_type, T>) as explict constructors*/ \
	template <typename K, typename T2, \
		std::enable_if_t<tc::is_safely_constructible<base_, tc::dense_map<K, T2> const&>::value>* =nullptr \
	> \
	explicit class_name(tc::dense_map<K, T2> const& other) noexcept(std::is_nothrow_constructible<base_, tc::dense_map<K, T2> const&>::value) \
		: base_(other) \
	{ \
		static_assert(std::is_same<K, typename base_::dense_map_key_type>::value); /*Should use tc::dense_map<dense_map_key_type, T2> for parameter above, but MSVC turns this into dense_map<int, T2>*/ \
	} \
	template <typename K, typename T2, \
		std::enable_if_t<tc::is_safely_constructible<base_, tc::dense_map<K, T2>&&>::value>* =nullptr \
	> \
	explicit class_name(tc::dense_map<K, T2>&& other) noexcept(std::is_nothrow_constructible<base_, tc::dense_map<K, T2>&&>::value) \
		: base_(other) \
	{ \
		static_assert(std::is_same<K, typename base_::dense_map_key_type>::value); /*Should use tc::dense_map<dense_map_key_type, T2> for parameter above, but MSVC turns this into dense_map<int, T2>*/ \
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
	}
