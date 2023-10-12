
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "../base/derivable.h"
#include "../base/generic_macros.h"
#include "../base/modified.h"
#include "../base/empty_chain.h"
#include "round.h"

namespace tc {
	#pragma push_macro("GENERIC_OP_BODY")
	#define GENERIC_OP_BODY(op, operation_body) \
		template< typename Lhs, typename Rhs, \
			typename Result = std::conditional_t<std::is_same<conversion_t<Lhs, Rhs>, Lhs>::value, Lhs&&, conversion_t<Lhs, Rhs>> \
		> \
			requires is_operation_available<Lhs&&, Rhs&&>::value \
		[[nodiscard]] friend constexpr Result operator op(Lhs&& lhs, Rhs&& rhs) noexcept { \
			static_assert(tc::decayed<conversion_t<Lhs, Rhs>>); \
			Result _ = tc_move_if_owned(lhs); \
			operation_body; \
			if constexpr( std::is_same<Result, Lhs&&>::value ) { \
				return tc_move_if_owned(_); \
			} else { \
				static_assert( \
					std::is_same<Result, tc::decay_t<Lhs>>::value \
					|| !is_compound_available<tc::decay_t<Lhs>, Rhs&&>, \
					"conversion_t provides a conversion, despite " #op "= being offered by the original type" \
				); \
				return _; \
			} \
		}

	#pragma push_macro("DEFINE_GENERIC_OP")
	#define DEFINE_GENERIC_OP(name, op) \
	namespace binary_operator_conversions { \
		template< typename Lhs, typename Rhs > \
		struct name##_conversion_type { \
			using type = Lhs; \
		}; \
		template< typename Lhs, typename Rhs, typename=void > \
		struct internal_##name##_conversion_type : name##_conversion_type<Lhs, Rhs> { }; \
	} \
	namespace generic_operator_helper { \
		/*Checks if (TThis&& op##= TOther&&) is defined*/ \
		TC_HAS_EXPR(compound_##name, (TThis)(TOther), std::declval<TThis>() op##= std::declval<TOther>()); \
		/*Checks if (TThis&&.operator op##=(TOther&&)) is defined*/ \
		TC_HAS_EXPR(mem_fn_compound_##name, (TThis)(TOther), std::declval<TThis>().operator op##=(std::declval<TOther>())); \
	} \
	namespace no_adl { \
		template< typename Base = void > \
		struct TC_EMPTY_BASES name : std::conditional_t<std::is_void<Base>::value, tc::empty_chain<name<void>>, Base> { \
		private: \
			template< typename Lhs, typename Rhs > \
			static constexpr bool is_compound_available = tc::generic_operator_helper::has_mem_fn_compound_ ##name<Lhs&, Rhs>; \
			template< typename Lhs, typename Rhs > \
			using conversion_t = typename binary_operator_conversions::internal_##name##_conversion_type<tc::decay_t<Lhs>, tc::decay_t<Rhs>>::type; \
			/*If we're not forwarding an rvalue, we're calling the operator on the result of tc::decay_copy(Lhs&), which is tc::decay_t<Lhs>& \
				If we're forwarding an rvalue, we're calling the operator on remove_reference_t<Lhs>&. But is_same<tc::decay_t<Lhs>, remove_reference_t<Lhs>>::value. */ \
			template< typename Lhs, typename Rhs > \
			using is_operation_available = tc::constant< \
				tc::derived_from<conversion_t<Lhs, Rhs>, name> \
				&& is_compound_available<conversion_t<Lhs, Rhs>, Rhs> \
			>; \
		public: \
			GENERIC_OP_BODY( op, _.operator op##=(tc_move_if_owned(rhs)) ); \
		}; \
		\
		template< typename Other, typename Base = void > \
		struct TC_EMPTY_BASES external_ ##name : std::conditional_t<std::is_void<Base>::value, tc::empty_chain<external_ ##name <void>>, Base> { \
		private: \
			template< typename Lhs, typename Rhs > \
			static constexpr bool is_compound_available = tc::generic_operator_helper::has_compound_ ##name<Lhs&, Rhs>; \
			template< typename Lhs, typename Rhs > \
			using conversion_t = tc::decay_t<Lhs>; \
			static_assert( std::is_fundamental<Other>::value, "external_" #name " is only meant for fundamental types" ); \
			STATICASSERTSAME( tc::decay_t<Other>, Other ); \
			template< typename Lhs, typename Rhs > \
			using is_operation_available = tc::constant< \
				tc::derived_from<std::remove_reference_t<Lhs>, Other> \
				&& tc::derived_from<std::remove_reference_t<Rhs>, external_ ##name<Other, Base>> \
				/* Same reasoning as in the internal operator */ \
				&& is_compound_available<tc::decay_t<Lhs>, Rhs> \
			>; \
		public: \
			GENERIC_OP_BODY( op, { \
				static_assert(!tc::generic_operator_helper::has_mem_fn_compound_ ##name<decltype((_)), decltype(tc_move_if_owned(rhs))>); \
				_ op##= tc_move_if_owned(rhs); \
			} ); \
		}; \
	} \
	using no_adl::name; \
	using no_adl::external_ ##name; \
	
	// By default, tc::addable &co. allow operations for which there exists a corresponding lhs.operator op=(rhs) member operator
	// For cases where the left-hand side requires a promoting conversion before the operation can take place,
	//  specialize one of the tc::binary_operator_conversions::*_conversion_type structs
	// Example: if class A wants A+B to be performed by converting A to C:
	//	1. both A and C need to derive from tc::addable (C needs it to satisfy SFINAE sanity checks, A needs it so that ADL can find the generic operator+)
	//		Note: A wouldn't need to derive from addable, if the generic operators were global, but that would increase the complexity of compiling any operator call
	//	2. C.operator+=(B) (with appropriate cvref qualifiers) must exist and be accessible
	//	3. there must be a specialization for tc::binary_operator_conversions::addable_conversion_type<A, B> that defines the alias type=C;

	DEFINE_GENERIC_OP(addable, +);
	DEFINE_GENERIC_OP(subtractable, -);
	DEFINE_GENERIC_OP(multipliable, *);
	DEFINE_GENERIC_OP(dividable, /);
	DEFINE_GENERIC_OP(orable, |);
	DEFINE_GENERIC_OP(andable, &);
	DEFINE_GENERIC_OP(xorable, ^);
	DEFINE_GENERIC_OP(left_shiftable, <<);
	#pragma pop_macro("DEFINE_GENERIC_OP")
	#pragma pop_macro("GENERIC_OP_BODY")

	template< typename Base = void >
	using additive = addable<subtractable<Base>>;

	template< typename Base = void >
	using multiplicative = multipliable<dividable<Base>>;

	template< typename Base = void >
	using arithmetic = additive<multiplicative<Base>>;

	template< typename Base = void >
	using setlike = andable<orable<xorable<subtractable<Base>>>>;

	template< typename Other, typename Base = void >
	using external_additive = external_addable<Other, external_subtractable<Other, Base>>;

	template< typename Other, typename Base = void >
	using external_multiplicative = external_multipliable<Other, external_dividable<Other, Base>>;

	template< typename Other, typename Base = void >
	using external_arithmetic = external_additive<Other, external_multiplicative<Other, Base>>;

	// If the same conversion logic applies to multiple operations, you can specialize one of the grouped conversions defined here
	namespace binary_operator_conversions {
		#pragma push_macro("PROXY_CONVERSION")
		#define PROXY_CONVERSION(nameFrom, nameTo) \
		template< typename Lhs, typename Rhs > \
		struct internal_##nameFrom##_conversion_type<Lhs, Rhs, tc::void_t<typename nameTo##_conversion_type<Lhs, Rhs>::type>> \
			: nameTo##_conversion_type<Lhs, Rhs> \
		{}

		template< typename Lhs, typename Rhs >
		struct additive_conversion_type { };
		PROXY_CONVERSION(addable, additive);
		PROXY_CONVERSION(subtractable, additive);
		
		
		template< typename Lhs, typename Rhs >
		struct multiplicative_conversion_type { };
		PROXY_CONVERSION(multipliable, multiplicative);
		PROXY_CONVERSION(dividable, multiplicative);

		template< typename Lhs, typename Rhs >
		struct arithmetic_conversion_type { };
		PROXY_CONVERSION(addable, arithmetic);
		PROXY_CONVERSION(subtractable, arithmetic);
		PROXY_CONVERSION(multipliable, arithmetic);
		PROXY_CONVERSION(dividable, arithmetic);

		template< typename Lhs, typename Rhs >
		struct setlike_conversion_type { };
		PROXY_CONVERSION(andable, setlike);
		PROXY_CONVERSION(orable, setlike);
		PROXY_CONVERSION(xorable, setlike);
		PROXY_CONVERSION(subtractable, setlike);
		#pragma pop_macro("PROXY_CONVERSION")
	}


	namespace scalar_binary_op_detail::no_adl {
		template<typename FnOp, typename Rhs>
		struct func {
			Rhs const& m_rhs;
			template<typename LhsElement>
			constexpr auto operator()(LhsElement&& lhselem) const& return_decltype_xvalue_by_ref_MAYTHROW( // should be NOEXCEPT, but NOEXCEPT does not allow xvalues.
				FnOp()(tc_move_if_owned(lhselem), m_rhs)
			)
		};

		struct no_prepost_scalar_operation {
			static constexpr void pre(tc::unused /*scalar*/) noexcept {}
			static constexpr void post(tc::unused /*lhs*/, tc::unused /*scalar*/) noexcept {}
		};

		template<typename Pred>
		struct assert_zero_to_scalar_relation final : no_prepost_scalar_operation {
			template<typename Scalar>
			static constexpr void pre(Scalar const& scalar) noexcept {
				_ASSERTE( Pred()(tc::explicit_cast<Scalar>(0), scalar) );
			}
		};
	}
	using scalar_binary_op_detail::no_adl::no_prepost_scalar_operation;
	using assert_non_zero_scalar = scalar_binary_op_detail::no_adl::assert_zero_to_scalar_relation<fn_not_equal_to>;
	using assert_non_negative_scalar = scalar_binary_op_detail::no_adl::assert_zero_to_scalar_relation<fn_less_equal>;
	using assert_positive_scalar = scalar_binary_op_detail::no_adl::assert_zero_to_scalar_relation<fn_less>;

#pragma push_macro("DEFINE_SCALAR_OP")
#define DEFINE_SCALAR_OP(name, op, fnop, fnassignop, prepostopdefault) \
	namespace no_adl { \
		template<typename Base = void, std::size_t nTransformDepth = 0, typename PrePostOperation = prepostopdefault> \
		struct TC_EMPTY_BASES scalar_ ## name : std::conditional_t<std::is_void<Base>::value, tc::empty_chain<scalar_ ## name<void, nTransformDepth, PrePostOperation>>, Base> { \
			template<tc::decayed_derived_from<scalar_ ## name> Lhs, typename Rhs> requires requires { \
				std::declval<Lhs>().template transform<nTransformDepth>(std::declval<scalar_binary_op_detail::no_adl::func<fnop, Rhs>>()); \
			} \
			[[nodiscard]] friend constexpr decltype(auto) operator op(Lhs&& lhs, Rhs const& rhs) noexcept { \
				PrePostOperation::pre(rhs); \
				decltype(auto) _ = tc_move_if_owned(lhs).template transform<nTransformDepth>(scalar_binary_op_detail::no_adl::func<fnop, Rhs>{rhs}); \
				PrePostOperation::post(_, rhs); \
				return _; \
			} \
			\
			template<tc::decayed_derived_from<scalar_ ## name> Lhs, typename Rhs> requires requires { \
				tc::for_each(std::declval<Lhs&>(), std::declval<scalar_binary_op_detail::no_adl::func<fnassignop, Rhs>>()); \
			} \
			friend constexpr Lhs& operator op ## =(Lhs& lhs, Rhs const& rhs) noexcept { \
				PrePostOperation::pre(rhs); \
				tc::for_each(lhs, scalar_binary_op_detail::no_adl::func<fnassignop, Rhs>{rhs}); \
				PrePostOperation::post(lhs, rhs); \
				return lhs; \
			} \
		}; \
	} \
	using no_adl::scalar_ ## name;

	DEFINE_SCALAR_OP(addable, +, tc::fn_plus, tc::fn_assign_plus, tc::no_prepost_scalar_operation)
	DEFINE_SCALAR_OP(subtractable, -, tc::fn_minus, tc::fn_assign_minus, tc::no_prepost_scalar_operation)
	DEFINE_SCALAR_OP(multipliable, *, tc::fn_mul, tc::fn_assign_mul, tc::no_prepost_scalar_operation)
	DEFINE_SCALAR_OP(dividable, /, tc::fn_div, tc::fn_assign_div, tc::assert_non_zero_scalar)
#pragma pop_macro("DEFINE_SCALAR_OP")

	namespace no_adl {
		template<typename Base = void, std::size_t nTransformDepth = 0>
		using scalar_multiplicative = tc::scalar_multipliable<tc::scalar_dividable<Base, nTransformDepth>, nTransformDepth>;
	}
	using no_adl::scalar_multiplicative;
}
