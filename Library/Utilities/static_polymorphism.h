#pragma once

#define STATIC_VIRTUAL_METHOD_NAME( Name ) \
	Name ## _ImplDoNotCallDirectly

#define STATIC_VIRTUAL( Name ) \
	using Name ## _derived_type = Derived; \
	template<typename... Args> \
	decltype(auto) Name(Args&& ...args) { \
		return tc::derived_cast<Derived>(*this).STATIC_VIRTUAL_METHOD_NAME( Name )(std::forward<Args>(args)...); \
	} \
	template<typename... Args> \
	decltype(auto) Name(Args&& ...args) const { \
		return tc::derived_cast<Derived>(*this).STATIC_VIRTUAL_METHOD_NAME( Name )(std::forward<Args>(args)...); \
	}

#define STATIC_FINAL_MOD(Mod, Name) \
	static_assert( \
		std::is_same< \
			typename this_type::Name ## _derived_type, \
			this_type \
		>::value, \
		"Static polymorphism error" \
	); \
	Mod \
	auto STATIC_VIRTUAL_METHOD_NAME( Name )

#define STATIC_FINAL(Name) \
	STATIC_FINAL_MOD(BOOST_PP_EMPTY(), Name)

#define STATIC_OVERRIDE_MOD(Mod, Name) \
	static_assert( \
		std::is_same<typename this_type::Name ## _derived_type, Derived>::value, \
		"Static polymorphism error" \
	); \
	Mod \
	auto STATIC_VIRTUAL_METHOD_NAME( Name )

#define STATIC_OVERRIDE( Name ) \
	STATIC_OVERRIDE_MOD( BOOST_PP_EMPTY(), Name )
