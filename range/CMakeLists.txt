project(RangeProposal)
cmake_minimum_required(VERSION 2.8.11)

include (CMakeParseArguments)

function(CreateStandalone CS_Dest)
	cmake_parse_arguments(CS "" "" "FILES" ${ARGN} )

	set(Dest "${CMAKE_BINARY_DIR}/${CS_Dest}")
	add_custom_target(CreateStandalone)
	add_custom_command(TARGET CreateStandalone PRE_BUILD
						COMMAND ${CMAKE_COMMAND} ARGS -E make_directory "${Dest}"
						COMMENT "Make directory ${Dest}"
					  )
	foreach(Src ${CS_FILES})
		add_custom_command(TARGET CreateStandalone PRE_BUILD
							COMMAND ${CMAKE_COMMAND} ARGS -E copy_if_different "${CMAKE_SOURCE_DIR}/${Src}" "${Dest}/${Src}"
							COMMENT "Copying '${Src}' to '${CS_DEST}/${Src}' ..."
						  )
	endforeach()
endfunction()

# Require boost system library
find_package(Boost 1.59 COMPONENTS system REQUIRED)
include_directories(${Boost_INCLUDE_DIR})
set(LIBRARIES ${LIBRARIES} ${Boost_LIBRARIES})

if (MSVC)
	add_definitions(-DBOOST_ALL_NO_LIB -D_SCL_SECURE_NO_WARNINGS)

	set(CMAKE_CXX_FLAGS_DEBUG          "/W4 /WX /MTd /Od /Ob0 /D_DEBUG /Zi /RTC1")
	set(CMAKE_CXX_FLAGS_MINSIZEREL     "/W4 /WX /MT  /O1 /Ob1 /DNDEBUG")
	set(CMAKE_CXX_FLAGS_RELEASE        "/W4 /WX /MT  /O2 /Ob2 /DNDEBUG")
	set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "/W4 /WX /MT  /O2 /Ob1 /DNDEBUG /Zi")
else ()
	add_definitions(-DCLANG)

	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++1z -stdlib=libc++ -Wall -pedantic -Wno-long-long -Wno-parentheses -Wno-unknown-pragmas -Wno-extra-semi -Wno-unused-function -Wno-unused-variable -Wno-unused-value -Wno-unused-private-field -fms-extensions -fdelayed-template-parsing")
	set(CMAKE_CXX_LINK_FLAGS "${CMAKE_CXX_LINK_FLAGS} -stdlib=libc++")
endif()

# Define Sources
set(Header
	accumulator.h
	algorithm.h
	array.h
	as_lvalue.h
	assign.h
	bool_cast.h
	break_or_continue.h
	casts.h
	compare.h
	container.h
	container_traits.h
	counting_range.h
	empty_chain.h
	empty.h
	equal.h
	equality_comparable.h
	explicit_cast.h
	filter_adaptor.h
	flatten_adaptor.h
	for_each.h
	for_each_adjacent_tuple.h
	functors.h
	has_mem_fn.h
	implements_compare.h
	index_iterator.h
	index_range.h
	inherit_ctors.h
	initializer_list.h
	inplace.h
	intersection_adaptor.h
	is_static_castable.h
	join_adaptor.h
	make_lazy.h
	merge_ranges.h
	meta.h
	minmax.h
	modified.h
	noncopyable.h
	partition_iterator.h
	partition_range.h
	quantifier.h
	range_adaptor.h
	range_defines.h
	range_fwd.h
	range.h
	reference_or_value.h
	renew.h
	result_of.h
	return_decltype.h
	reverse_adaptor.h
	round.h
	scope.h
	size.h
	sparse_adaptor.h
	static_polymorphism.h
	storage_for.h
	sub_range.h
	tc_move.h
	transform_adaptor.h
	transform.h
	trivial_functors.h
	types.h
	type_traits.h
	union_adaptor.h
	unique_range_adaptor.h
	zip_range.h
)
source_group(Header FILES ${Header})

set(Tests
	range.t.h
	algorithm.t.cpp
	compat.t.cpp
	const.t.cpp
	create.t.cpp
	empty.t.cpp
	equal.t.cpp
	for_each.t.cpp
	merge_ranges.t.cpp
	range.t.cpp
	reverse_adaptor.t.cpp
	sub_range.t.cpp
	transform.t.cpp
	type_traits_t.cpp
)
source_group(Tests FILES ${Tests})

set(Examples
	range.example.cpp
)
source_group(Examples FILES ${Examples})

set(Build
	CMakeLists.txt
)
source_group(Build FILES ${Build})

# create a test runner
add_definitions(-DPERFORMUNITTESTS -DBOOST_RANGE_ENABLE_CONCEPT_ASSERT=0)

foreach(Example ${Examples})
	add_executable(example_${Example} ${Build} ${Header} ${Tests} ${Example})
	target_link_libraries(example_${Example} ${LIBRARIES})
endforeach()

CreateStandalone("StandaloneRanges/Library/Utilities/Range" FILES ${Build} ${Header} ${Tests} ${Examples})
