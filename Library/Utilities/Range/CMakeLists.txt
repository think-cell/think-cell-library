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

add_definitions(-DCLANG)
	
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++1y -stdlib=libc++ -Wall -pedantic -Wno-long-long -Wno-parentheses -Wno-unknown-pragmas -Wno-extra-semi -Wno-unused-function -Wno-unused-variable -Wno-unused-value -Wno-unused-private-field -fms-extensions -fdelayed-template-parsing")
set(CMAKE_CXX_LINK_FLAGS "${CMAKE_CXX_LINK_FLAGS} -stdlib=libc++")


# Define Sources
set(Ranges
	range_defines.h
	range_fwd.h
	meta.h
	container_traits.h
	break_or_continue.h
	partition_iterator.h
	partition_range.h
	index_range.h
	index_iterator.h
	quantifier.h
	range_adaptor.h
	sparse_adaptor.h
	sub_range.h
	filter_adaptor.h
	transform.h
	transform_adaptor.h
	for_each.h
	equal.h
	empty.h 
	size.h
	algorithm.h
	intersection_adaptor.h
	join_adaptor.h
	types.h
	union_adaptor.h
	unique_range_adaptor.h
	../Range.h
   )
source_group(Ranges FILES ${Ranges})

set(TC_Utils
	../assign.h
	../casts.h
	../compare.h
	../in_place.h
	../make_lazy.h
	../modified.h
	../noncopyable.h
	../reference_or_value.h
	../remove_cvref.h
	../renew.h
	../return_decltype.h
	../trivial_functors.h
	../static_polymorphism.h
	../result_of.h
	../round.h
	../../ErrorReporting/tc_move.h
	../../ErrorReporting/storage_for.h
	../../ErrorReporting/functors.h
	../accumulator.h
	../conversion_traits.h
	../has_mem_fn.h
   )
source_group(Utilities FILES ${TC_Utils})

set(Tests
	  range.t.h
	  range.t.cpp
	  equal.t.cpp
	  for_each.t.cpp
	  create.t.cpp
	  empty.t.cpp
	  const.t.cpp
	  compat.t.cpp
	  algorithm.t.cpp
	  sub_range.t.cpp
	  transform.t.cpp
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
include_directories(".."           # include Utilities
					"../../.."     # include code (for error Reporting)
				   )
add_definitions(-DRANGE_PROPOSAL_BUILD_STANDALONE -DPERFORMUNITTESTS -DBOOST_RANGE_ENABLE_CONCEPT_ASSERT=0)

foreach(Example ${Examples})
	add_executable(example_${Example} ${Build} ${Ranges} ${TC_Utils} ${Tests} ${Example})
    target_link_libraries(example_${Example} ${LIBRARIES})
endforeach()

CreateStandalone("StandaloneRanges/Library/Utilities/Range" FILES ${Build} ${Ranges} ${TC_Utils} ${Tests} ${Examples})


