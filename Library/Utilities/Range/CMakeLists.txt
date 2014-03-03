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


 ## Adapt those to your environment
if (MSVC)
	include_directories(C:/Libraries/boost_1_55_0/)
	add_definitions(-DBOOST_ALL_NO_LIB -D_SCL_SECURE_NO_WARNINGS)
	
	set(CMAKE_CXX_FLAGS_DEBUG "/D_DEBUG /MTd /Zi /Ob0 /Od /RTC1")
    set(CMAKE_CXX_FLAGS_MINSIZEREL     "/MT /O1 /Ob1 /D NDEBUG")
    set(CMAKE_CXX_FLAGS_RELEASE        "/MT /O2 /Ob2 /D NDEBUG")
    set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "/MT /Zi /O2 /Ob1 /D NDEBUG")
 else ()
	include_directories(/usr/local/include/boost_1_55_0/)
	add_definitions(-DCLANG)

	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -stdlib=libc++ -Wall -pedantic -Wno-long-long -Wno-parentheses -Wno-unknown-pragmas")
	set(CMAKE_CXX_LINK_FLAGS "${CMAKE_CXX_LINK_FLAGS} -stdlib=libc++")
endif()

# Define Sources
set(Ranges 
      range_defines.h
      range_fwd.h
      meta.h
      container_traits.h
      break_or_continue.h
      continue_if_void.h
      partition_iterator.h
      partition_range.h
      index_range.h
      range_adaptor.h
      sub_range.h
      filter_range.h
      transform.h
      transform_range.h
      for_each.h
      equal.h
      is_ordered.h
      size.h
      quantifier.h
      algorithm.h
      ../Range.h
   )
source_group(Ranges FILES ${Ranges})
   
set(TC_Utils
	  ../casts.h
      ../reference_or_value.h
	  ../remove_cvref.h
	  ../perfect_forward.h
	  ../renew.h
	  ../convert.h
      ../decltype_return.h
	  ../result_of.h
	  ../../ErrorReporting/tc_move.h
	  ../../ErrorReporting/make_lvalue.h
	  ../../ErrorReporting/UnitTest.h
	  ../../ErrorReporting/storage_for.h
	  ../../ErrorReporting/functors.h
	  ../inherit_ctors.h
	  ../static_vector.h
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
	  const.t.cpp
	  compat.t.cpp
	  algorithm.t.cpp
	  sub_range.t.cpp
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
add_definitions(-DRANGE_PROPOSAL_BUILD_STANDALONE -DPERFORMUNITTESTS)

foreach(Example ${Examples})
	add_executable(example_${Example} ${Build} ${Ranges} ${TC_Utils} ${Tests} ${Example})
endforeach()

CreateStandalone("StandaloneRanges/Library/Utilities/Range" FILES ${Build} ${Ranges} ${TC_Utils} ${Tests} ${Examples})


