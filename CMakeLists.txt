cmake_minimum_required(VERSION 3.21)

project(
  tc_library_test
  VERSION 1.0
  LANGUAGES C CXX
)

set(CMAKE_CXX_STANDARD 20)

# cmake -S . -B build -DBoost_INCLUDE_DIR=C:\Libraries\boost_1_79_0/ -DBoost_LIBRARY_DIR=C:\Libraries\boost_1_79_0/
find_package(Boost 1.75 REQUIRED)

# Generate one cpp file per header
file(
  GLOB_RECURSE liststrLibrary
  LIST_DIRECTORIES false 
  CONFIGURE_DEPENDS
  RELATIVE ${CMAKE_SOURCE_DIR}/tc
  ${CMAKE_SOURCE_DIR}/tc/*.h
)

set(liststrLibraryCpp "")

foreach(strLibrary ${liststrLibrary})
  set(strLibraryCpp ${CMAKE_BINARY_DIR}/test/${strLibrary}.cpp)
  list(APPEND liststrLibraryCpp ${strLibraryCpp})

  file(WRITE ${strLibraryCpp} 
    "#include " \"${strLibrary}\"
  )
endforeach()

file(WRITE ${CMAKE_BINARY_DIR}/test/main.cpp
  "int main() { return 0; }"
)

# Test if each header compiles individually
add_executable(compile_test ${liststrLibraryCpp} ${CMAKE_BINARY_DIR}/test/main.cpp)
target_include_directories(compile_test PRIVATE ${CMAKE_SOURCE_DIR}/tc)
target_link_libraries(compile_test Boost::boost Boost::disable_autolinking)

# Run unit tests
include(CTest)
list(APPEND CMAKE_CTEST_ARGUMENTS "--verbose")

file(
  GLOB_RECURSE liststrUnitTestFiles
  LIST_DIRECTORIES false 
  CONFIGURE_DEPENDS
  ${CMAKE_SOURCE_DIR}/tc/*.t.cpp
)

add_executable(unit_test ${liststrUnitTestFiles} ${CMAKE_BINARY_DIR}/test/main.cpp)
target_include_directories(unit_test PRIVATE ${CMAKE_SOURCE_DIR}/tc)
target_link_libraries(unit_test Boost::boost Boost::disable_autolinking)
add_test(NAME unit_test COMMAND unit_test)

add_executable(example_test range.example.cpp)
target_link_libraries(example_test Boost::boost Boost::disable_autolinking)
add_test(NAME example_test COMMAND example_test)
