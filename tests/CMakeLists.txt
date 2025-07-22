cmake_minimum_required(VERSION 3.24.0)

include(FetchContent)

# GoogleTest
FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG        52eb8108c5bdec04579160ae17225d66034bd723 # v1.17.0
  FIND_PACKAGE_ARGS NAMES GTest                           # Try `find_package` first
)
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE) # For Windows: Prevent overriding the parent project's compiler/linker settings
FetchContent_MakeAvailable(googletest)

add_executable(gtest_test gtest_test.cpp adjacency_list/directed_multigraph.cpp)

target_link_options(gtest_test PRIVATE --coverage)
target_compile_options(gtest_test PRIVATE --coverage)

target_link_libraries(gtest_test PRIVATE GTest::gtest_main graph gcov)
target_compile_features(gtest_test PRIVATE cxx_std_17)

include(GoogleTest)
gtest_discover_tests(gtest_test)