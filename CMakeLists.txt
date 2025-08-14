cmake_minimum_required(VERSION 3.24.0)
project(graph VERSION 0.1.0 LANGUAGES CXX)

# Add subdirectory for the library
add_subdirectory(include)

# Add subdirectory for examples
add_subdirectory(examples)

# Add tests
include(CTest)
enable_testing()
add_subdirectory(tests)