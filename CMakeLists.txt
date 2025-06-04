cmake_minimum_required(VERSION 3.10.0)
project(graph VERSION 0.1.0 LANGUAGES CXX)

# Define the project as a header-only library.
add_library(graph INTERFACE)

# Define the directory in which to search for includes
# used in the library headers.
target_include_directories(graph INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

# Set the minimum C++ Standard against which to compile.
target_compile_features(graph INTERFACE cxx_std_23)

# Add subdirectory for examples
add_subdirectory(examples)

# Install the library in the include directory of the
# project linking it.
install(DIRECTORY ${PROJECT_SOURCE_DIR}/include/ DESTINATION include)