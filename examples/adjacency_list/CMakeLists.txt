cmake_minimum_required(VERSION 3.10.0)
project(example_adjacency_list VERSION 0.1.0 LANGUAGES CXX)

# Set the executable and the source files to compile it from.
add_executable(example_adjacency_list adjacency_list.cpp)

# Define the directory in which to search for includes
# used in the sources.
target_include_directories(example_adjacency_list PRIVATE
    $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

# Link the graph library
target_link_libraries(example_adjacency_list PRIVATE graph)