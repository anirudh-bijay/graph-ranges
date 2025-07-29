# Define the project as a header-only library.
add_library(graph INTERFACE)

# Define the directory in which to search for includes
# used in the library headers.
target_include_directories(graph INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<INSTALL_INTERFACE:include>
)

# Set the minimum C++ Standard against which to compile.
target_compile_features(graph INTERFACE cxx_std_23)