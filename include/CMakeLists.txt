# Define the project as a header-only library.
add_library(${PROJECT_NAME} INTERFACE)

include(GNUInstallDirs)

# Define the directory in which to search for includes
# used in the library headers.
target_include_directories(${PROJECT_NAME} INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

# Set the minimum C++ Standard against which to compile.
target_compile_features(${PROJECT_NAME} INTERFACE cxx_std_23)

# Export the target to a config file (.cmake) for use by other projects
# with find_package().
install(TARGETS ${PROJECT_NAME} EXPORT ${PROJECT_NAME}-config)

# Install the export
install(
    EXPORT ${PROJECT_NAME}-config   # Name of .cmake file
    NAMESPACE ${PROJECT_NAME}::     # Namespace for the target
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}
)

# Install the library in the include directory of the
# project linking it (exclude this CMakeLists.txt file).
install(DIRECTORY ${CMAKE_CURRENT_LIST_DIR}/${PROJECT_NAME} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME})