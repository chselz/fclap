cmake_minimum_required(VERSION 3.15)
project(fclap-subproject-consumer LANGUAGES Fortran)

# Consume fclap from source, as add_subdirectory or FetchContent users do.
# The build-tree target spelling must match the installed fclap::fclap.
add_subdirectory(
  "${CMAKE_CURRENT_SOURCE_DIR}/../.."
  "${CMAKE_CURRENT_BINARY_DIR}/fclap"
)
if(TARGET fclap-tester)
  message(FATAL_ERROR "fclap tests must not be built for a subproject")
endif()

add_executable(fclap-subproject-consumer ../package/main.f90)
target_link_libraries(fclap-subproject-consumer PRIVATE fclap::fclap)

enable_testing()
add_test(NAME fclap-subproject-consumer COMMAND fclap-subproject-consumer)
