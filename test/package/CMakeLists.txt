cmake_minimum_required(VERSION 3.15)
project(fclap-package-consumer LANGUAGES Fortran)

find_package(fclap 0.1 CONFIG REQUIRED)

add_executable(fclap-package-consumer main.f90)
target_link_libraries(fclap-package-consumer PRIVATE fclap::fclap)

enable_testing()
add_test(NAME fclap-package-consumer COMMAND fclap-package-consumer)
