# This file is part of fclap.
# SPDX-Identifier: MIT
#
# Licensed under the MIT license; you may not use this file except
# in compliance with the License.
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

cmake_minimum_required(VERSION 3.15)
get_directory_property(is-subproject PARENT_DIRECTORY)

project(
  "fclap"
  LANGUAGES "Fortran"
  VERSION "0.1.0"
  DESCRIPTION "A Fortran Command Line Argument Parser library"
)

# Follow GNU conventions for installing directories
include(GNUInstallDirs)
include(CTest) # option(BUILD_TESTING). ON by default.

# General configuration information
add_subdirectory("config")

# Collect source of the project
set(srcs)
add_subdirectory("src")

# Fortran Command Line Argument Parser library target
add_library(
  "${PROJECT_NAME}-lib"
  "${srcs}"
)
set_target_properties(
  "${PROJECT_NAME}-lib"
  PROPERTIES
  POSITION_INDEPENDENT_CODE TRUE
  OUTPUT_NAME "${PROJECT_NAME}"
  VERSION "${PROJECT_VERSION}"
  SOVERSION "${PROJECT_VERSION_MAJOR}"
  Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include"
)
target_include_directories(
  "${PROJECT_NAME}-lib"
  PUBLIC
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${module-dir}>
)
if(NOT EXISTS "${PROJECT_BINARY_DIR}/include")
  file(MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
endif()

# Export targets for other projects
add_library("${PROJECT_NAME}" INTERFACE)
target_link_libraries("${PROJECT_NAME}" INTERFACE "${PROJECT_NAME}-lib")
install(
  TARGETS
  "${PROJECT_NAME}"
  "${PROJECT_NAME}-lib"
  EXPORT
  "${PROJECT_NAME}-targets"
  LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
)
install(
  EXPORT
  "${PROJECT_NAME}-targets"
  NAMESPACE
  "${PROJECT_NAME}::"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
)
install(
  DIRECTORY
  "${PROJECT_BINARY_DIR}/include/"
  DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/${module-dir}"
)
# Package license files
install(
  FILES
  "LICENSE.md"
  DESTINATION "${CMAKE_INSTALL_DATADIR}/licenses/${PROJECT_NAME}"
)

# add the testsuite
if(BUILD_TESTING)
  enable_testing()
  set(fpm-toml "${PROJECT_SOURCE_DIR}/fpm.toml")
  add_subdirectory("test")
endif()