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

find_package(test-drive REQUIRED)

# Unit testing
add_subdirectory("unit")

# Process-level smoke test for the only terminating parser entry point.
add_executable(
  "${PROJECT_NAME}-cli-fixture"
  "fixtures/cli_fixture.f90"
)
target_link_libraries(
  "${PROJECT_NAME}-cli-fixture"
  PRIVATE
  "${PROJECT_NAME}-lib"
)
add_test(
  NAME "${PROJECT_NAME}-cli-policy"
  COMMAND
  "${CMAKE_COMMAND}"
  "-DFCLAP_CLI=$<TARGET_FILE:${PROJECT_NAME}-cli-fixture>"
  "-P"
  "${CMAKE_CURRENT_SOURCE_DIR}/fixtures/check_cli.cmake"
)
