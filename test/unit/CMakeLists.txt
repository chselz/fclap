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

set(
  test-srcs
  "main.f90"
)

add_executable(
  ${PROJECT_NAME}-tester
  "${test-srcs}"
)
target_link_libraries(
  ${PROJECT_NAME}-tester
  PRIVATE
  "${PROJECT_NAME}-lib"
)
add_test(
  "${PROJECT_NAME}-test"
  ${PROJECT_NAME}-tester
)
