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

add_subdirectory("utils")

set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/fclap_actions.f90"
  "${dir}/fclap_constants.f90"
  "${dir}/fclap_errors.f90"
  "${dir}/fclap_formatter.f90"
  "${dir}/fclap_namespace.f90"
  "${dir}/fclap_parser.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)
