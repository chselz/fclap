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
  "${dir}/nargs.f90"
  "${dir}/error/codes.f90"
  "${dir}/error/entry.f90"
  "${dir}/error/stack.f90"
  "${dir}/value/abstract.f90"
  "${dir}/value/builtin.f90"
  "${dir}/value/convert.f90"
  "${dir}/namespace.f90"
  "${dir}/parse/context.f90"
  "${dir}/parse/result.f90"
  "${dir}/actions/abstract.f90"
  "${dir}/actions/store.f90"
  "${dir}/actions/boolean.f90"
  "${dir}/actions/accumulate.f90"
  "${dir}/actions/control.f90"
  "${dir}/actions/builtin.f90"
  "${dir}/validators/abstract.f90"
  "${dir}/validators/choices.f90"
  "${dir}/validators/bounds.f90"
  "${dir}/argument.f90"
  "${dir}/groups/abstract.f90"
  "${dir}/groups/helppage.f90"
  "${dir}/groups/mutex.f90"
  "${dir}/formatter/model.f90"
  "${dir}/formatter/abstract.f90"
  "${dir}/formatter/standard.f90"
  "${dir}/parse/engine.f90"
  "${dir}/argparser.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)
