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

test_srcs = files(
  'main.f90',
  'test_nargs.f90',
  'test_errors.f90',
  'test_values.f90',
  'test_namespace.f90',
  'test_registration.f90',
  'test_actions.f90',
  'test_parse_engine.f90',
  'test_parse_nargs.f90',
  'test_parse_actions.f90',
  'test_validators.f90',
  'test_formatter.f90',
  'test_groups.f90',
  'test_subparsers.f90',
)

tester = executable(
  'fclap-tester',
  sources: test_srcs,
  dependencies: [fclap_dep, testdrive_dep],
)

test(
  'fclap-unit-tests',
  tester,
)
