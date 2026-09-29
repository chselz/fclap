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

testdrive_dep = dependency(
  'test-drive',
  fallback: ['test-drive', 'testdrive_dep'],
)

# Unit testing
subdir('unit')

# Process-level smoke tests. Structured parser behavior remains in Test Drive.
cli_fixture = executable(
  'fclap-cli-fixture',
  sources: files('fixtures'/'cli_fixture.f90'),
  dependencies: fclap_dep,
)

test('fclap-cli-success', cli_fixture)
test('fclap-cli-help', cli_fixture, args: ['--help'])
test('fclap-cli-version', cli_fixture, args: ['--version'])
test('fclap-cli-warning', cli_fixture, args: ['--legacy', 'value'])
test(
  'fclap-cli-failure',
  cli_fixture,
  args: ['--unknown'],
  should_fail: true,
)
