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

project(
  'fclap',
  'fortran',
  version: '0.1.0',
  license: 'MIT',
  meson_version: '>=0.55',
  default_options: [
    'buildtype=debugoptimized',
    'default_library=both',
  ],
)
install = not (meson.is_subproject() and get_option('default_library') == 'static')

# General configuration information
subdir('config')

# Collect source of the project
srcs = []
subdir('src')

# Fortran Command Line Argument Parser library target
fclap_lib = library(
  meson.project_name(),
  sources: srcs,
  version: meson.project_version(),
  install: install,
)

# Export dependency for other projects and test suite
fclap_inc = fclap_lib.private_dir_include()
fclap_dep = declare_dependency(
  link_with: fclap_lib,
  include_directories: fclap_inc,
)

# Package the license files
fclap_lic = files(
  'LICENSE.md',
)

if install
  # Distribute the license files in share/licenses/<name>
  install_data(
    fclap_lic,
    install_dir: get_option('datadir')/'licenses'/meson.project_name()
  )

  module_id = meson.project_name() / 'modules'
  meson.add_install_script(
    find_program(files('config'/'install-mod.py')),
    get_option('includedir') / module_id,
  )

  pkg = import('pkgconfig')
  pkg.generate(
    fclap_lib,
    description: 'A Fortran Command Line Argument Parser library',
    subdirs: ['', module_id],
  )
endif

# add the testsuite
if get_option('tests')
  fpm_toml = meson.current_source_dir()/'fpm.toml'
  subdir('test')
endif