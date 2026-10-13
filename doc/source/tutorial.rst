Tutorial
========

fclap parser definitions are reusable, while each parse invocation owns fresh
state. This makes the same parser safe to use for a normal command-line
program or repeatedly with explicit token arrays.

Installation
------------

With fpm, add fclap as a dependency of your application:

.. code-block:: toml

   [dependencies]
   fclap = { git = "https://github.com/chselz/fclap.git" }

An installed CMake package provides the ``fclap::fclap`` target:

.. code-block:: cmake

   find_package(fclap 0.1 CONFIG REQUIRED)
   target_link_libraries(my_program PRIVATE fclap::fclap)

CMake projects can also build fclap from source with ``FetchContent`` or
``add_subdirectory``. The target has the same ``fclap::fclap`` name, and
fclap's own test suite is skipped, so test-drive is not required:

.. code-block:: cmake

   include(FetchContent)
   FetchContent_Declare(
     fclap
     GIT_REPOSITORY https://github.com/chselz/fclap.git
     GIT_TAG main
   )
   FetchContent_MakeAvailable(fclap)
   target_link_libraries(my_program PRIVATE fclap::fclap)

Set ``FCLAP_BUILD_TESTING=ON`` to build fclap's tests in that configuration.

Meson installations provide ``fclap.pc``:

.. code-block:: meson

   fclap_dep = dependency('fclap', version: '>=0.1.0')
   executable('my_program', 'main.f90', dependencies: fclap_dep)

To build fclap as a Meson subproject instead, add
``subprojects/fclap.wrap``:

.. code-block:: ini

   [wrap-git]
   url = https://github.com/chselz/fclap.git
   revision = main

   [provide]
   fclap = fclap_dep

The same ``dependency('fclap')`` call then falls back to the subproject when
no installed fclap is found. fclap's tests are not built as a subproject.

Applications should import public names only through ``use fclap``. The
additional module files installed beside ``fclap.mod`` are compiler-required
implementation dependencies, not separate supported interfaces.

Build a basic command line
--------------------------

The basic example is compiled and smoke-tested by fpm:

.. literalinclude:: ../../example/basic.f90
   :language: fortran
   :linenos:

``parse_args`` reads the process command line. It returns a ``Namespace`` on
success, prints and exits successfully for help or version, and prints
structured diagnostics before exiting with status 2 on failure. This is the
closest interface to Python's ``argparse``. A mistake in the parser definition
itself, such as an option registered twice, is not the user's fault:
``parse_args`` then prints ``prog: invalid parser definition: ...`` naming the
argument and exits with status 70.

The example accepts the usual command-line spellings:

.. code-block:: console

   $ fclap-basic input.dat --threads=4 -v
   $ fclap-basic input.dat -j4 -v
   $ fclap-basic - -vj 4          # '-' is an ordinary value (stdin)
   $ fclap-basic input.dat -j 0
   usage: fclap-basic [-h] [-v] [-j THREADS] INPUT
   fclap-basic: error: argument -j/--threads: value must not be less than 1 (got '0')

The example reads its values without ``stat=``. That is safe for values that
always exist (positionals, flags, and options with defaults): if a key is
mistyped or the variable has the wrong type, retrieval stops with a message
naming the key instead of silently leaving the variable unset. Pass ``stat``
for optional values that may be absent. Strings are read into
``character(len=:), allocatable`` variables, which receive the exact value, so
long paths are never cut off. Import names with an ``only`` list; ``use
fclap`` without one shadows the ``count`` intrinsic.

Report checks your program makes after parsing with ``parser%error``, which
prints usage and ``prog: error: <message>`` and exits with status 2:

.. code-block:: fortran

   inquire(file=input, exist=found)
   if (.not. found) call parser%error("input file '" // input // "' not found")

Parser construction
-------------------

Initialize every parser before adding arguments:

.. code-block:: fortran

   call parser%init( &
       prog="solver", &
       usage="solver [OPTIONS] INPUT", &
       description="Run a calculation", &
       epilog="See the manual for input syntax.", &
       version="solver 1.0")

All keywords are optional. ``prog`` defaults to the executable name,
``usage`` defaults to generated usage, and ``add_help=.false.`` disables the
automatic ``-h``/``--help`` option. Supplying ``version`` adds ``--version``.
Calling ``init`` again clears the prior definition.

Arguments
---------

A name without a leading hyphen is positional. Names beginning with a hyphen
are aliases for one optional argument:

.. code-block:: fortran

   call parser%add_argument("input", help="input file")
   call parser%add_argument("-o", "--output", metavar="FILE", &
       default="result.dat", help="output file")

The destination defaults to the longest long option (or the longest alias when
there is no long option) with leading hyphens removed and internal hyphens
changed to underscores. Override it with ``dest=``. Names and destinations
must be unique.

Value types
~~~~~~~~~~~

The canonical ``data_type`` names are ``string``, ``integer``, ``real``, and
``logical``. The aliases ``character``, ``char``, ``int``, ``float``,
``double``, ``bool``, and ``boolean`` are also accepted. Values and defaults are converted to the declared type:

.. code-block:: fortran

   call parser%add_argument("--iterations", data_type="integer", default=20)
   call parser%add_argument("--tolerance", data_type="real", default=1.0e-8_wp)
   call parser%add_argument("--enabled", data_type="logical", default=.true.)

Integers need no kind parameter: plain literals such as ``default=20``,
``choices=[0, 1, 2]``, or ``not_less_than(1)`` and plain ``integer`` variables
work everywhere, and ``integer(int64)`` is accepted where values exceed 32
bits. Reals are stored in double precision. Write real literals with your
project's double-precision kind, as above with ``_wp`` from your own kind
module (or ``real64`` from ``iso_fortran_env``): ``1.0e-8`` alone is a
single-precision value whose rounding error is kept. ``real(real32)``
variables can also receive values.
Namespace retrieval reports ``FCLAP_OK`` or an ``ERR_NAMESPACE_*`` code
through its optional status argument.

Choices and validators
~~~~~~~~~~~~~~~~~~~~~~

Choices are converted before parsing. Bound validators operate on converted
integer or real values, and all bounds are inclusive:

.. code-block:: fortran

   call parser%add_argument("--mode", &
       choices=[character(len=4) :: "fast", "safe"], default="safe")
   call parser%add_argument("--threads", data_type="integer", &
       validator=not_less_than(1))
   call parser%add_argument("--ratio", data_type="real", &
       validator=not_bigger_than(1.0_wp))
   call parser%add_argument("--level", data_type="integer", &
       validator=in_range(0, 3))

The bound must have the argument's type: ``not_less_than(0)`` on a ``real``
argument is a configuration error; write ``not_less_than(0.0_wp)``.

Choices are compared exactly by default. Pass
``case_insensitive_choices=.true.`` to accept any letter case for string
choices; the value is stored with the spelling you declared, so program code
only has to compare one form:

.. code-block:: fortran

   call parser%add_argument("--closure", &
       choices=[character(len=3) :: "HNC", "KH", "PY"], default="HNC", &
       case_insensitive_choices=.true.)
   ! --closure hnc, --closure Hnc, and --closure HNC all store "HNC"

Invalid defaults, choices, or validator combinations are configuration errors
and the invalid argument is not registered.

Actions
~~~~~~~

Actions are concrete values returned by public factory functions:

.. code-block:: fortran

   call parser%add_argument("--verbose", action=store_true())
   call parser%add_argument("--color", action=store_false())
   call parser%add_argument("--cache", action=boolean_optional(), &
       default=.true.)
   call parser%add_argument("--level", action=count())
   call parser%add_argument("--tag", action=append())
   call parser%add_argument("--mode", action=store_const("fast"))

``store`` is the default. ``store_true`` defaults to false, ``store_false``
defaults to true, ``count`` defaults to zero, and ``append`` accumulates a
typed list; explicit ``--tag`` occurrences replace a ``default=`` list.
``boolean_optional`` registers both ``--cache`` and ``--no-cache``; the last
one given wins. Repeated ``store`` occurrences use the last value.

Number of values
~~~~~~~~~~~~~~~~

``nargs`` accepts a positive integer or one of these character forms:

.. list-table::
   :header-rows: 1
   :widths: 15 85

   * - Value
     - Meaning
   * - ``"?"``
     - Zero or one value. An option requires ``const=`` for the zero-value case.
   * - ``"*"``
     - Zero or more values, returned as a list.
   * - ``"+"``
     - One or more values, returned as a list.
   * - ``"remainder"``
     - All remaining tokens for a final positional argument.
   * - positive integer
     - Exactly that many values; counts greater than one return a list.

For example:

.. code-block:: fortran

   call parser%add_argument("--pair", nargs=2, data_type="real")
   call parser%add_argument("--maybe", nargs="?", const="automatic")
   call parser%add_argument("files", nargs="+")

The marker ``--`` ends option recognition. Signed numeric tokens are accepted
as values when a numeric argument is being consumed.

Help metadata and lifecycle
~~~~~~~~~~~~~~~~~~~~~~~~~~~

``visible=.false.`` hides an argument from usage and help without disabling
parsing. ``print_default`` and ``print_choices`` control annotations in the
standard formatter. Lifecycle messages are explicit:

.. code-block:: fortran

   call parser%add_argument("--legacy", &
       deprecated_msg="use --mode instead")
   call parser%add_argument("--removed", &
       removed_msg="this option was removed in version 2")

Using a deprecated argument succeeds with a warning. Using a removed argument
returns a fatal structured error and does not execute its action.

Non-terminating parsing
-----------------------

Libraries, tests, and interactive applications should use ``parse_tokens`` or
``try_parse_args``. Neither prints nor terminates:

.. literalinclude:: ../../example/library_mode.f90
   :language: fortran
   :linenos:

``ParseResult%outcome`` is one of ``PARSE_SUCCESS``, ``PARSE_FAILURE``,
``PARSE_HELP``, or ``PARSE_VERSION``. The result also owns its ``namespace``,
``errors``, optional help/version ``text``, and optional
``selected_command_path``.

Argument groups
---------------

Group creation returns a stable ``GroupHandle``. Pass that handle when adding
members:

.. code-block:: fortran

   type(GroupHandle) :: output_group, mode_group

   output_group = parser%add_argument_group( &
       "output options", description="Control generated files")
   call parser%add_argument("-o", "--output", group=output_group)

   mode_group = parser%add_mutually_exclusive_group( &
       required=.true., title="mode")
   call parser%add_argument("--fast", action=store_true(), group=mode_group)
   call parser%add_argument("--safe", action=store_true(), group=mode_group)

A handle belongs to exactly one initialized parser. Stale, foreign, or invalid
handles produce configuration errors.

Members of a mutually exclusive group must be omissible. A ``required=.true.``
option or a positional that needs a value would conflict with every other
member, so it is rejected with ``ERR_INVALID_GROUP``; make the group itself
required instead.

Parent parsers
--------------

``init_with_parents`` deep-copies reusable definitions in parent order:

.. code-block:: fortran

   type(ArgumentParser) :: common, application

   call common%init(add_help=.false.)
   call common%add_argument("--config", default="app.toml")
   call application%init_with_parents([common], prog="application")
   call application%add_argument("input")

Actions, validators, defaults, and group membership are copied. Automatic
help/version actions, parent subcommand trees, and parent formatters are not
inherited. Conflicting names or destinations are fatal configuration errors.

Subcommands
-----------

Child parsers are registered as deep snapshots, so later changes to the source
child do not affect the command tree. Add every argument to a child *before*
calling ``add_parser``; anything added afterwards is silently not part of the
parent:

.. literalinclude:: ../../example/subcommands.f90
   :language: fortran
   :linenos:

Only one subparser collection may be added to a parser. Its default
destination is ``command``; ``required=.true.`` rejects a missing command.
Child namespace values are deep-merged into the returned namespace and win on
duplicate keys. ``ParseResult%selected_command_path`` records nested command
names from root to leaf.

Errors and configuration checks
-------------------------------

Registration errors accumulate on the parser. Inspect them before parsing if
definitions are assembled dynamically:

.. code-block:: fortran

   type(ErrorStack) :: errors

   if (.not. parser%is_valid()) then
       errors = parser%get_config_errors()
       call errors%print_all()
   end if

Parsing an invalid parser returns ``PARSE_FAILURE`` without consuming input.
For runtime errors, each ``ErrorEntry`` includes a stable code and severity,
plus optional argument, token, and one-based token index. Expected-failure
tests should assert these fields through ``parse_tokens`` rather than invoking
the terminating ``parse_args`` facade.
