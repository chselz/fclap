Public API reference
====================

Supported module
----------------

Applications import the ``fclap`` facade. Its exports are grouped into the
*application interface* (parser, namespace, results, action and validator
factories, error codes) and the *extension interface* (abstract
types and the value, nargs, and help-model types they exchange). Most programs
need only the first group:

.. code-block:: fortran

   use fclap, only : ArgumentParser, Namespace, ParseResult

The CMake target is ``fclap::fclap`` both for an installed package and for
``add_subdirectory``/``FetchContent`` builds, and the pkg-config package is
``fclap``. Internal modules prefixed with ``fclap_`` are implementation
dependencies and may change without compatibility guarantees.

Always import with an ``only`` list. The facade exports short names such as
``count``, ``store``, and ``append``; a bare ``use fclap`` shadows the
``count`` intrinsic. Rename on import when needed:

.. code-block:: fortran

   use fclap, only : ArgumentParser, count_action => count

ArgumentParser
--------------

``ArgumentParser`` owns an immutable-by-parsing command-line definition. Its
data components are private.

Construction
~~~~~~~~~~~~

.. code-block:: fortran

   call parser%init(prog, usage, description, epilog, version, &
                    formatter, add_help, help_on_empty)

All arguments are optional. ``formatter`` is ``class(FormatterType)`` and is
cloned. ``add_help`` defaults to true. With ``help_on_empty=.true.`` an empty
command line at this parser's level is answered with its help instead of
being parsed: ``parse_tokens`` returns ``PARSE_FAILURE`` with one
``ERR_NO_ARGUMENTS`` entry and the help in ``text``, and ``parse_args`` prints
only that help to ``error_unit`` and stops with status 2. On a subcommand it
applies to ``prog cmd`` without further tokens.

.. code-block:: fortran

   call parser%init_with_parents(parents, prog, usage, description, epilog, &
                                version, formatter, add_help, help_on_empty)

``parents`` is ``type(ArgumentParser) :: parents(:)``. Definitions are copied
deeply in array order after the receiving parser is initialized.

Argument registration
~~~~~~~~~~~~~~~~~~~~~

.. code-block:: fortran

   call parser%add_argument(name1, name2, name3, name4, action, nargs, &
       data_type, default, const, choices, validator, required, help, &
       metavar, dest, visible, deprecated_msg, removed_msg, print_default, &
       print_choices, group, case_insensitive_choices, env, global, &
       delimiter, choice_help)

``name1`` is required; the other names are optional aliases. Important dummy
argument types are:

.. list-table::
   :header-rows: 1
   :widths: 28 72

   * - Keyword
     - Type
   * - ``action``
     - ``class(ActionType)``
   * - ``nargs``
     - integer or character scalar
   * - ``data_type``
     - character scalar
   * - ``default``
     - supported intrinsic scalar or rank-one array
   * - ``const``
     - supported intrinsic scalar
   * - ``choices``
     - supported intrinsic rank-one array
   * - ``validator``
     - ``class(ValidatorType)``
   * - ``required``, ``visible``, ``print_default``, ``print_choices``,
       ``case_insensitive_choices``
     - logical scalar
   * - ``group``
     - ``type(GroupHandle)``
   * - ``env``, ``delimiter``
     - character scalar
   * - ``global``
     - logical scalar
   * - ``choice_help``
     - character rank-one array

``case_insensitive_choices=.true.`` matches string choices without regard to
ASCII letter case and stores the declared spelling: with
``choices=["HNC", "KH", "PY"]``, the token ``hnc`` is stored as ``HNC``.
Defaults and ``const`` values are matched and normalized the same way. It is
valid only for string arguments with choices, and choices that differ only by
case are rejected as ambiguous.

``env="NAME"`` makes an option fall back to the environment variable
``NAME`` when it is not on the command line; the command line wins over the
environment, which wins over ``default``. An empty variable counts as unset.
The value is converted and checked like a command-line value, and an invalid
one is reported as ``argument --opt: environment variable NAME: ...``. A flag
reads the variable as a logical: true acts like the flag, false leaves it
unset, except that a ``boolean_optional`` flag stores false. An option taking
several values splits the variable at blanks. A value from the environment
counts as given for ``required``, groups, and rules. Help shows
``(env: NAME)``. Only options accept ``env``; positionals, removed options,
empty names, and names containing ``=`` or blanks are rejected with
``ERR_INVALID_ENV``.

``global=.true.`` also accepts an option after a subcommand (``prog run -v``).
Each subcommand, including ones registered later and nested ones, receives a
copy of the option, so its help lists it too. A value given before the
command is kept unless the option is given again after it. Only optional,
non-required options outside mutually exclusive groups and rules can be
global, because those checks see only their own parser's part of the command
line; a subcommand that already defines the name or destination is a
``ERR_DUPLICATE_OPTION`` configuration error.

``delimiter=","`` splits one value token into several values:
``--grid 64,64,128`` or ``--grid=64,64,128``. Each occurrence reads exactly
one token, so a following positional is never consumed, and ``nargs`` (two or
more, ``*``, or ``+``) bounds the number of items after splitting. Empty
items are kept and converted, so ``1,,3`` is an invalid integer. An
environment value is split at the delimiter instead of at blanks. Usage
shows ``GRID,GRID,GRID`` or ``SPECIES[,SPECIES...]``. Values beginning with
``-`` must be attached: ``--offsets=-1,2``.

``choice_help`` gives one help line per entry of ``choices``; help lists them
below the argument::

     --solver {cg,gmres}   linear solver
                             cg     conjugate gradient
                             gmres  generalized minimal residual

Registration is transactional. Invalid definitions append a structured
configuration error and are not partially committed.

Groups
~~~~~~

.. code-block:: fortran

   handle = parser%add_argument_group(title, description)
   handle = parser%add_mutually_exclusive_group(required, title, description)

Both functions return ``GroupHandle``. All dummy arguments except the ordinary
group's ``title`` are optional. Pass the result as ``group=handle`` to
``add_argument``. ``handle%is_valid()`` is false for a handle that was never
assigned by a parser.

Members of a mutually exclusive group must be omissible: a
``required=.true.`` option or a positional that needs a value is rejected with
``ERR_INVALID_GROUP``, because it would conflict with every other member. Use
``required=.true.`` on the group instead.

Argument rules
~~~~~~~~~~~~~~

.. code-block:: fortran

   call parser%add_requires(subject, needed)
   call parser%add_conflict(subject, others)
   call parser%add_required_unless(subject, alternatives)

Each call names already registered arguments by destination (``"output"``)
or by any option spelling (``"-o"``, ``"--output"``). The second argument is a
character scalar or a rank-one character array; give every element enough
length, because a truncated name does not match. Rules use explicit presence:
a default never satisfies or violates them.

* ``add_requires``: when ``subject`` is given, every ``needed`` argument must
  be given too; otherwise ``ERR_MISSING_DEPENDENCY``.
* ``add_conflict``: ``subject`` and any of ``others`` must not be given
  together; each conflicting argument adds ``ERR_ARGUMENT_CONFLICT``. One call
  covers both directions.
* ``add_required_unless``: ``subject`` must be given unless one of
  ``alternatives`` is; otherwise ``ERR_MISSING_REQUIRED``.

An unknown name, an empty list, a rule naming its own subject, a
``required_unless`` subject that is already required, and a conflict
involving a required argument are rejected with ``ERR_INVALID_RULE``. Rules
are copied by ``init_with_parents`` and kept by ``add_parser``.

Subcommands
~~~~~~~~~~~

.. code-block:: fortran

   call parser%add_subparsers(title, description, dest, required)
   call parser%add_parser(name, subparser, help, aliases)

All ``add_subparsers`` arguments are optional. Defaults are title ``commands``,
destination ``command``, and ``required=.false.``. ``name`` is required for
``add_parser``; ``subparser``, ``help``, and ``aliases`` are optional. A
supplied child is deep-copied and its complete program path is rebased under
the parent.

``aliases`` is a rank-one character array of alternative command spellings,
for example ``[character(len=2) :: "r", "ex"]``. An alias selects the command
under its registered name: the destination value and
``selected_command_path`` hold ``name``. Help lists ``name (r, ex)``; usage
shows registered names only. An alias that is not a valid command token, or
that repeats any registered name or alias, is a configuration error and the
command is not registered.

.. important::

   ``add_parser`` stores a **snapshot**. Arguments, groups, or nested
   subparsers added to the child variable after ``add_parser`` returns are not
   seen by the parent. Build each child completely before registering it.

Parsing and output
~~~~~~~~~~~~~~~~~~

.. code-block:: fortran

   result = parser%parse_tokens(tokens, environment, lengths)
   result = parser%try_parse_args()
   args = parser%parse_args()

   result = parser%parse_known_tokens(tokens, environment, lengths)
   result = parser%try_parse_known_args()
   args = parser%parse_known_args(extras)

``tokens`` is a rank-one character array. The optional ``environment`` is a
rank-one array of ``NAME=value`` entries that replaces the process
environment for ``env=`` options, which makes them testable; the last entry
for a name wins. Trailing blanks of a token are treated as padding of the
fixed-length array; pass ``lengths``, one integer per token, when they belong
to a value (``--sep ", "``) or to give an empty token. ``try_parse_args`` and
``parse_args`` pass the real lengths of the command-line arguments, so values
from the shell keep their trailing blanks. The first two functions return
``ParseResult`` and never print or terminate. ``parse_args`` returns
``Namespace`` only on success and implements the process-level print/exit
policy for help, version, warnings, and failures.

The ``known`` variants accept unknown options and surplus positionals and
return them in order, in ``result%extra_tokens`` or the allocatable character
array ``extras``, e.g. to leave options for MPI or PETSc. As in argparse, a
separate value of an unknown option is not recognized: in
``-ksp_type cg input.dat`` the positional receives ``cg``. Write such values
attached (``-ksp_type=cg``) or after the program's own arguments. Unknown
subcommands, clusters with an unknown letter, and invalid values are still
errors. Failures end with a normal
``stop 2`` so compiler runtimes do not print a backtrace for user mistakes.

If the parser definition itself is invalid (for example a duplicated option),
``parse_args`` prints each problem as ``prog: invalid parser definition:
argument -x/--output: ...`` and stops with status 70 instead of treating it as
a user error. ``parser%is_valid()`` and ``parser%get_config_errors()`` let
programs check this earlier.

As in argparse, ``-h`` or ``--version`` anywhere after an unrecognized option
or a stray positional still shows help or the version. Conversion and
validation errors are reported even when help follows.

Report your own checks the same way the parser reports usage errors:

.. code-block:: fortran

   if (.not. file_exists) call parser%error("input file does not exist")

``error`` prints the usage line and ``prog: error: <message>`` to
``error_unit`` and stops with status 2.

The parser is the single place diagnostics are rendered. ``parse_args``
uses the same functions, so library-mode callers that handle errors themselves
print exactly what a command-line user would see:

.. code-block:: fortran

   text = parser%format_diagnostic(entry)
   text = parser%format_diagnostics(result%errors)
   call parser%print_diagnostics(result%errors, unit)

``format_diagnostics`` joins one line per entry. ``print_diagnostics`` writes
to ``error_unit`` unless ``unit`` is given. Both accept any ``ErrorStack``,
including ``parser%get_config_errors()``.

Each line names the program and the severity, for example ``tool: error:
argument -j/--threads: value must not be less than 1 (got '0')`` for a fatal
diagnostic or ``tool: warning: argument --old: use --new`` for a warning.
Configuration errors read ``tool: invalid parser definition: ...``.

Token syntax
~~~~~~~~~~~~

The parser accepts, in order of precedence:

- an exact registered spelling, including multi-letter single-dash options
  such as ``-np``;
- ``--name=value`` and ``-n=value``, split at the first ``=``; the attached
  text is the only value of that occurrence and may begin with ``-``;
- ``-nVALUE`` for a one-letter option that takes values (``-j4``,
  ``-I/usr/include``);
- clusters of one-letter flags such as ``-vvn``; the first letter that takes
  values ends the cluster and receives the rest of the token (``-vj4``) or the
  following tokens (``-vj 4``).

A lone ``-`` is an ordinary value, conventionally standard input or output.
``--`` ends option processing. An attached value given to a flag produces
``ERR_UNEXPECTED_VALUE``; an attached value for an option that needs more than
one value produces ``ERR_MISSING_VALUE``.

Integer tokens must be an optional sign followed by decimal digits. Real
tokens are decimal literals with an optional ``e``, ``E``, ``d``, or ``D``
exponent, or ``inf``, ``infinity``, or ``nan``. Anything else, including
Fortran list-directed forms such as ``2*7`` or ``4,5``, is ``ERR_INVALID_VALUE``.

.. code-block:: fortran

   text = parser%format_usage()
   text = parser%format_help()
   call parser%print_usage(unit)
   call parser%print_help(unit)

The formatting functions are side-effect free. ``unit`` is optional.

Queries
~~~~~~~

.. code-block:: fortran

   n = parser%argument_count()
   n = parser%group_count()
   n = parser%rule_count()
   n = parser%subparser_count()
   found = parser%has_option(name)
   found = parser%has_dest(dest)
   valid = parser%is_valid()
   definition = parser%get_argument(index)
   errors = parser%get_config_errors()
   model = parser%get_help_model()

``get_argument`` returns an owned ``Argument`` snapshot for valid one-based
indices. ``get_help_model`` returns an owned data-only ``HelpModel`` snapshot.

Argument
--------

``Argument`` is the inspectable normalized definition returned by
``get_argument``. Its metadata includes allocated names and destination,
normalized ``NargsSpec``, data type, polymorphic action and validator,
``ValueBox`` values, help metadata, and lifecycle state. Useful queries are:

.. code-block:: fortran

   n = definition%name_count()
   yes = definition%matches_name("--output")
   yes = definition%is_positional()
   name = definition%primary_name()
   dest = definition%dest
   metavar = definition%effective_metavar()
   spelling = definition%nargs_display()
   value = definition%nargs_value()
   yes = definition%produces_list()

Callers should treat returned definitions as snapshots; mutating them does not
alter the parser.

Namespace
---------

``Namespace`` stores typed scalar and rank-one list values.

.. code-block:: fortran

   call args%get(key, scalar_or_list, stat)
   call args%set(key, scalar_or_list)
   call args%append(key, scalar, stat)
   present = args%contains(key)
   count = args%size()
   call args%merge(other, overwrite)
   call args%remove(key)
   call args%clear()

The generic ``get``, ``set``, and ``append`` support character, default
``integer`` and ``integer(int64)``, double-precision ``real`` (``real64``) and
``real(real32)``, and logical values. Plain integer literals and variables
need no kind parameter. Integers are stored with 64-bit range;
retrieving into a narrower kind returns ``ERR_NAMESPACE_OUT_OF_RANGE`` instead
of wrapping.

Strings and lists are returned in allocatable variables with their exact
stored length and size:

.. code-block:: fortran

   character(len=:), allocatable :: path
   character(len=:), allocatable :: tags(:)
   integer, allocatable :: sizes(:)

   call args%get("path", path)
   call args%get("tag", tags)
   call args%get("size", sizes)

A fixed-length ``character(len=n)`` variable does not match ``get`` and is a
compile-time error; assign from the allocatable result if a fixed buffer is
needed.

``stat`` follows Fortran's ``iostat`` convention. When it is present it
receives ``FCLAP_OK`` or a namespace error code and the program continues.
When it is absent, a failure (a mistyped key, a wrong variable type, or a
value outside the requested kind) stops the program with a message naming the
key. Values that always exist, such as
positionals, flags, and options with defaults, can therefore be read without
``stat``. Pass ``stat`` for optional values that may be absent:

.. code-block:: fortran

   call args%get("threads", threads)          ! always present: default 1
   call args%get("output", output, stat)      ! optional, no default
   if (stat == FCLAP_OK) call write_results(output)

Retrieval leaves the output unchanged on failure. ``merge`` deep-copies
values; ``overwrite`` defaults to true.

Custom actions store ready-made values with ``set_value(key, box)`` and
``append_value(key, box, stat)``, where ``box`` is a ``ValueBox``.

ParseResult
-----------

``ParseResult`` has public components:

.. list-table::
   :header-rows: 1
   :widths: 30 70

   * - Component
     - Meaning
   * - ``namespace``
     - ``Namespace`` populated atomically by successful actions.
   * - ``errors``
     - Ordered ``ErrorStack`` of fatal diagnostics and warnings.
   * - ``outcome``
     - A ``PARSE_*`` constant.
   * - ``text``
     - Optional help, version, or selected-child usage text.
   * - ``selected_command_path``
     - Optional root-to-leaf array of selected command names.
   * - ``extra_tokens``
     - Unknown tokens of a ``parse_known_*`` call; unallocated otherwise.

``result%value_source(dest)`` tells where the value stored under ``dest``
came from: ``SOURCE_COMMAND_LINE``, ``SOURCE_ENVIRONMENT``,
``SOURCE_DEFAULT`` (including implicit defaults such as ``.false.`` for a
flag), or ``SOURCE_NONE`` when the namespace holds no value for it. A
subcommand destination is ``SOURCE_COMMAND_LINE``. The constants are ordered
by precedence.

When a destination is stored both by a parser and by its selected
subcommand, the subcommand's value is kept unless the parent's value has a
higher-precedence source. A flag the user gave to the parent is therefore not
hidden by the child's default, while between two defaults the deeper one wins.

``result%succeeded()`` is true for success, help, and version.
``result%failed()`` is true only for ``PARSE_FAILURE``.

Errors
------

``ErrorEntry`` exposes ``code``, ``severity``, ``message``, and optional
``arg_name``, ``token``, ``token_index``, and ``suggestion``. ``token`` is the command-line
token that caused the diagnostic and ``token_index`` its one-based position on
the full command line, also inside subcommands. ``suggestion`` holds the
known option, subcommand, or string choice closest to a mistyped ``token``
(Jaro similarity above 0.7) and is unallocated when nothing is close enough. For configuration errors,
``token`` holds the definition's spelling, such as ``-x/--output``.
Render entries with ``parser%format_diagnostic(entry)``; the parser maps
destinations back to the option spellings users type. The numeric code is not
part of the text and remains available separately for structured handling.

``ErrorStack`` provides:

.. code-block:: fortran

   call errors%add(message, code, severity, arg_name, token, token_index, &
       suggestion)
   call errors%append(entry)
   call errors%merge(other)
   entry = errors%get(index)
   n = errors%count()
   yes = errors%has_errors()
   yes = errors%has_fatal_errors()
   yes = errors%has_warnings()
   call errors%offset_token_indices(offset)
   call errors%clear()

Outcome and severity constants are ``PARSE_SUCCESS``, ``PARSE_FAILURE``,
``PARSE_HELP``, ``PARSE_VERSION``, ``ERROR_FATAL``, and ``ERROR_WARNING``. The
stable ``ERR_*`` catalogue is documented in the design API contract.

Actions and validators
----------------------

Built-in action factories are:

.. code-block:: fortran

   store()
   store_const(value)
   store_true()
   store_false()
   boolean_optional()
   append()
   count()

``boolean_optional()`` adds a ``--no-name`` spelling for every long
``--name``: ``--cache`` stores true and ``--no-cache`` false, with the last
occurrence winning. Its implicit default is false; pass ``default=.true.`` for
features that are on unless disabled. Help lists both spellings.

``append()`` collects every occurrence in order. Explicit occurrences replace
a ``default=`` list rather than extending it, so ``--tag x`` with
``default=["base"]`` gives ``["x"]``. This deliberately differs from argparse.

Their concrete public types are ``StoreAction``, ``StoreConstAction``,
``StoreTrueAction``, ``StoreFalseAction``, ``BooleanOptionalAction``,
``AppendAction``, and ``CountAction``. Extend abstract ``ActionType`` by
implementing ``apply``. Override ``default_nargs``, ``accepts_nargs``,
``produces_list``, ``value_type``, or ``implicit_default`` when the action's
arity, list shape, value type, or implicit default differs from ``store``, and
optionally:

- ``option_names(names, expanded)`` to add spellings at registration;
- ``apply_option(option_name, ...)`` to react to the spelling that matched
  (it calls ``apply`` by default);
- ``replaces_default()`` to start from an empty value on the first explicit
  occurrence.

An implementation reports ``ACTION_CONTINUE``, ``ACTION_HELP_REQUESTED``, or
``ACTION_VERSION_REQUESTED`` through the action outcome argument.

Built-in validator factories are ``not_less_than(bound)``,
``not_bigger_than(bound)``, and ``in_range(lower, upper)``, returning
``LowerBoundValidator``, ``UpperBoundValidator``, and ``RangeValidator``. All
bounds are inclusive and accept default ``integer``, ``integer(int64)``,
double-precision, and ``real(real32)`` limits, e.g. ``in_range(1, 64)``.

The limit kind must match the argument type: ``not_less_than(0)`` on a
``real`` argument is rejected at registration with
``ERR_INCOMPATIBLE_VALIDATOR``; write ``not_less_than(0.0_wp)``. An
``in_range`` whose lower limit exceeds its upper limit is rejected the same
way.

Extend ``ValidatorType`` by implementing ``validate``. Override
``accepts_type(type_name)`` to reject argument types the validator cannot
check; the default accepts every type.

Formatting
----------

``StandardFormatter`` extends ``FormatterType`` and has configurable
``show_defaults``, ``show_choices``, ``raw_description``, ``raw_help_text``,
``help_width``, and ``width_from_env`` components. With
``width_from_env=.true.`` the width comes from the ``COLUMNS`` environment
variable when it holds a whole number of at least 20, otherwise from
``help_width``. Shells often do not export ``COLUMNS``, so the fallback is
common; the default stays off so that help text is deterministic:

.. code-block:: fortran

   type(StandardFormatter) :: formatter

   formatter = StandardFormatter(help_width=100)
   call parser%init(formatter=formatter)

The standard formatter follows argparse conventions: usage lists options
before positionals, arguments with choices and no explicit ``metavar`` show
the choices as metavar (``--mode {fast,slow}``), flags do not repeat their
implicit ``.false.`` default, and real values are printed in their shortest
exact form (``0.1``, ``1e-6``, ``2.5e10``).

A fixed ``nargs`` repeats the metavar once per value (``--grid N N N``, or
``--grid N,N,N`` with ``delimiter=","``). An explicit ``metavar`` that already
contains the separator describes all values and is shown as given, e.g.
``metavar="EX,EY,EZ"`` for ``nargs=3, delimiter=","`` renders
``--efield EX,EY,EZ``.

Custom formatters implement ``format_usage(model)`` and
``format_help(model)``. The public data-only model types are ``HelpArgument``,
``HelpGroup``, ``HelpCommand``, and ``HelpModel``.

Nargs, values, kinds, and version
---------------------------------

Construct and normalize ``NargsSpec`` values with:

.. code-block:: fortran

   spec = new_nargs(value)
   call normalize_nargs(value, spec, stat)

The type provides ``value``, ``is_valid``, ``min_count``, ``max_count``,
``is_variable``, ``produces_list``, and ``to_string`` queries. Public constants
include ``NARGS_ZERO``, ``NARGS_ONE``, ``NARGS_OPTIONAL``,
``NARGS_ZERO_OR_MORE``, ``NARGS_ONE_OR_MORE``, ``NARGS_REMAINDER``,
``NARGS_UNBOUNDED``, and ``NARGS_INVALID`` (the value of an invalid spec);
normalization reports ``NARGS_SUCCESS`` or ``NARGS_INVALID_VALUE``.

``ValueType``, ``ValueBox``, ``StringValue``, ``IntegerValue``, ``RealValue``,
``LogicalValue``, ``ListValue``, and ``new_value`` support custom action and
validator extension code. Normal applications normally use ``Namespace``
instead.

Integer interfaces exist for both 32-bit and 64-bit kinds, so default
``integer`` literals and variables match on every supported compiler; no kind
suffix is needed.
Real interfaces exist for ``real64`` and ``real32``; any project kind with the
same precision, such as a ``wp = selected_real_kind(15)``, matches directly.
A default-kind real literal such as ``default=0.1`` is widened to double
precision and keeps its single-precision error; write ``0.1_wp`` with your
project's own kind, or ``real64`` from ``iso_fortran_env``.

Version information is available through ``fclap_version_string``,
``fclap_version_compact``, and:

.. code-block:: fortran

   call get_fclap_version(major, minor, patch, string)

Every output argument is optional.
