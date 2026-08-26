Public API reference
====================

Supported module
----------------

Applications import the ``fclap`` facade:

.. code-block:: fortran

   use fclap, only : ArgumentParser, Namespace, ParseResult

The installed CMake target is ``fclap::fclap`` and the pkg-config package is
``fclap``. Internal modules prefixed with ``fclap_`` are implementation
dependencies and may change without compatibility guarantees.

ArgumentParser
--------------

``ArgumentParser`` owns an immutable-by-parsing command-line definition. Its
data components are private.

Construction
~~~~~~~~~~~~

.. code-block:: fortran

   call parser%init(prog, usage, description, epilog, version, &
                    formatter, add_help)

All arguments are optional. ``formatter`` is ``class(FormatterType)`` and is
cloned. ``add_help`` defaults to true.

.. code-block:: fortran

   call parser%init_with_parents(parents, prog, usage, description, epilog, &
                                version, formatter, add_help)

``parents`` is ``type(ArgumentParser) :: parents(:)``. Definitions are copied
deeply in array order after the receiving parser is initialized.

Argument registration
~~~~~~~~~~~~~~~~~~~~~

.. code-block:: fortran

   call parser%add_argument(name1, name2, name3, name4, action, nargs, &
       data_type, default, const, choices, validator, required, help, &
       metavar, dest, visible, deprecated_msg, removed_msg, print_default, &
       print_choices, group)

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
   * - ``required``, ``visible``, ``print_default``, ``print_choices``
     - logical scalar
   * - ``group``
     - ``type(GroupHandle)``

Registration is transactional. Invalid definitions append a structured
configuration error and are not partially committed.

Groups
~~~~~~

.. code-block:: fortran

   handle = parser%add_argument_group(title, description)
   handle = parser%add_mutually_exclusive_group(required, title, description)

Both functions return ``GroupHandle``. All dummy arguments except the ordinary
group's ``title`` are optional. Pass the result as ``group=handle`` to
``add_argument``.

Subcommands
~~~~~~~~~~~

.. code-block:: fortran

   call parser%add_subparsers(title, description, dest, required)
   call parser%add_parser(name, subparser, help_text)

All ``add_subparsers`` arguments are optional. Defaults are title ``commands``,
destination ``command``, and ``required=.false.``. ``name`` is required for
``add_parser``; ``subparser`` and ``help_text`` are optional. A supplied child
is deep-copied and its complete program path is rebased under the parent.

Parsing and output
~~~~~~~~~~~~~~~~~~

.. code-block:: fortran

   result = parser%parse_tokens(tokens)
   result = parser%try_parse_args()
   args = parser%parse_args()

``tokens`` is a rank-one character array. The first two functions return
``ParseResult`` and never print or terminate. ``parse_args`` returns
``Namespace`` only on success and implements the process-level print/exit
policy for help, version, warnings, and failures.

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
   dest = definition%derive_dest()
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

   call args%get(key, scalar_or_allocatable_list, stat)
   call args%set(key, scalar_or_list)
   call args%append(key, scalar, stat)
   present = args%contains(key)
   present = args%has_key(key)
   count = args%size()
   call args%merge(other, overwrite)
   call args%clear()

The generic ``get``, ``set``, and ``append`` support character, ``integer(ip)``,
``real(wp)``, and logical values. List outputs from ``get`` are allocatable.
The optional ``stat`` is ``FCLAP_OK`` or a namespace error code. Retrieval
leaves the output unchanged on failure. ``merge`` deep-copies values;
``overwrite`` defaults to true.

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

``result%succeeded()`` is true for success, help, and version.
``result%failed()`` is true only for ``PARSE_FAILURE``.

Errors
------

``ErrorEntry`` exposes ``code``, ``severity``, ``message``, optional
``arg_name`` and ``flag``, and ``flag_index``. ``entry%to_string()`` returns a
deterministic diagnostic without including the numeric error code. The code
remains available separately for structured handling.

``ErrorStack`` provides:

.. code-block:: fortran

   call errors%add(message, code, severity, arg_name, token, token_index)
   call errors%append(entry)
   call errors%merge(other)
   entry = errors%get(index)
   n = errors%count()
   yes = errors%has_errors()
   yes = errors%has_fatal_errors()
   yes = errors%has_warnings()
   text = errors%format_all()
   call errors%print_all(unit)
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
   append()
   count()

Their concrete public types are ``StoreAction``, ``StoreConstAction``,
``StoreTrueAction``, ``StoreFalseAction``, ``AppendAction``, and
``CountAction``. Extend abstract ``ActionType`` by implementing ``apply``;
override its arity, list-shape, value-type, or implicit-default methods when
needed. An implementation reports ``ACTION_CONTINUE``,
``ACTION_HELP_REQUESTED``, or ``ACTION_VERSION_REQUESTED`` through the action
outcome argument.

Built-in validator factories are ``not_less_than(bound)`` and
``not_bigger_than(bound)``, returning ``LowerBoundValidator`` and
``UpperBoundValidator``. Extend ``ValidatorType`` by implementing ``validate``.

Formatting
----------

``StandardFormatter`` extends ``FormatterType`` and has configurable
``show_defaults``, ``show_choices``, ``raw_description``, ``raw_help_text``,
and ``help_width`` components:

.. code-block:: fortran

   type(StandardFormatter) :: formatter

   formatter = StandardFormatter(help_width=100)
   call parser%init(formatter=formatter)

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
``NARGS_ZERO_OR_MORE``, ``NARGS_ONE_OR_MORE``, ``NARGS_REMAINDER``, and
``NARGS_UNBOUNDED``; normalization reports ``NARGS_SUCCESS`` or
``NARGS_INVALID_VALUE``.

``ValueType``, ``ValueBox``, ``StringValue``, ``IntegerValue``, ``RealValue``,
``LogicalValue``, ``ListValue``, and ``new_value`` support custom action and
validator extension code. Normal applications normally use ``Namespace``
instead.

``ip`` and ``wp`` are the integer and real storage kinds used by the public
typed interfaces.

Version information is available through ``fclap_version_string``,
``fclap_version_compact``, and:

.. code-block:: fortran

   call get_fclap_version(major, minor, patch, string)

Every output argument is optional.
