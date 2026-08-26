Migrating from the legacy prototype
===================================

Phase 8 removed the ``old/`` prototype after its retained behavior was mapped
to named Test Drive cases. The rewrite is behavior-oriented rather than source
compatible: parser ownership, extension points, error handling, and value
storage were deliberately redesigned.

Use only the facade module
--------------------------

Replace imports of ``fclap_parser``, ``fclap_actions``,
``fclap_namespace``, ``fclap_formatter``, and ``fclap_constants`` with:

.. code-block:: fortran

   use fclap, only : ArgumentParser, Namespace, store_true

Internal modules beginning with ``fclap_`` are compiler dependencies and are
not separately supported application interfaces.

Actions are values, not strings
-------------------------------

Pass an action factory result instead of a magic character value:

.. list-table::
   :header-rows: 1
   :widths: 40 60

   * - Legacy spelling
     - Current spelling
   * - ``action="store"``
     - omit ``action`` or use ``action=store()``
   * - ``action="store_true"``
     - ``action=store_true()``
   * - ``action="store_false"``
     - ``action=store_false()``
   * - ``action="count"``
     - ``action=count()``
   * - ``action="append"``
     - ``action=append()``
   * - bound encoded in an action string
     - ``validator=not_less_than(value)`` or
       ``validator=not_bigger_than(value)``

Custom behavior extends ``ActionType`` or ``ValidatorType``. The token engine
does not dispatch on user-defined strings.

Registration keywords changed
-----------------------------

.. list-table::
   :header-rows: 1
   :widths: 38 62

   * - Legacy
     - Current
   * - ``default_val=``
     - ``default=``
   * - integer status constants
     - ``deprecated_msg=`` or ``removed_msg=``
   * - ``group_idx=`` / ``mutex_group_idx=``
     - obtain ``GroupHandle`` and pass ``group=handle``
   * - integer ``ARG_*`` sentinels
     - positive counts or ``nargs="?"``, ``"*"``, ``"+"``,
       ``"remainder"``
   * - parent references retained by the child
     - ``init_with_parents([parents])`` deep-copies definitions

The facade accepts up to four aliases, but stores them dynamically. All
registration arrays grow as needed; the legacy ``MAX_ACTIONS``,
``MAX_CHOICES``, ``MAX_GROUPS``, ``MAX_LIST_VALUES``, and related capacity
constants no longer exist.

Parsing and errors
------------------

The legacy parser mixed token consumption, printing, mutable seen-state, and
termination. Choose the current entry point by policy:

.. list-table::
   :header-rows: 1
   :widths: 30 70

   * - Entry point
     - Policy
   * - ``parse_tokens(tokens)``
     - Explicit tokens; returns ``ParseResult``; never prints or terminates.
   * - ``try_parse_args()``
     - Process arguments; returns ``ParseResult``; never prints or terminates.
   * - ``parse_args()``
     - argparse-like process facade; prints help/errors and exits when needed.

Each invocation creates new parse state, so defaults do not count as supplied
and repeated parses do not share seen flags. Expected failures are represented
by ``PARSE_FAILURE`` and stable ``ErrorEntry`` codes instead of an inaccessible
``last_error`` or an unconditional stop.

Namespace access is typed
-------------------------

Use the generic getter with an output of the intended type:

.. code-block:: fortran

   integer(ip) :: count
   integer :: stat

   call args%get("count", count, stat)
   if (stat /= FCLAP_OK) then
       ! missing key or type mismatch
   end if

Lists are returned through allocatable rank-one outputs. Retrieval never
silently converts between numeric types and never truncates to a legacy fixed
capacity.

Groups and subcommands own stable data
--------------------------------------

Group indices were replaced by parser-owned ``GroupHandle`` values. A handle
from another parser, or from before reinitialization, is rejected.

``add_parser`` stores a deep child snapshot. Child groups, formatter, errors,
and nested commands remain independent. Parsed child namespace values are
merged into the parent result, and ``selected_command_path`` records recursive
dispatch.

Formatting
----------

Help rendering consumes an owned ``HelpModel`` snapshot. Custom formatters
extend ``FormatterType`` and cannot mutate the parser. ``format_help`` and
``format_usage`` are deterministic and side-effect free; ``print_help`` and
``print_usage`` are thin output wrappers.

Compatibility boundary
----------------------

Long-option abbreviation, combined short flags, attached option values,
``--option=value``, and ``parse_known_args`` are not currently implemented.
See the feature matrix for the explicit supported/deferred decisions. The
complete historical routine-to-test deletion gate is recorded in
``docs/design/legacy-test-map.md`` in the source distribution.
