API Reference
=============

This section provides comprehensive documentation for all public types, 
procedures, and constants in the fclap library.

Main Public Interface
---------------------

fclap Module
~~~~~~~~~~~~

The main entry point module that re-exports all public symbols.

.. code-block:: fortran

   use fclap, only: ArgumentParser, Namespace

.. fortran:automodule:: fclap
   :members:
   :undoc-members:
   :show-inheritance:

Core Types
----------

ArgumentParser
~~~~~~~~~~~~~~

The main parser type for defining and parsing command-line arguments.

**Type Components:**

- ``prog`` - Program name displayed in help/usage
- ``description`` - Description text shown before arguments
- ``epilog`` - Text shown after arguments in help
- ``version`` - Version string for ``--version`` flag
- ``add_help`` - Whether to add ``-h/--help`` automatically

**Type-Bound Procedures:**

.. code-block:: fortran

   ! Initialize the parser
   call parser%init(prog, description, epilog, add_help, version)
   
   ! Initialize with parent parsers
   call parser%init_with_parents(parents, prog, description, epilog, add_help, version)
   
   ! Add an argument
   call parser%add_argument(name_or_flags, ...)
   
   ! Add an argument group
   group = parser%add_argument_group(title, description)
   
   ! Add a mutually exclusive group
   mutex = parser%add_mutually_exclusive_group(required)
   
   ! Enable subparsers
   call parser%add_subparsers(title, dest)
   
   ! Add a subparser
   subparser = parser%add_parser(name, help)
   
   ! Parse command-line arguments
   args = parser%parse_args()
   
   ! Print help message
   call parser%print_help()
   
   ! Print usage message
   call parser%print_usage()

.. fortran:automodule:: fclap_parser
   :members:
   :show-inheritance:

Namespace
~~~~~~~~~

Container for parsed argument values. Provides dictionary-like access to results.

**Type-Bound Procedures:**

.. code-block:: fortran

   ! String value retrieval
   call args%get_string(key, value)
   
   ! Integer value retrieval
   call args%get_integer(key, value)
   
   ! Real value retrieval
   call args%get_real(key, value)
   
   ! Logical value retrieval
   call args%get_logical(key, value)
   
   ! String list retrieval (for nargs or append)
   call args%get_string_list(key, values, count)
   
   ! Integer list retrieval
   call args%get_integer_list(key, values, count)
   
   ! Check if key exists
   exists = args%has_key(key)
   
   ! Debug output
   call args%show()

.. fortran:automodule:: fclap_namespace
   :members:
   :show-inheritance:

Action
~~~~~~

Represents a single argument definition with all its properties.

**Type Components:**

- ``dest`` - Destination name for storage
- ``option_strings`` - Array of option flags (e.g., "-v", "--verbose")
- ``nargs`` - Number of arguments to consume
- ``help_text`` - Help description
- ``required`` - Whether argument is required
- ``default_value`` - Default value container
- ``metavar`` - Display name in usage
- ``choices`` - Array of valid choices
- ``value_type`` - Expected value type
- ``is_positional`` - Whether positional argument
- ``action_type`` - Type of action to perform

.. fortran:automodule:: fclap_actions
   :members:
   :show-inheritance:

Supporting Types
----------------

ValueContainer
~~~~~~~~~~~~~~

Generic container for storing argument values of different types.

**Type Components:**

- ``value_type`` - Type indicator (TYPE_STRING, TYPE_INTEGER, etc.)
- ``string_value`` - String storage
- ``integer_value`` - Integer storage
- ``real_value`` - Real storage
- ``logical_value`` - Logical storage
- ``string_list`` - String array storage (for append/nargs)
- ``integer_list`` - Integer array storage
- ``real_list`` - Real array storage
- ``list_count`` - Number of items in list
- ``is_set`` - Whether value has been set

ArgumentGroup
~~~~~~~~~~~~~

Organizes related arguments in the help output.

**Type Components:**

- ``title`` - Group title
- ``description`` - Group description
- ``required`` - Whether group is required

MutuallyExclusiveGroup
~~~~~~~~~~~~~~~~~~~~~~

Ensures only one argument from the group can be used.

**Type Components:**

- ``title`` - Group title
- ``required`` - Whether one option must be chosen

fclap_error
~~~~~~~~~~~

Error handling type for parser errors.

**Type Components:**

- ``message`` - Error message
- ``argument`` - Related argument name
- ``has_error`` - Error state flag

**Type-Bound Procedures:**

.. code-block:: fortran

   ! Initialize error
   call error%init(message, argument)
   
   ! Report error to output
   call error%report(unit)
   
   ! Clear error state
   call error%clear()

Constants
---------

Nargs Specifiers
~~~~~~~~~~~~~~~~

Control how many command-line arguments are consumed:

.. code-block:: fortran

   ARG_OPTIONAL      ! '?' - zero or one argument
   ARG_ZERO_OR_MORE  ! '*' - zero or more arguments
   ARG_ONE_OR_MORE   ! '+' - one or more arguments (required)
   ARG_PARSER        ! For subparser mode
   ARG_REMAINDER     ! '...' - all remaining arguments
   ARG_SINGLE        ! Single argument (default)

Type Specifiers
~~~~~~~~~~~~~~~

Identify the expected value type:

.. code-block:: fortran

   TYPE_STRING   ! Character string values
   TYPE_INTEGER  ! Integer values
   TYPE_REAL     ! Real/floating-point values
   TYPE_LOGICAL  ! Boolean values

Action Types
~~~~~~~~~~~~

Specify what action to perform when argument is encountered:

.. code-block:: fortran

   ACT_STORE        ! Store single value (default)
   ACT_STORE_TRUE   ! Store .true. when flag present
   ACT_STORE_FALSE  ! Store .false. when flag present
   ACT_COUNT        ! Count flag occurrences
   ACT_APPEND       ! Append value to list
   ACT_HELP         ! Print help and exit
   ACT_VERSION      ! Print version and exit

Argument Status
~~~~~~~~~~~~~~~

Control argument lifecycle:

.. code-block:: fortran

   STATUS_ACTIVE      ! Normal active argument
   STATUS_DEPRECATED  ! Deprecated with warning
   STATUS_REMOVED     ! Removed, will error

Group Types
~~~~~~~~~~~

Identify argument group types:

.. code-block:: fortran

   GROUP_STANDARD  ! Standard group for organization
   GROUP_MUTEX     ! Mutually exclusive group

Special Values
~~~~~~~~~~~~~~

.. code-block:: fortran

   SUPPRESS  ! Suppress default/help display

Utility Functions
-----------------

.. code-block:: fortran

   ! Get program name from command line
   prog_name = get_prog_name(override)
   
   ! Get fclap version components
   call get_fclap_version(major, minor, patch)
   
   ! Get compact version string (e.g., "1.2.3")
   version_str = fclap_version_compact()
   
   ! Get full version string (e.g., "fclap version 1.2.3")
   version_str = fclap_version_string()
