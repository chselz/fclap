Tutorial
========

This tutorial will guide you through using **fclap** to build command-line 
interfaces for your Fortran applications. We'll start with the basics and 
progress to more advanced features.

Installation
------------

Using fpm (Fortran Package Manager)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Add fclap to your ``fpm.toml`` file:

.. code-block:: toml

   [dependencies]
   fclap = { git = "https://github.com/your-org/fclap.git" }

Then build your project:

.. code-block:: bash

   fpm build

Manual Installation
~~~~~~~~~~~~~~~~~~~

Clone the repository and include the source files in your project:

.. code-block:: bash

   git clone https://github.com/your-org/fclap.git
   # Copy src/*.f90 to your project's source directory

The Basics
----------

Creating a Parser
~~~~~~~~~~~~~~~~~

Every fclap application starts by creating an ``ArgumentParser`` object:

.. code-block:: fortran

   program basic_example
       use fclap, only: ArgumentParser, Namespace
       
       type(ArgumentParser) :: parser
       type(Namespace) :: args
       
       ! Initialize with program name and description
       call parser%init(prog="myapp", &
                        description="A simple example application")
       
       ! Parse the command line
       args = parser%parse_args()
   end program basic_example

The ``init`` subroutine accepts several optional arguments:

- ``prog``: The program name (defaults to the actual program name)
- ``description``: Text displayed before the argument help
- ``epilog``: Text displayed after the argument help
- ``add_help``: Whether to add ``-h/--help`` automatically (default: ``.true.``)
- ``version``: Version string (adds ``-V/--version`` if provided)

Adding Positional Arguments
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Positional arguments are required and must appear in order:

.. code-block:: fortran

   program positional_example
       use fclap, only: ArgumentParser, Namespace
       
       type(ArgumentParser) :: parser
       type(Namespace) :: args
       character(len=256) :: input_file, output_file
       
       call parser%init(prog="converter", &
                        description="Convert files between formats")
       
       ! Add positional arguments
       call parser%add_argument("input", help="Input file path")
       call parser%add_argument("output", help="Output file path")
       
       args = parser%parse_args()
       
       ! Retrieve values
       call args%get("input", input_file)
       call args%get("output", output_file)
       
       print '(A,A)', "Input:  ", trim(input_file)
       print '(A,A)', "Output: ", trim(output_file)
   end program positional_example

Running this program:

.. code-block:: bash

   $ ./converter data.txt result.csv
   Input:  data.txt
   Output: result.csv
   
   $ ./converter --help
   usage: converter input output
   
   Convert files between formats
   
   positional arguments:
     input         Input file path
     output        Output file path
   
   optional arguments:
     -h, --help    show this help message and exit

Adding Optional Arguments (Flags)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Optional arguments start with ``-`` or ``--`` and can appear in any order:

.. code-block:: fortran

   program optional_example
       use fclap, only: ArgumentParser, Namespace
       
       type(ArgumentParser) :: parser
       type(Namespace) :: args
       type :: argsholder
         character(len=256) :: filename, output
         logical :: verbose
         integer :: count
       end type argsholder
       
       call parser%init(prog="processor")
       
       ! Positional argument
       call parser%add_argument("filename", help="File to process")
       
       ! Optional arguments
       call parser%add_argument("-v", "--verbose", action="store_true", &
                                help="Enable verbose output")
       call parser%add_argument("-o", "--output", default="out.txt", &
                                help="Output file (default: out.txt)")
       call parser%add_argument("-c", "--count", type="int", default="1", &
                                help="Number of iterations")
       
       args = parser%parse_args()
       
       call args%get("filename", argsholder%filename)
       call args%get("output", argsholder%output)
       call args%get("verbose", argsholder%verbose)
       call args%get("count", argsholder%count)
       
       if (argsholder%verbose) print *, "Verbose mode enabled"
       print '(A,A)', "Processing: ", trim(argsholder%filename)
       print '(A,A)', "Output to:  ", trim(argsholder%output)
       print '(A,I0,A)', "Running ", argsholder%count, " iterations"
   end program optional_example

Running this program:

.. code-block:: bash

   $ ./processor input.dat -v -o results.txt --count 5
   Verbose mode enabled
   Processing: input.dat
   Output to:  results.txt
   Running 5 iterations

Types and Nargs
---------------

Argument Types
~~~~~~~~~~~~~~

fclap supports four value types specified with the ``type`` parameter:

- ``"string"`` (default): Character strings
- ``"int"``: Integer values
- ``"real"``: Real/floating-point values  
- ``"logical"``: Boolean values (typically used with store_true/store_false)

.. code-block:: fortran

   program types_example
       use fclap, only: ArgumentParser, Namespace
       
       type(ArgumentParser) :: parser
       type(Namespace) :: args
       character(len=256) :: name
       integer :: iterations
       real :: threshold
       logical :: debug
       
       call parser%init(prog="typed_args")
       
       call parser%add_argument("-n", "--name", type="string", &
                                help="Name string")
       call parser%add_argument("-i", "--iterations", type="int", &
                                default="10", help="Number of iterations")
       call parser%add_argument("-t", "--threshold", type="real", &
                                default="0.5", help="Threshold value")
       call parser%add_argument("-d", "--debug", action="store_true", &
                                help="Enable debug mode")
       
       args = parser%parse_args()
       
       call args%get("name", name)
       call args%get("iterations", iterations)
       call args%get("threshold", threshold)
       call args%get("debug", debug)
   end program types_example

Nargs: Consuming Multiple Values
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``nargs`` parameter controls how many command-line arguments are consumed:

- ARG_OPTIONAL: Zero or one argument
- ARG_ZERO_OR_MORE: Zero or more arguments
- ARG_ONE_OR_MORE: One or more arguments (required)
- Positive integer: Exact number of arguments

.. code-block:: fortran

   program nargs_example
       use fclap, only: ArgumentParser, Namespace, ARG_ONE_OR_MORE
       
       type(ArgumentParser) :: parser
       type(Namespace) :: args
       character(len=256) :: files(64)
       integer :: num_files, i
       
       call parser%init(prog="multi_file")
       
       ! Accept one or more input files
       call parser%add_argument("files", nargs=ARG_ONE_OR_MORE, &
                                help="Input files to process")
       
       args = parser%parse_args()
       
       ! Get the list of files
       call args%get_string_list("files", files, num_files)
       
       print '(A,I0,A)', "Processing ", num_files, " files:"
       do i = 1, num_files
           print '(A,A)', "  - ", trim(files(i))
       end do
   end program nargs_example

Actions
-------

The ``action`` parameter specifies what to do when an argument is encountered:

store (default)
~~~~~~~~~~~~~~~

Stores the argument value:

.. code-block:: fortran

   call parser%add_argument("-n", "--name", action="store", help="Your name")

store_true / store_false
~~~~~~~~~~~~~~~~~~~~~~~~

Stores ``.true.`` or ``.false.`` when the flag is present:

.. code-block:: fortran

   call parser%add_argument("-v", "--verbose", action="store_true", &
                            help="Enable verbose mode")
   call parser%add_argument("-q", "--quiet", action="store_false", &
                            help="Disable output")

count
~~~~~

Counts the number of times a flag appears:

.. code-block:: fortran

   program count_example
       use fclap, only: ArgumentParser, Namespace
       
       type(ArgumentParser) :: parser
       type(Namespace) :: args
       integer :: verbosity
       
       call parser%init(prog="verbose_app")
       
       call parser%add_argument("-v", "--verbose", action="count", &
                                help="Increase verbosity (use multiple times)")
       
       args = parser%parse_args()
       call args%get_integer("verbose", verbosity)
       
       print '(A,I0)', "Verbosity level: ", verbosity
   end program count_example

.. code-block:: bash

   $ ./verbose_app -v -v -v
   Verbosity level: 3

append
~~~~~~

Appends values to a list (allows repeated use of the same flag):

.. code-block:: fortran

   program append_example
       use fclap, only: ArgumentParser, Namespace
       
       type(ArgumentParser) :: parser
       type(Namespace) :: args
       character(len=256) :: include_dirs(64)
       integer :: num_dirs, i
       
       call parser%init(prog="compiler")
       
       call parser%add_argument("-I", "--include", action="append", &
                                help="Add include directory")
       
       args = parser%parse_args()
       call args%get_string_list("include", include_dirs, num_dirs)
       
       print '(A,I0,A)', "Include directories (", num_dirs, "):"
       do i = 1, num_dirs
           print '(A,A)', "  ", trim(include_dirs(i))
       end do
   end program append_example

.. code-block:: bash

   $ ./compiler -I /usr/include -I ./src -I ../lib
   Include directories (3):
     /usr/include
     ./src
     ../lib

special Actions
~~~~~~~~~~~~~~~

The ``not_less_than()`` and the ``not_bigger_than()`` actions allow to check that a ``int``, ``real`` and ``character`` is not smaller or bigger, respectively than a certain value.

.. code-block:: fortran
   program nargs_example
      use fclap, only: ArgumentParser, Namespace, not_bigger_than
      implicit none

      type(ArgumentParser) :: parser
      type(Namespace) :: args
      integer :: count_val

      call parser%init(prog="multi_file")

      ! Accept a count that must be >= 5
      call parser%add_argument("count", data_type="int", action=not_bigger_than(5), &
                              help="A count value (must be >= 5)")

      args = parser%parse_args()

      call args%get("count", count_val)

      print '(A,I0)', "Count value: ", count_val
   end program nargs_example

Advanced Features
-----------------

Argument Groups
~~~~~~~~~~~~~~~

Groups organize related arguments in the help output:

.. code-block:: fortran

   program groups_example
       use fclap, only: ArgumentParser, Namespace, ArgumentGroup
       
       type(ArgumentParser) :: parser
       type(ArgumentGroup) :: io_group, proc_group
       type(Namespace) :: args
       
       call parser%init(prog="processor", &
                        description="Data processing application")
       
       ! Create input/output group
       io_group = parser%add_argument_group("Input/Output Options", &
                      description="Control input and output files")
       call parser%add_argument("-i", "--input", group=io_group, &
                                help="Input file")
       call parser%add_argument("-o", "--output", group=io_group, &
                                help="Output file")
       
       ! Create processing group
       proc_group = parser%add_argument_group("Processing Options", &
                        description="Control processing behavior")
       call parser%add_argument("-n", "--iterations", type="int", &
                                group=proc_group, help="Number of iterations")
       call parser%add_argument("-t", "--threads", type="int", &
                                group=proc_group, help="Number of threads")
       
       args = parser%parse_args()
   end program groups_example

Mutually Exclusive Groups
~~~~~~~~~~~~~~~~~~~~~~~~~

Ensure only one option from a group is used:

.. code-block:: fortran

   program mutex_example
       use fclap, only: ArgumentParser, Namespace, MutuallyExclusiveGroup
       
       type(ArgumentParser) :: parser
       type(MutuallyExclusiveGroup) :: verbosity_group
       type(Namespace) :: args
       
       call parser%init(prog="app")
       
       ! Create a mutually exclusive group - user can only pick one
       verbosity_group = parser%add_mutually_exclusive_group(required=.true.)
       
       call parser%add_argument("-v", "--verbose", action="store_true", &
                                mutex_group=verbosity_group, &
                                help="Verbose output")
       call parser%add_argument("-q", "--quiet", action="store_true", &
                                mutex_group=verbosity_group, &
                                help="Quiet output")
       
       args = parser%parse_args()
   end program mutex_example

.. code-block:: bash

   $ ./app -v -q
   error: argument -q: not allowed with argument -v

Parent Parsers
~~~~~~~~~~~~~~

Share common arguments across multiple parsers:

.. code-block:: fortran

   program parents_example
       use fclap, only: ArgumentParser, Namespace
       
       type(ArgumentParser) :: parent, child
       type(Namespace) :: args
       
       ! Create parent with common arguments
       call parent%init(add_help=.false.)
       call parent%add_argument("-v", "--verbose", action="store_true", &
                                help="Enable verbose mode")
       call parent%add_argument("--config", help="Config file path")
       
       ! Create child that inherits from parent
       call child%init_with_parents([parent], prog="myapp", &
                                    description="Application with inherited args")
       
       ! Add child-specific arguments
       call child%add_argument("input", help="Input file")
       
       args = child%parse_args()
   end program parents_example

Subparsers (Subcommands)
~~~~~~~~~~~~~~~~~~~~~~~~

Create git-style subcommands:

.. code-block:: fortran

   program subcommand_example
       use fclap, only: ArgumentParser, Namespace
       
       type(ArgumentParser) :: parser, clone_parser, commit_parser
       type(Namespace) :: args
       character(len=64) :: command
       
       call parser%init(prog="mygit", description="A simple VCS")
       
       ! Enable subparsers
       call parser%add_subparsers(title="commands", dest="command")
       
       ! Add 'clone' subcommand
       clone_parser = parser%add_parser("clone", help="Clone a repository")
       call clone_parser%add_argument("url", help="Repository URL")
       call clone_parser%add_argument("-b", "--branch", help="Branch to clone")
       
       ! Add 'commit' subcommand
       commit_parser = parser%add_parser("commit", help="Commit changes")
       call commit_parser%add_argument("-m", "--message", required=.true., &
                                       help="Commit message")
       
       args = parser%parse_args()
       
       ! Check which command was used
       call args%get_string("command", command)
       
       select case(trim(command))
       case("clone")
           print *, "Cloning repository..."
       case("commit")
           print *, "Committing changes..."
       end select
   end program subcommand_example

.. code-block:: bash

   $ ./mygit --help
   usage: mygit [-h] {clone,commit}
   
   A simple VCS
   
   commands:
     clone        Clone a repository
     commit       Commit changes
   
   $ ./mygit clone https://github.com/user/repo.git -b main
   Cloning repository...

Version Information
~~~~~~~~~~~~~~~~~~~

Add version information to your application:

.. code-block:: fortran

   program version_example
       use fclap, only: ArgumentParser, Namespace
       
       type(ArgumentParser) :: parser
       type(Namespace) :: args
       
       call parser%init(prog="myapp", &
                        description="My Application", &
                        version="myapp 1.2.3")
       
       args = parser%parse_args()
   end program version_example

.. code-block:: bash

   $ ./myapp --version
   myapp 1.2.3

Error Handling
--------------

fclap automatically handles common errors and displays helpful messages:

.. code-block:: bash

   # Missing required argument
   $ ./app
   usage: app [-h] filename
   error: the following arguments are required: filename
   
   # Invalid type
   $ ./app --count abc
   error: argument --count: invalid integer value -- abc
   
   # Unknown argument
   $ ./app --unknown
   error: unrecognized arguments: --unknown

Complete Example
----------------

Here's a complete example combining multiple features:

.. code-block:: fortran

   program complete_example
       use fclap, only: ArgumentParser, Namespace, ARG_ONE_OR_MORE
       
       type(ArgumentParser) :: parser
       type(Namespace) :: args
       character(len=256) :: output, files(64), format_str
       integer :: num_files, verbosity, i
       logical :: force
       
       call parser%init( &
           prog="processor", &
           description="A comprehensive file processing utility", &
           epilog="For more information, visit https://github.com/fclap", &
           version="processor 2.0.0" &
       )
       
       ! Positional: one or more input files
       call parser%add_argument("files", nargs=ARG_ONE_OR_MORE, &
                                metavar="FILE", &
                                help="Input files to process")
       
       ! Optional arguments
       call parser%add_argument("-o", "--output", default="output.dat", &
                                help="Output file (default: output.dat)")
       call parser%add_argument("-f", "--format", &
                                choices=["csv ", "json", "xml "], &
                                default="csv", &
                                help="Output format: csv, json, or xml")
       call parser%add_argument("-v", "--verbose", action="count", &
                                help="Increase verbosity (-v, -vv, -vvv)")
       call parser%add_argument("--force", action="store_true", &
                                help="Overwrite existing output file")
       
       args = parser%parse_args()
       
       ! Retrieve all values
       call args%get("files", files, num_files)
       call args%get("output", output)
       call args%get("format", format_str)
       call args%get("verbose", verbosity)
       call args%get("force", force)
       
       ! Use the values
       if (verbosity >= 1) then
           print '(A)', "=== Processing Configuration ==="
           print '(A,I0)', "Verbosity level: ", verbosity
           print '(A,A)', "Output file: ", trim(output)
           print '(A,A)', "Output format: ", trim(format_str)
           print '(A,L1)', "Force overwrite: ", force
           print '(A,I0)', "Number of input files: ", num_files
       end if
       
       if (verbosity >= 2) then
           print '(A)', "Input files:"
           do i = 1, num_files
               print '(A,A)', "  - ", trim(files(i))
           end do
       end if
       
       print '(A)', "Processing complete!"
   end program complete_example

.. code-block:: bash

   $ ./processor -vv --format json -o result.json file1.txt file2.txt
   === Processing Configuration ===
   Verbosity level: 2
   Output file: result.json
   Output format: json
   Force overwrite: F
   Number of input files: 2
   Input files:
     - file1.txt
     - file2.txt
   Processing complete!
