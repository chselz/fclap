Welcome to fclap's documentation!
==================================

**fclap** (Fortran Command Line Argument Parser) is a modern Fortran library 
for parsing command-line arguments, inspired by Python's ``argparse`` module.
It provides a clean, intuitive API for defining and parsing command-line 
interfaces in Fortran applications.

Key Features
------------

* **Familiar API**: Designed to mirror Python's argparse for easy adoption
* **Type Safety**: Full support for integer, real, logical, and string argument types
* **Flexible Arguments**: Positional arguments, optional flags, and multiple value support
* **Auto-generated Help**: Automatic ``-h/--help`` output with formatted usage information
* **Subcommands**: Support for nested subparsers (like ``git commit``, ``git push``)
* **Argument Groups**: Organize arguments logically in help output
* **Mutually Exclusive Groups**: Ensure only one of a set of options is used
* **Modern Fortran**: Written in Fortran 2008+ with object-oriented design

Quick Example
-------------

.. code-block:: fortran

   program myapp
       use fclap, only: ArgumentParser, Namespace
       
       type(ArgumentParser) :: parser
       type(Namespace) :: args
       character(len=256) :: filename
       logical :: verbose
       
       ! Initialize the parser
       call parser%init(prog="myapp", description="Process a file")
       
       ! Add arguments
       call parser%add_argument("filename", help="Input file to process")
       call parser%add_argument("-v", "--verbose", action="store_true", &
                                help="Enable verbose output")
       
       ! Parse command line
       args = parser%parse_args()
       
       ! Use the results
       call args%get_string("filename", filename)
       call args%get_logical("verbose", verbose)
       
       print *, "Processing:", trim(filename)
       if (verbose) print *, "Verbose mode enabled"
   end program myapp

.. toctree::
   :maxdepth: 2
   :caption: Contents:

   tutorial
   api/fclap
   documentation

Indices and tables
------------------

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
