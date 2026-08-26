fclap
=====

**fclap** is a Fortran 2018 command-line parser inspired by Python's
``argparse``. Its modular design separates parser definitions, actions,
validators, values, formatting, and parsing state while presenting one
supported application-facing module: ``fclap``.

The library supports typed positional and optional arguments, generated help,
symbolic and exact ``nargs``, argument and mutex groups, parent parsers, and
nested subcommands. Applications that cannot terminate during parsing can use
``parse_tokens`` or ``try_parse_args`` and inspect a structured
``ParseResult``.

Quick example
-------------

This is the complete basic example compiled and smoke-tested by fpm:

.. literalinclude:: ../../example/basic.f90
   :language: fortran
   :linenos:

Run it with, for example:

.. code-block:: console

   $ fclap-basic input.dat --threads 4 --verbose
   input=input.dat
   verbose=T
   threads=4

.. toctree::
   :maxdepth: 2
   :caption: User guide

   tutorial
   api/fclap
   migration
   documentation

Design references
-----------------

The frozen behavioral contract and compatibility matrix live in
``docs/design/api-contract.md`` and ``docs/design/feature-matrix.md`` in the
source distribution.

Indices and tables
------------------

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
