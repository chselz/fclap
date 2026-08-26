Building the documentation
==========================

Install the Python dependencies and run Sphinx from the repository root:

.. code-block:: console

   $ python -m pip install -r docs/requirements.txt
   $ sphinx-build -W -b html docs/source docs/build/html

``-W`` promotes documentation warnings to errors and is also used by the
documentation CI job. The generated site is written to ``docs/build/html``.

The Fortran examples shown with ``literalinclude`` are the same files compiled
and smoke-tested by fpm. Public snippets should import symbols only from the
``fclap`` facade module.
