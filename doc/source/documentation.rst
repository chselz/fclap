Building the documentation
==========================

Install the Python dependencies and run Sphinx from the repository root:

.. code-block:: console

   $ python -m pip install -r doc/requirements.txt
   $ sphinx-build -W -b html doc/source doc/build/html

