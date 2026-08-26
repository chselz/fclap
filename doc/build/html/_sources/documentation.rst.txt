Documentation
=============

This section describes how to build the documentation

.. code-block:: bash

    mamba create -n fclap_docs python -y
    mamba activate flacp_docs

Make sure to be in the docs directory and continue with:

.. code-block:: bash

    pip install -r requirements.txt
    make html

The build files for the documentation can be found in the docs/build folder