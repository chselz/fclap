# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

import os
import sys

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'fclap'
copyright = '2026, fclap contributors'
author = 'fclap contributors'
release = '0.1.0'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = [
    'sphinxfortran.fortran_domain',
    'myst_parser',
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
exclude_patterns = []

# The suffix(es) of source filenames.
source_suffix = {
    '.rst': 'restructuredtext',
    '.md': 'markdown',
}

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'sphinx_rtd_theme'
html_static_path = ['_static']

# -- Sphinx-Fortran configuration --------------------------------------------
# Path to Fortran source files (relative to conf.py location)
fortran_src = [
    '../../src'
]

# Fortran source file extensions
fortran_ext = ['f90', 'F90', 'f95', 'F95']

# -- MyST Parser configuration -----------------------------------------------
# Enable auto-generated header anchors
myst_heading_anchors = 3

# Enable additional MyST extensions
myst_enable_extensions = [
    'colon_fence',
    'deflist',
]
