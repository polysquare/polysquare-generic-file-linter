# /polysquarelinter/__init__.py
#
# Entry point for linter.
#
# See /LICENCE.md for Copyright information
"""Load the polysquarelinter package."""

import os

# Hack to work around a bug in pkg_resources.
#
# We need to override the __file__ attribute of polysquarelinter.__file__
# with its absolute path, as opposed to its relative path. This fixes an
# issue where running tests in the source directory and then changing
# directory mid-test will cause pkg_resources to fail, since it relies on
# __file__ to be a valid path (which ceases to be valid as soon as we
# exit the source directory).
if not os.path.isabs(__file__):
    __file__ = os.path.abspath(__file__)
