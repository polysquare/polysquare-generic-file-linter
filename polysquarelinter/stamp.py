# /polysquarelinter/stamp.py
#
# Wrap jobstamps using environment variable to enable and disable
# caching.
#
# See /LICENCE.md for Copyright information
"""Wrap jobstamps using environment variable to disable caching."""

import os

from jobstamps import jobstamp


def stamp(func, *args, **kwargs):
    """Use jobstamps to cache the result of func, if not disabled."""
    if os.environ.get("_POLYSQUARE_GENERIC_FILE_LINTER_NO_STAMPING", None):
        kwargs = {k: v for k, v in kwargs.items()
                  if not k.startswith("jobstamps_")}
        return func(*args, **kwargs)
    else:
        return jobstamp.run(func, *args, **kwargs)
