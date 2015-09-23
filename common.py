"""
Common functions.
"""
import os
import contextlib

@contextlib.contextmanager
def cd(new_path):
    """
    Python snippet to CD into a directory.
    """
    saved_path = os.getcwd()
    os.chdir(new_path)
    yield
    os.chdir(saved_path)
