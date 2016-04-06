"""
Common functions and constants.
"""
import os
import contextlib

VERSION = '0.1.3'
COMMON_DIR = os.path.dirname(os.path.realpath(__file__))
TEMPLATE_DIR = os.path.join(os.path.dirname(os.path.realpath(__file__)),
                            'templates')

@contextlib.contextmanager
def change_working_directory(new_path):
    """
    Change working directory to be new_path.

    Input:
        new_path -- new path
    """
    saved_path = os.getcwd()
    os.chdir(new_path)
    yield
    os.chdir(saved_path)

def get_file_extension(path):
    """
    Get extension of file.

    Input:
        path -- path to file

    Output:
        File extension.
    """
    return os.path.splitext(path)[1]

def get_file_name(path):
    """
    Get name of file.

    Input:
        path -- path to file

    Output:
        File name.
    """
    return os.path.splitext(os.path.basename(path))[0]
