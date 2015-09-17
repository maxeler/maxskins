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

def check_target(target):
    """
    Checks if target languages is supported.
    """
    if target == 'Python' or target == 'py':
        return 'py'

    elif target == 'C++' or target == 'cpp':
        return 'cpp'

    elif target == 'Java' or target == 'java':
        return 'java'

    elif target == 'Ruby' or target == 'rb':
        return 'rb'

    elif target == 'Go' or target == 'go':
        return 'go'

    elif target == 'C#' or target == 'csharp':
        return 'csharp'

    elif target == 'Haskell' or target == 'hs':
        return 'hs'

    else:
        return None
