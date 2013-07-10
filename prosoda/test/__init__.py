from contextlib import contextmanager
import sys

@contextmanager
def hide_stderr():
    '''
    Context manager to temporarily hide stderr
    '''
    old_stderr = sys.stderr
    class Redirector(object):
        def write(self, *args):
            pass
    sys.stderr = Redirector()
    yield
    sys.stderr = old_stderr

@contextmanager
def hide_output():
    '''
    Context manager to temporarily hide stderr and stdout
    '''
    old_stderr = sys.stderr
    old_stdout = sys.stdout
    class Redirector(object):
        def write(self, *args):
            pass
    sys.stderr = Redirector()
    sys.stdout = Redirector()
    yield
    sys.stderr = old_stderr
    sys.stdout = old_stdout
