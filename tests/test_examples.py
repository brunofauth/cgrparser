from __future__ import annotations

import contextlib as cl
import importlib.util
import os
import subprocess as sp
import sys
import time
import typing
import unittest

from .test_util import cpp_supported

if typing.TYPE_CHECKING:
    from collections.abc import Iterable, Iterator
    from types import ModuleType


# https://docs.python.org/3/library/importlib.html#importing-a-source-file-directly
def import_from_path(module_name: str, file_path: str) -> ModuleType:
    spec = importlib.util.spec_from_file_location(module_name, file_path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    spec.loader.exec_module(module)
    return module


@cl.contextmanager
def temp_attr(obj: Any, attr_name: str, value: Any) -> Iterator[None]:
    old_value = getattr(obj, attr_name)
    setattr(obj, attr_name, value)
    try:
        yield
    finally:
        setattr(obj, attr_name, old_value)


def iter_examples(examples_dir: str = 'examples') -> Iterable[os.DirEntry]:
    yield from (e for e in os.scandir(examples_dir) if e.is_file() and e.name.endswith('.py'))


class TestExamplesSucceed(unittest.TestCase):
    @unittest.skipUnless(cpp_supported(), 'cpp only works on Unix')
    def test_all_examples(self):
        examples_root = "examples"
        with open(os.devnull, "w") as dev_null, temp_attr(sys, 'argv', []):
            for file in iter_examples(examples_root):
                module_name = f"{examples_root}.{file.name[:-3]}"
                with self.subTest(name=file.name):
                    try:
                        module = import_from_path(module_name, file.path)
                        with cl.redirect_stdout(dev_null), cl.redirect_stderr(dev_null):
                            module.main()
                    except Exception as e:
                        raise AssertionError(f"Could not run example {file.name!r}") from e


if __name__ == '__main__':
    unittest.main()
