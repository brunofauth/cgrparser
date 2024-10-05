from __future__ import annotations
import fnmatch
import os
import shutil
import sys
import typing

if typing.TYPE_CHECKING:
    from collections.abc import Sequence

DIRS_TO_DEL = ("__pycache__",)
FILES_TO_DEL = ("lextab.py", "yacctab.py", "parser.out", "ast.pickle")


def cleanup(
    root: str,
    targetdirs: Sequence[str] = DIRS_TO_DEL,
    targetfiles: Sequence[str] = FILES_TO_DEL,
) -> None:
    for fp, dirs, files in os.walk(root, topdown=True, followlinks=False):
        dirs_to_skip = []
        for i, dirname in enumerate(dirs):
            if any(fnmatch.fnmatch(dirname, pattern) for pattern in targetdirs):
                target = os.path.join(fp, dirname)
                shutil.rmtree(target)
                dirs_to_skip.append(i)
                print(f"rm '{target}/'", file=sys.stderr)
        for dir_index in reversed(dirs_to_skip):
            del dirs[i]
        for file in files:
            if any(fnmatch.fnmatch(file, pattern) for pattern in targetfiles):
                target = os.path.join(fp, file)
                os.remove(target)
                print(f"rm {target!r}", file=sys.stderr)


if __name__ == "__main__":
    cleanup('..')
