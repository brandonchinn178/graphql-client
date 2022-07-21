#!/usr/bin/env python3
# pyright: strict

from __future__ import annotations

import re
import subprocess
import tempfile
from pathlib import Path
from typing import NamedTuple

ROOT = Path(__file__).resolve().parent.parent

# ---------- Overlay definition ---------- #


class Module(NamedTuple):
    package: str
    name: str
    overlays: list[TickFunction]


class TickFunction(NamedTuple):
    name: str
    line_num: int
    pattern: str


OVERLAY_CONTENTS = [
    Module(
        package="graphql-client",
        name="Data.GraphQL.Monad",
        overlays=[
            # GraphQLManager fields used by RecordWildCards: https://gitlab.haskell.org/ghc/ghc/issues/17834
            TickFunction("manager", 96, r"manager :: Manager"),
            TickFunction("baseReq", 97, r"baseReq :: Request"),
            # Instance for MonadUnliftIO for transformer stacks built on top of QueryT
            TickFunction("withRunInIO", 156, r"withRunInIO inner = GraphQLQueryT \$"),
            # GraphQLSettings fields used by RecordWildCards: https://gitlab.haskell.org/ghc/ghc/issues/17834
            TickFunction("managerSettings", 66, r"managerSettings :: ManagerSettings"),
            TickFunction("url", 68, r"url\s+:: String"),
            TickFunction("modifyReq", 69, r"modifyReq\s+:: Request -> Request"),
        ],
    ),
    Module(
        package="graphql-client",
        name="Data.GraphQL.Monad.Class",
        overlays=[
            # MonadQuery instances for standard monad transformers
            TickFunction("runQuerySafe", line_num, r"lift \. runQuerySafe")
            for line_num in [62, 65, 68, 71, 74, 77, 80, 83, 86, 89]
        ],
    ),
]

# ---------- Main ---------- #


class PackageInfo(NamedTuple):
    hash: str
    srcdir: str


def main():
    hpc_dir = get_hpc_dir()

    packages = {
        "graphql-client": PackageInfo(
            hash=get_graphql_client_hash(hpc_dir),
            srcdir=f"{ROOT}/graphql-client/src",
        ),
    }

    header("Generating draft file")

    overlay_file = "\n".join(
        generate_overlay_module(packages[module.package], module)
        for module in OVERLAY_CONTENTS
    )

    print(overlay_file)

    header("Generating overlay tix file")

    with tempfile.TemporaryDirectory() as tmpdir:
        f = Path(tmpdir) / "overlay"
        f.write_text(overlay_file)
        overlay_tix = subprocess.run(
            [
                "stack",
                "exec",
                "--",
                "hpc",
                "overlay",
                "--srcdir=./graphql-client",
                f"--hpcdir={hpc_dir}",
                f,
            ],
            check=True,
            capture_output=True,
            encoding="utf-8",
        )

    Path("overlay.tix").write_text(overlay_tix.stdout)


def get_hpc_dir() -> Path:
    dist_dir = subprocess.run(
        ["stack", "path", "--dist-dir"],
        check=True,
        capture_output=True,
        encoding="utf-8",
    )
    return Path(dist_dir.stdout.strip()) / "hpc"


def get_graphql_client_hash(hpc_dir: Path) -> str:
    graphql_client_hpc_dir = ROOT / "graphql-client" / hpc_dir
    for entry in graphql_client_hpc_dir.rglob("graphql-client-*-*"):
        if entry.is_dir() and re.match(r"graphql-client-[\d.]+-\w+", entry.name):
            return entry.name

    raise Exception(
        f"Could not find hash for graphql-client in {graphql_client_hpc_dir}"
    )


def generate_overlay_module(package: PackageInfo, module: Module) -> str:
    qualified_module = f"{package.hash}/{module.name}"

    overlays = "\n".join(
        "  {}".format(generate_overlay(package, module, overlay))
        for overlay in module.overlays
    )

    return f'module "{qualified_module}" {{\n{overlays}\n}}'


def generate_overlay(
    package: PackageInfo, module: Module, tick_function: TickFunction
) -> str:
    line_num = tick_function.line_num
    pattern = tick_function.pattern
    module_file = module.name.replace(".", "/") + ".hs"

    filepath = Path(package.srcdir) / module_file
    lines = filepath.read_text().splitlines()
    line = lines[line_num - 1]

    # check that the line matches the regex
    if re.search(pattern, line) is None:
        raise Exception(
            f'Line {line_num} in file "{filepath}" did not match regex: `{pattern}`'
        )

    return f'tick function "{tick_function.name}" on line {line_num};'


# ---------- Helpers ---------- #


def header(msg: str):
    print(f"\n===== {msg} =====\n")


if __name__ == "__main__":
    main()
