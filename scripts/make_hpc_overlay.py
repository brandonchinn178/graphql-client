#!/usr/bin/env python

import os
import re
import subprocess
import tempfile
from collections import namedtuple

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

## Overlay definition

Module = namedtuple('Module', ['package', 'name', 'overlays'])
TickFunction = namedtuple('TickFunction', ['name', 'line_num', 'pattern'])

OVERLAY_CONTENTS = [
    Module('graphql-client', 'Data.GraphQL.Monad', [
        # GraphQLManager fields used by RecordWildCards: https://gitlab.haskell.org/ghc/ghc/issues/17834
        TickFunction('manager', 96, r'manager :: Manager'),
        TickFunction('baseReq', 97, r'baseReq :: Request'),
        # Instance for MonadUnliftIO for transformer stacks built on top of QueryT
        TickFunction('withRunInIO', 156, r'withRunInIO inner = GraphQLQueryT \$'),
        # GraphQLSettings fields used by RecordWildCards: https://gitlab.haskell.org/ghc/ghc/issues/17834
        TickFunction('managerSettings', 66, r'managerSettings :: ManagerSettings'),
        TickFunction('url', 68, r'url\s+:: String'),
        TickFunction('modifyReq', 69, r'modifyReq\s+:: Request -> Request'),
    ]),
    Module('graphql-client', 'Data.GraphQL.Monad.Class', [
        # MonadQuery instances for standard monad transformers
        TickFunction('runQuerySafe', line_num, r'lift \. runQuerySafe')
        for line_num in [62, 65, 68, 71, 74, 77, 80, 83, 86, 89]
    ]),
]

## Main

PackageInfo = namedtuple('PackageInfo', ['hash', 'srcdir'])

def main():
    hpc_dir = get_hpc_dir()

    packages = {
        'graphql-client': PackageInfo(
            get_graphql_client_hash(hpc_dir),
            '{}/graphql-client/src'.format(ROOT)
        ),
    }

    header('Generating draft file')

    overlay_file = '\n'.join(
        generate_overlay_module(packages[module.package], module)
        for module in OVERLAY_CONTENTS
    )

    print(overlay_file)

    header('Generating overlay tix file')

    with tempfile.NamedTemporaryFile() as f:
        f.write(overlay_file)
        f.seek(0)
        overlay_tix = subprocess.check_output([
            'stack',
            'exec',
            '--',
            'hpc',
            'overlay',
            '--srcdir=./graphql-client',
            '--hpcdir={}'.format(hpc_dir),
            f.name,
        ])

    open('overlay.tix', 'w').write(overlay_tix)

def get_hpc_dir():
    dist_dir = subprocess.check_output(['stack', 'path', '--dist-dir']).strip()
    return '{}/hpc'.format(dist_dir)

def get_graphql_client_hash(hpc_dir):
    graphql_client_hpc_dir = '{}/graphql-client/{}'.format(ROOT, hpc_dir)
    entries = os.listdir(graphql_client_hpc_dir)

    for entry in entries:
        entry_path = '{}/{}'.format(graphql_client_hpc_dir, entry)
        if os.path.isdir(entry_path) and re.match(r'graphql-client-[\d.]+-\w+', entry):
            return entry

    raise Exception(
        'Could not find hash for graphql-client. Searched "{}", found: {}'.format(
            graphql_client_hpc_dir,
            entries
        )
    )

def generate_overlay_module(package, module):
    qualified_module = '{}/{}'.format(package.hash, module.name)

    overlays = '\n'.join(
        '  {}'.format(generate_overlay(package, module, overlay))
        for overlay in module.overlays
    )

    return 'module "{}" {{\n{}\n}}'.format(qualified_module, overlays)

def generate_overlay(package, module, overlay):
    if isinstance(overlay, TickFunction):
        return generate_overlay_tick_function(package, module, overlay)
    else:
        raise ValueError('Inalid overlay: {}'.format(overlay))

def generate_overlay_tick_function(package, module, tick_function):
    line_num = tick_function.line_num
    pattern = tick_function.pattern

    filepath = '{}/{}.hs'.format(package.srcdir, module.name.replace('.', '/'))
    lines = open(filepath).readlines()
    line = lines[line_num - 1]

    # check that the line matches the regex
    if re.search(pattern, line) is None:
        raise Exception('Line {} in file "{}" did not match regex: `{}`'.format(line_num, filepath, pattern))

    return 'tick function "{}" on line {};'.format(tick_function.name, line_num)

## Helpers

def header(msg):
    print('\n===== {} =====\n'.format(msg))

if __name__ == '__main__':
    main()
