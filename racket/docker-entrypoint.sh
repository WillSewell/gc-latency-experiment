#!/bin/bash

set -euo pipefail
cd "$(dirname "$0")"/../..

export PLT_INCREMENTAL_GC=1
exec "$@"
