#!/usr/bin/env bash
set -eux

source ./scripts/install_regtest_dependencies.sh
make check-end2end
