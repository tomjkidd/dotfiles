#! /bin/sh
PROJECT_ROOT=$(git rev-parse --show-toplevel)
${PROJECT_ROOT}/scripts/sync-emacs.sh -v -o
