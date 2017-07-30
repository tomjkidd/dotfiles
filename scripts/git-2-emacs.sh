#! /bin/sh

# Use default
PROJECT_ROOT=$(git rev-parse --show-toplevel)

EMACS_DIR=$(cd ~/.emacs.d; pwd)

if [ ! -e "${PROJECT_ROOT}/.emacs.d/init.el" ]; then
    "init.el file not detected. Terminating"
    exit 1
fi

# http://www.shelldorado.com/goodcoding/cmdargs.html
oflag=0
while [ $# -gt 0 ]
do
    case "$1" in
        -o) oflag=1;;
    esac
    shift
done

if [ -e "${EMACS_DIR}/init.el" ]; then
    if [ "$oflag" -eq 0 ]; then
        echo "${EMACS_DIR}/init.el already exists. Use -o to overwrite. Terminating"
        exit 1
    fi
fi

cp "${PROJECT_ROOT}/.emacs.d/init.el" "${EMACS_DIR}/init-test.el"
