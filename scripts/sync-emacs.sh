#! /bin/sh

# Use default
PROJECT_ROOT=$(git rev-parse --show-toplevel)
PROJECT_FILE=${PROJECT_ROOT}/.emacs.d/init.el

EMACS_DIR=$(cd ~/.emacs.d; pwd)
EMACS_FILE=${EMACS_DIR}/init.el

# http://www.shelldorado.com/goodcoding/cmdargs.html
oflag=0
rflag=0
vflag=0
while [ $# -gt 0 ]
do
    case "$1" in
        -o) oflag=1;;
        -r) rflag=1;;
        -v) vflag=1;;
    esac
    shift
done

# Default to `pull`, replace local with repo
SRC_FILE=$PROJECT_FILE
DEST_FILE=$EMACS_FILE

if [ "$rflag" -eq 1 ]; then
    SRC_FILE=$EMACS_FILE
    DEST_FILE=$PROJECT_FILE
fi

if [ ! -e "${SRC_FILE}" ]; then
    "${SRC_FILE} file not detected. Terminating"
    exit 1
fi

if [ -e "${DEST_FILE}" ]; then
    if [ "$oflag" -eq 0 ]; then
        echo "${DEST_FILE} already exists. Use -o to overwrite. Terminating"
        exit 1
    fi
fi

if [ "$vflag" -eq 1 ]; then
    echo "Writing ${SRC_FILE} to ${DEST_FILE}"
fi

cp ${SRC_FILE} ${DEST_FILE}
