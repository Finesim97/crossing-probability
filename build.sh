#!/usr/bin/env bash
set -ex

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
ENVNAME=crossing
ENVFILE="$SCRIPT_DIR/env.yml"

eval "$(conda shell.bash hook)"
MANAGER="conda"

if ! [[ -z "${CONDA_CACHE}" ]]; then
    mkdir -p "${CONDA_CACHE}/pkg" "${CONDA_CACHE}/envs"
    conda config --add pkgs_dirs "${CONDA_CACHE}/pkg"
fi


if [ -x "$(command -v mamba)" ]; then
    MANAGER="mamba"
fi

if  conda env list | grep "${ENVNAME}"; then
#    $MANAGER env update -f $ENVFILE -n $ENVNAME --prune
    echo "Already exists..."
else
    $MANAGER env create -f $ENVFILE -n $ENVNAME  
fi

conda activate $ENVNAME
make
make python
python setup.py install
