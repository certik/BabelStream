#!/bin/bash

# uncomment to disable GPU targets
#HAS_GPU=0

COMPILERS="gcc"
if [ $(uname -m) != "arm64" ] ; then
    COMPILERS="${COMPILERS} nvhpc"
fi
if [ $(uname -m) == "aarch64" ] ; then
    COMPILERS="${COMPILERS} arm cray fj"
elif [ $(uname -m) == "x86_64" ] ; then
    COMPILERS="${COMPILERS} amd cray oneapi"
fi

for compiler in ${COMPILERS} ; do
    TARGETS="DoConcurrent Array OpenMP OpenMPTaskloop OpenMPWorkshare"
    if [ "${HAS_GPU}" != "0" ] ; then
        TARGETS="${TARGETS} OpenMPTarget OpenMPTargetLoop"
        if [ "x${compiler}" == "xnvhpc" ] ; then
            TARGETS="${TARGETS} CUDA CUDAKernel"
        fi
    fi
    if [ "x${compiler}" == "xnvhpc" ] || [ "x${compiler}" == "xgcc" ] || [ "x${compiler}" == "xcray" ] ; then
        TARGETS="${TARGETS} OpenACC OpenACCArray"
    fi
    for implementation in ${TARGETS} ; do
        make COMPILER=${compiler} IMPLEMENTATION=${implementation}
    done
done
