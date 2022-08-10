#!/bin/bash

export KMP_HW_PLACES=1s,4c,1t
export KMP_HW_SUBSET=1s,4c,1t
export KMP_AFFINITY=compact,granularity=fine #,verbose

# CPU
for compiler in oneapi ; do
    for implementation in DoConcurrent Array OpenMP OpenMPWorkshare OpenMPTaskloop ; do
        if [ -f BabelStream.${compiler}.${implementation} ] ; then
            ./BabelStream.${compiler}.${implementation} -s $((1024*1024*64))
        fi
    done
done
# GPU
for compiler in nvhpc ; do
    for implementation in DoConcurrent OpenMPTarget OpenMPTargetLoop OpenACC OpenACCArray CUDA CUDAKernel ; do
        if [ -f BabelStream.${compiler}.${implementation} ] ; then
            ./BabelStream.${compiler}.${implementation} -s $((1024*1024*64))
        fi
    done
done
