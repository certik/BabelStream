#!/bin/bash

#for compiler in gcc oneapi nvhpc ; do
for compiler in nvhpc ; do
    # CPU
    #for implementation in DoConcurrent Array OpenMP OpenMPWorkshare OpenMPTaskloop OpenACC OpenACCArray ; do
    # GPU
    for implementation in DoConcurrent OpenMPTarget OpenMPTargetLoop OpenACC OpenACCArray CUDA CUDAKernel ; do
        if [ -f BabelStream.${compiler}.${implementation} ] ; then
            ./BabelStream.${compiler}.${implementation} -s $((1024*1024*1024))
        fi
    done
done
