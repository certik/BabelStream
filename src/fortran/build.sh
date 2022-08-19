#!/bin/bash

for compiler in oneapi gcc nvhpc ; do
    for implementation in DoConcurrent Array OpenMP OpenMPWorkshare OpenMPTarget OpenMPTargetLoop OpenMPTaskloop OpenACC OpenACCArray CUDA CUDAKernel ; do
        make COMPILER=${compiler} IMPLEMENTATION=${implementation}
    done
done
