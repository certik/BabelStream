#!/bin/bash

for compiler in oneapi ; do
    for implementation in DoConcurrent Array OpenMP OpenMPWorkshare OpenMPTarget OpenMPTargetLoop OpenMPTaskloop ; do
        make COMPILER=${compiler} IMPLEMENTATION=${implementation}
    done
done
#exit
for compiler in nvhpc ; do
    for implementation in DoConcurrent Array OpenMP OpenMPWorkshare OpenMPTarget OpenMPTargetLoop OpenMPTaskloop OpenACC OpenACCArray CUDA CUDAKernel ; do
        make COMPILER=${compiler} IMPLEMENTATION=${implementation}
    done
done
