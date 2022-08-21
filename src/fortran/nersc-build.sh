#!/bin/bash

#module swap PrgEnv-cray PrgEnv-nvhpc
#module load nvhpc/22.7
for compiler in nvhpc gcc cray amd ; do
    if [ "x$compiler" == "xgcc" ] ; then
        module load gcc/11.2.0
    fi
    if [ "x$compiler" == "xnvhpc" ] ; then
        module unload nvidia
        module load nvhpc/22.7
    fi
    if [ "x$compiler" == "xcray" ] ; then
        module load PrgEnv-cray/8.3.3
        module load craype-accel-host
    fi
    for implementation in DoConcurrent Array OpenMP OpenMPWorkshare OpenMPTarget OpenMPTargetLoop OpenMPTaskloop OpenACC OpenACCArray CUDA CUDAKernel ; do
        make COMPILER=${compiler} IMPLEMENTATION=${implementation}
    done
done
