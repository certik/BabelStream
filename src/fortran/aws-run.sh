#!/bin/bash

export OMP_PROC_BIND=close
export OMP_PLACES=cores

M=1024
export OMP_NUM_THREADS=64
export ACC_NUM_CORES=${OMP_NUM_THREADS}
for compiler in nvhpc arm gcc ; do
    for implementation in DoConcurrent Array OpenMP OpenMPWorkshare OpenMPTarget OpenMPTargetLoop OpenMPTaskloop OpenACC OpenACCArray  ; do
        if [ -f BabelStream.${compiler}.${implementation} ] ; then
        echo BabelStream.${compiler}.${implementation} OMP_NUM_THREADS=${OMP_NUM_THREADS}
        ./BabelStream.${compiler}.${implementation} -s $((1024*1024*${M}))
        fi
    done
done

M=128
export OMP_NUM_THREADS=1
export ACC_NUM_CORES=${OMP_NUM_THREADS}
for compiler in nvhpc arm gcc ; do
    for implementation in DoConcurrent Array OpenMP OpenMPWorkshare OpenMPTarget OpenMPTargetLoop OpenMPTaskloop OpenACC OpenACCArray  ; do
        if [ -f BabelStream.${compiler}.${implementation} ] ; then
        echo BabelStream.${compiler}.${implementation} OMP_NUM_THREADS=${OMP_NUM_THREADS}
        ./BabelStream.${compiler}.${implementation} -s $((1024*1024*${M}))
        fi
    done
done
