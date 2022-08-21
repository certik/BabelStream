#!/bin/bash

cat ./nersc-run.sh

#export OMP_NUM_THREADS=64
export OMP_PROC_BIND=close
export OMP_PLACES=cores

for compiler in nvhpc gcc cray ; do
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
    M=1024
    export OMP_NUM_THREADS=64
    AFFCONTROL="numactl -m 0,1,2,3 -C `seq -s "," 0 $((${OMP_NUM_THREADS}-1))`"
    for implementation in OpenMP OpenMPTaskloop OpenACC OpenACCArray DoConcurrent Array OpenMPWorkshare ; do
        if [ -f BabelStream.${compiler}.${implementation} ] ; then
            echo BabelStream.${compiler}.${implementation} OMP_NUM_THREADS=64
            time $AFFCONTROL \
            ./BabelStream.${compiler}.${implementation} -s $((1024*1024*${M}))
        fi
    done
done
