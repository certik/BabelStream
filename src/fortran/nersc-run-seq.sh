#!/bin/bash

cat ./nersc-run.sh

#export OMP_NUM_THREADS=64
export OMP_PROC_BIND=close
export OMP_PLACES=cores

for compiler in nvhpc gcc cray ; do
    M=128
    export OMP_NUM_THREADS=1
    AFFCONTROL="numactl -m 0 -C 0"
    for implementation in DoConcurrent Array OpenMP OpenMPTaskloop OpenMPWorkshare OpenACC OpenACCArray ; do
        if [ -f BabelStream.${compiler}.${implementation} ] ; then
            echo BabelStream.${compiler}.${implementation} OMP_NUM_THREADS=${OMP_NUM_THREADS}
            time $AFFCONTROL \
            ./BabelStream.${compiler}.${implementation} -s $((1024*1024*${M}))
        fi
    done
done
