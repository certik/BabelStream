#!/bin/bash

module load gcc/11.2.0

export OMP_NUM_THREADS=64
export OMP_PROC_BIND=close
export OMP_PLACES=cores

M=1024
for compiler in gcc ; do
    AFFCONTROL="numactl -m 0,1,2,3 -C `seq -s "," 0 63`"
    for implementation in OpenMP OpenMPTaskloop OpenACC OpenACCArray DoConcurrent Array OpenMPWorkshare ; do
        if [ -f BabelStream.${compiler}.${implementation} ] ; then
            echo BabelStream.${compiler}.${implementation} OMP_NUM_THREADS=64
            time $AFFCONTROL \
            ./BabelStream.${compiler}.${implementation} -s $((1024*1024*${M}))
        fi
    done
done
