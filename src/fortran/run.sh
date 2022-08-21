#!/bin/bash

cat ./run.sh

M=1024
AFFCONTROL="numactl -N 0 -m 0 -C `seq -s "," 0 31`"
export OMP_NUM_THREADS=32
export OMP_PROC_BIND=close
export OMP_PLACES=threads

for compiler in oneapi gcc nvhpc ; do
    for implementation in OpenMP OpenMPTaskloop OpenMPWorkshare DoConcurrent Array OpenACC OpenACCArray CUDA CUDAKernel ; do
        if [ -f BabelStream.${compiler}.${implementation} ] ; then
            echo "BabelStream.${compiler}.${implementation}"
            $AFFCONTROL \
            ./BabelStream.${compiler}.${implementation} -s $((1024*1024*${M}))
        fi
    done
done
