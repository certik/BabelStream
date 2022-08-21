#!/bin/bash

module load gnu/11.2.0
module load acfl/22.0.2

for compiler in nvhpc arm gcc ; do
    for implementation in DoConcurrent Array OpenMP OpenMPWorkshare OpenMPTarget OpenMPTargetLoop OpenMPTaskloop OpenACC OpenACCArray  ; do
        make COMPILER=${compiler} IMPLEMENTATION=${implementation}
    done
done
