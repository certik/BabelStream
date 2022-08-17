#!/bin/bash

compiler=cray
for implementation in DoConcurrent Array OpenMP OpenMPWorkshare OpenMPTarget OpenMPTargetLoop OpenMPTaskloop OpenACC OpenACCArray  ; do
    make COMPILER=${compiler} IMPLEMENTATION=${implementation}
done
