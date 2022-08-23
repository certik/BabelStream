#!/bin/bash

for compiler in arm gcc nvhpc ; do
    for implementation in DoConcurrent Array OpenMP OpenMPTaskloop OpenMPWorkshare OpenACC OpenACCArray ; do
        make COMPILER=${compiler} IMPLEMENTATION=${implementation}
    done
done
