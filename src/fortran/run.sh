#!/bin/bash

M=64

compiler=cray
# CPU
for implementation in DoConcurrent Array OpenMP OpenMPWorkshare OpenMPTaskloop ; do
    if [ -f BabelStream.${compiler}.${implementation} ] ; then
        ./BabelStream.${compiler}.${implementation} -s $((1024*1024*${M}))
    fi
done
exit
# GPU
for implementation in OpenMPTarget OpenMPTargetLoop OpenACC OpenACCArray ; do
    if [ -f BabelStream.${compiler}.${implementation} ] ; then
        ./BabelStream.${compiler}.${implementation} -s $((1024*1024*${M}))
    fi
done
