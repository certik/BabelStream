ifeq ($(COMPILER),nvhpc)
    include make.inc.nvhpc
else ifeq ($(COMPILER),oneapi)
    include make.inc.oneapi
else ifeq ($(COMPILER),gcc)
    include make.inc.gcc
else
    $(info Set COMPILER={nvhpc,oneapi}. Default is gcc.)
    include make.inc.gcc
endif

FCFLAGS += -DVERSION_STRING="4.0"

ifeq ($(IMPLEMENTATION),DoConcurrent)
    FCFLAGS += -DUSE_DOCONCURRENT $(DOCONCURRENT_FLAG)
    IMPLEMENTATION_OBJECT = DoConcurrentStream.o
else ifeq ($(IMPLEMENTATION),Array)
    FCFLAGS += -DUSE_ARRAY $(ARRAY_FLAG)
    IMPLEMENTATION_OBJECT = ArrayStream.o
else ifeq ($(IMPLEMENTATION),OpenMP)
    FCFLAGS += -DUSE_OPENMP $(OPENMP_FLAG)
    IMPLEMENTATION_OBJECT = OpenMPStream.o
else ifeq ($(IMPLEMENTATION),OpenMPTaskloop)
    FCFLAGS += -DUSE_OPENMPTASKLOOP $(OPENMP_FLAG)
    IMPLEMENTATION_OBJECT = OpenMPTaskloopStream.o
else ifeq ($(IMPLEMENTATION),OpenACC)
    FCFLAGS += -DUSE_OPENACC $(OPENACC_FLAG)
    IMPLEMENTATION_OBJECT = OpenACCStream.o
else ifeq ($(IMPLEMENTATION),OpenACCArray)
    FCFLAGS += -DUSE_OPENACCARRAY $(OPENACC_FLAG)
    IMPLEMENTATION_OBJECT = OpenACCArrayStream.o
else ifeq ($(IMPLEMENTATION),Sequential)
    FCFLAGS += -DUSE_SEQUENTIAL $(SEQUENTIAL_FLAG)
    IMPLEMENTATION_OBJECT = SequentialStream.o
else
    $(info Set IMPLEMENTATION={DoConcurrent,Array,OpenMP,OpenMPTaskloop,OpenACC,OpenACCArray}. Default is Sequential.)
    FCFLAGS += -DUSE_SEQUENTIAL $(SEQUENTIAL_FLAG)
    IMPLEMENTATION_OBJECT = SequentialStream.o
endif

BabelStream: main.F90 $(IMPLEMENTATION_OBJECT)
	$(FC) $(FCFLAGS) $^ BabelStreamTypes.o -o $@

BabelStreamTypes.o BabelStreamTypes.mod: BabelStreamTypes.F90
	$(FC) $(FCFLAGS) -c $<

%.o: %.F90 BabelStreamTypes.mod
	$(FC) $(FCFLAGS) -c $<

clean:
	-rm -f BabelStream
	-rm -f main.o BabelStreamUtil.mod babelstreamutil.mod
	-rm -f BabelStreamTypes.o BabelStreamTypes.mod babelstreamtypes.mod
	-rm -f DoConcurrentStream.o DoConcurrentStream.mod doconcurrentstream.mod
	-rm -f ArrayStream.o ArrayStream.mod arraystream.mod
	-rm -f SequentialStream.o SequentialStream.mod sequentialstream.mod
	-rm -f OpenMPStream.o OpenMPStream.mod openmpstream.mod
	-rm -f OpenMPTaskloopStream.o OpenMPTaskloopStream.mod openmpstream.mod
	-rm -f OpenACCStream.o OpenACCStream.mod openaccstream.mod
	-rm -f OpenACCArrayStream.o OpenACCArrayStream.mod openaccarraystream.mod

