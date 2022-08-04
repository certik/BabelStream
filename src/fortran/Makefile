ifeq ($(COMPILER),nvhpc)
    include make.inc.nvhpc
else ifeq ($(COMPILER),oneapi)
    include make.inc.oneapi
else
    $(info Set COMPILER={nvhpc,oneapi}. Default is gcc.)
    include make.inc.gcc
endif

FCFLAGS += -DVERSION_STRING="4.0"

FCFLAGS += -DUSE_DOCONCURRENT $(DOCONCURRENT_FLAG)
IMPLEMENTATION_OBJECT = DoConcurrentStream.o

#FCFLAGS += -DUSE_ARRAY $(ARRAY_FLAG)
#IMPLEMENTATION_OBJECT = ArrayStream.o

#FCFLAGS += -DUSE_OPENACC $(OPENACC_FLAG)
#IMPLEMENTATION_OBJECT = OpenACCStream.o

# default
#FCFLAGS += -DUSE_SEQUENTIAL $(SEQUENTIAL_FLAG)
#IMPLEMENTATION_OBJECT = SequentialStream.o

BabelStream: main.F90 $(IMPLEMENTATION_OBJECT)
	$(FC) $(FCFLAGS) $^ BabelStreamTypes.o -o $@

DoConcurrentStream.o: DoConcurrentStream.F90 BabelStreamTypes.mod
	$(FC) $(FCFLAGS) -c $<

ArrayStream.o: ArrayStream.F90 BabelStreamTypes.mod
	$(FC) $(FCFLAGS) -c $<

SequentialStream.o: SequentialStream.F90 BabelStreamTypes.mod
	$(FC) $(FCFLAGS) -c $<

OpenACCStream.o: OpenACCStream.F90 BabelStreamTypes.mod
	$(FC) $(FCFLAGS) -c $<

BabelStreamTypes.o BabelStreamTypes.mod: BabelStreamTypes.F90
	$(FC) $(FCFLAGS) -c $<

clean:
	-rm -f BabelStream
	-rm -f main.o BabelStreamUtil.mod
	-rm -f BabelStreamTypes.o   BabelStreamTypes.mod
	-rm -f DoConcurrentStream.o DoConcurrentStream.mod
	-rm -f ArrayStream.o ArrayStream.mod
	-rm -f SequentialStream.o SequentialStream.mod
	-rm -f OpenACCStream.o OpenACCStream.mod

