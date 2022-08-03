FC	:= gfortran
FCFLAGS	:= -std=f2018 -Wall -O1

FCFLAGS += -DVERSION_STRING="4.0"


#FCFLAGS += -DUSE_DOCONCURRENT
#IMPLEMENTATION_OBJECT = DoConcurrentStream.o

#FCFLAGS += -DUSE_ARRAY
#IMPLEMENTATION_OBJECT = ArrayStream.o

FCFLAGS += -fopenacc -DUSE_OPENACC
IMPLEMENTATION_OBJECT = OpenACCStream.o

#FCFLAGS += -DUSE_SEQUENTIAL # default
#IMPLEMENTATION_OBJECT = SequentialStream.o

BabelStream: main.F90 $(IMPLEMENTATION_OBJECT)
	$(FC) $(FCFLAGS) $^ -o $@

DoConcurrentStream.o: DoConcurrentStream.F90 BabelStreamTypes.mod
	$(FC) $(FCFLAGS) -c $<

ArrayStream.o: ArrayStream.F90 BabelStreamTypes.mod
	$(FC) $(FCFLAGS) -c $<

SequentialStream.o: SequentialStream.F90 BabelStreamTypes.mod
	$(FC) $(FCFLAGS) -c $<

OpenACCStream.o: OpenACCStream.F90 BabelStreamTypes.mod
	$(FC) $(FCFLAGS) -c $<

BabelStreamTypes.mod: BabelStreamTypes.F90
	$(FC) $(FCFLAGS) -c $<

clean:
	-rm -f BabelStream
	-rm -f main.o BabelStreamUtil.mod
	-rm -f BabelStreamTypes.o   BabelStreamTypes.mod
	-rm -f DoConcurrentStream.o DoConcurrentStream.mod
	-rm -f ArrayStream.o ArrayStream.mod
	-rm -f SequentialStream.o SequentialStream.mod
	-rm -f OpenACCStream.o OpenACCStream.mod

