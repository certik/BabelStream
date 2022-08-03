FC	:= gfortran
FCFLAGS	:= -std=f2018 -Wall -O1

FCFLAGS += -DVERSION_STRING="4.0"


FCFLAGS += -DIMPLEMENTATION_STRING="DoConcurrent"
IMPLEMENTATION_OBJECT = DoConcurrentStream.o

BabelStream: main.F90 $(IMPLEMENTATION_OBJECT)
	$(FC) $(FCFLAGS) $^ -o $@

DoConcurrentStream.o: DoConcurrentStream.F90 BabelStreamTypes.mod
	$(FC) $(FCFLAGS) -c $<

BabelStreamTypes.mod: BabelStreamTypes.F90
	$(FC) $(FCFLAGS) -c $<

clean:
	-rm -f BabelStream
	-rm -f main.o
	-rm -f DoConcurrentStream.o DoConcurrentStream.mod
	-rm -f BabelStreamTypes.o   BabelStreamTypes.mod

