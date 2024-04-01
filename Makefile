FC=gfortran
FFLAGS=

all: tests

tests: test_fdict

test_fdict: test_fdict.f90 fdict.o
	$(FC) $(FFLAGS) -c test_fdict.f90
	$(FC) -o tests/test_fdict test_fdict.o fdict.o
	
fdict.o: fdict.f90
	$(FC) $(FFLAGS) -c fdict.f90
	
	
.PHONY: clean
clean: 
	rm -f *.o
	rm -f *.mod

cleanall: clean
	rm -f tests/*
