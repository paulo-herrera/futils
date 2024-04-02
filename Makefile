FC=gfortran
FFLAGS=

all: tests

tests: test_fdict test_fprop

test_fprop: test_fprop.f90 fprop.o fdict.o
	$(FC) $(FFLAGS) -c test_fprop.f90
	$(FC) -o tests/test_fprop test_fprop.o fprop.o fdict.o
	
test_fdict: test_fdict.f90 fdict.o
	$(FC) $(FFLAGS) -c test_fdict.f90
	$(FC) -o tests/test_fdict test_fdict.o fdict.o
	
fdict.o: fdict.f90
	$(FC) $(FFLAGS) -c fdict.f90

fprop.o: fprop.f90 fdict.o
	$(FC) $(FFLAGS) -c fprop.f90		
	
.PHONY: clean run
clean: 
	rm -f *.o
	rm -f *.mod

cleanall: clean
	rm -f tests/test_fdict
	rm -f tests/test_fprop

run: 
	cd tests && ./test_fdict
	cd tests && ./test_fprop
