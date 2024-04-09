FC=gfortran
FFLAGS=-cpp -ffree-line-length-512 #-DWARNING_OFF -DASSERT_OFF -DVERBOSE_OFF -DDEBUG_OFF

all: tests

tests: test_fdict test_fprop test_fmacros test_ftext

test_fprop: test_fprop.f90 fprop.o fdict.o
	$(FC) $(FFLAGS) -c test_fprop.f90
	$(FC) -o tests/test_fprop test_fprop.o fprop.o fdict.o ftext.o

test_fdict: test_fdict.f90 fdict.o
	$(FC) $(FFLAGS) -c test_fdict.f90
	$(FC) -o tests/test_fdict test_fdict.o fdict.o

test_ftext: test_ftext.f90 ftext.o
	$(FC) $(FFLAGS) -c test_ftext.f90
	$(FC) -o tests/test_ftext test_ftext.o ftext.o

test_fmacros: test_fmacros.f90 fmacros.fpp
	$(FC) $(FFLAGS) -o tests/test_fmacros test_fmacros.f90

fdict.o: fdict.f90
	$(FC) $(FFLAGS) -c fdict.f90

fprop.o: fprop.f90 fdict.o ftext.o
	$(FC) $(FFLAGS) -c fprop.f90

ftext.o: ftext.f90 fmacros.fpp
	$(FC) $(FFLAGS) -c ftext.f90

.PHONY: clean run
clean: 
	rm -f *.o
	rm -f *.mod

cleanall: clean
	rm -f tests/test_fmacros
	rm -f tests/test_ftext
	rm -f tests/test_fdict
	rm -f tests/test_fprop

run: 
	cd tests && ./test_fmacros
	cd tests && ./test_ftext
	cd tests && ./test_fdict
	cd tests && ./test_fprop

