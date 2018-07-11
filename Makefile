FORT = gfortran
CFLAGS = -c

all : main

main: main.f90 Vector.o
	$(FORT) main.f90 Vector.o -o main

Vector.o: Vector.f90
	$(FORT) $(CFLAGS) Vector.f90

clean:
	rm *.o *.mod main
