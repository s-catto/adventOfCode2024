CFLAGS = -g -fbacktrace -Wall

objs = p2.o modelos.o
mods = modelos.mod

all: p2

p2: $(objs)
	gfortran $(CFLAGS) $(objs) -o p2

p2.o: p2.f90 modelos.o
	gfortran $(CFLAGS) -c p2.f90 

modelos.o: modelos.f90
	gfortran $(CFLAGS) -c modelos.f90
	
clean:
	rm -f $(objs) $(mods)
	
purge: clean
	rm -f p1 
