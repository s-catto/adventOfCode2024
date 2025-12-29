CFLAGS = -g -fbacktrace

objs = p1.o ord.o
mods = ord.mod

all: p1

p1: $(objs)
	gfortran $(CFLAGS) p1.o ord.o -o p1 

p1.o: p1.f90 ord.o
	gfortran $(CFLAGS) -c p1.f90 
	
ord.o: ord.f90
	gfortran $(CFLAGS) -c ord.f90 
	
clean:
	rm -f $(objs) $(mods)
	
purge: clean
	rm -f p1 
