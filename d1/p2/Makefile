CFLAGS = -g -fbacktrace

objs = p2.o ord.o
mods = ord.mod

all: p2

p2: $(objs)
	gfortran $(CFLAGS) p2.o ord.o -o p2 

p2.o: p2.f90 ord.o
	gfortran $(CFLAGS) -c p2.f90 
	
ord.o: ../p1/ord.f90
	gfortran $(CFLAGS) -c ../p1/ord.f90 
	
clean:
	rm -f $(objs) $(mods)
	
purge: clean
	rm -f p1 
