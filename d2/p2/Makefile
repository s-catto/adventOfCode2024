CFLAGS = -g -fbacktrace

objs = p2.o safe.o
mods = safe.mod

all: p2

p2: $(objs)
	gfortran $(CFLAGS) p2.o safe.o -o p2 

p2.o: p2.f90 safe.o
	gfortran $(CFLAGS) -c p2.f90 
	
safe.o: ../p1/safe.f90
	gfortran $(CFLAGS) -c ../p1/safe.f90 
	
clean:
	rm -f $(objs) $(mods)
	
purge: clean
	rm -f p1 
