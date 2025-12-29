CFLAGS = -g -fbacktrace

objs = p1.o safe.o
mods = safe.mod

all: p1

p1: $(objs)
	gfortran $(CFLAGS) p1.o safe.o -o p1 

p1.o: p1.f90 safe.o
	gfortran $(CFLAGS) -c p1.f90 
	
safe.o: safe.f90
	gfortran $(CFLAGS) -c safe.f90 
	
clean:
	rm -f $(objs) $(mods)
	
purge: clean
	rm -f p1 
