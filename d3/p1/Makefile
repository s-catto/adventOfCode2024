CFLAGS = -g -fbacktrace

objs = p1.o

all: p1

p1: $(objs)
	gfortran $(CFLAGS) $(objs) -o p1 

p1.o: p1.f90 safe.o
	gfortran $(CFLAGS) -c p1.f90 
	
clean:
	rm -f $(objs)
	
purge: clean
	rm -f p1 
