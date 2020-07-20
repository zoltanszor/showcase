# Compiler

FC = gfortran
CXX = g++-8
FFLAGS = -fdefault-real-8 -fdefault-double-8
#-O0 -ffixed-line-length-none -g -fbacktrace -fbounds-check -ffpe-trap=invalid,zero,overflow,underflow,denormal
CXXFLAGS = -O

# Executable
EXEC = cluster 

#Fastjet
FJDIR = fjcore-3.3.3
FJCORE = $(FJDIR)/fjcore.cc $(FJDIR)/fjcorefortran.cc

#ALLSOURCE = main.f $(PSLIMIT)
ALLSOURCE = rambo.f main.f90 $(FJCORE)

# Objects

OBJ = $(addsuffix .o, $(basename $(ALLSOURCE)))

all: $(OBJ)
	$(FC) -g -lstdc++ -o $(EXEC) $(OBJ)

clean:
	rm -f $(EXEC) $(OBJ)

%.o: %.f
	$(FC) $(FFLAGS) -c -o $@ $^

%.o: %.f90
	$(FC) $(FFLAGS) -c -o $@ $^

%.o : %.cc 
	$(CXX) $(CXXFLAGS) -c -o $@ $^
