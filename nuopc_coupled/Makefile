# User paths
ESMF_DIR = /Users/apletzer/work/esmf/8.6/esmf-8.6.1
ESMF_LIBDIR = $(ESMF_DIR)/lib/libO/Darwin.gfortranclang.64.openmpi.default

include $(ESMF_LIBDIR)/esmf.mk

# The source files
SRCS = $(wildcard *.f90 *.F90)
OBJS = $(patsubst %.f90, %.o, $(patsubst %.F90, %.o, $(SRCS)))

# The executable (example)
PROGRAM = 2comp_time_example

all: $(PROGRAM)

# Compile rule (ESMF already defines $(ESMF_F90))
%.o: %.F90
	$(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) -c $< -o $@

%.o: %.f90
	$(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) -c $< -o $@

esmf.o: ocn.o atm.o


# Link rule (ESMF provides link flags & libs)
$(PROGRAM): $(OBJS) ocn.o atm.o
	$(ESMF_F90COMPILER) $(ESMF_F90LINKOPTS) -o $@ $(OBJS) -L$(ESMF_LIBDIR) $(ESMF_F90ESMFLINKLIBS)

clean:
	rm -f *.o *.mod *.x
