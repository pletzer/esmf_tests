# User must specify ESMF_LIBDIR where esmf.mk resides

include $(ESMF_LIBDIR)/esmf.mk

all: test_esmf_field.x

OBJS := esmf_field_mod.o

# Compile rule
%.o: %.F90
	$(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) -c $< -o $@

test_esmf_field.o: test_esmf_field.F90 esmf_field_mod.o
	$(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) -c $< -o $@

# Link rule (ESMF provides link flags & libs)
test_esmf_field.x: test_esmf_field.o esmf_field_mod.o
	$(ESMF_F90COMPILER) $(ESMF_F90LINKOPTS) -o $@ $< $(OBJS) -L$(ESMF_LIBDIR) $(ESMF_F90ESMFLINKLIBS)

clean:
	rm -f *.o *.mod *.x