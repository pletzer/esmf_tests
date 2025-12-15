# --------------------------------------
# User configuration
# --------------------------------------
FC      = mpif90
FFLAGS  = -O2 -Wall -Wextra
LDFLAGS =

TARGET  = coupled_driver

SRC     = component_mod.f90 \
          coupled_driver.f90

OBJ     = $(SRC:.f90=.o)

# --------------------------------------
# Default target
# --------------------------------------
all: $(TARGET)

# --------------------------------------
# Link
# --------------------------------------
$(TARGET): $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ) $(LDFLAGS)

# --------------------------------------
# Compile
# --------------------------------------
%.o: %.F90
	$(FC) $(FFLAGS) -c $<

# --------------------------------------
# Cleanup
# --------------------------------------
clean:
	rm -f $(OBJ) $(TARGET) *.mod

.PHONY: all clean

