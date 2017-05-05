# particleTracking makefile
#
FC=ifort
F90=ifort
CFLAGS= -O0 -traceback -g 
CFLAGS90= -O0 -traceback -g -fopenmp
OPENMP=-fopenmp
LIBS=/sw/local/netcdf-4.3.0/lib/
BIN=particleTracking
INCLUDES=/sw/local/netcdf-fortran-4.2/include/
#
FILES=\
utilities.f90\
ptmModule.f90\
allocateArrays.f90\
get_closest_point.f90\
initialize.f90\
advectionDiffusion2D.f90\
advectionDiffusion3D.f90\
computeConcentrations.f90\
readUV2D.f90\
readUV3D.f90\
updateParticleLevel.f90\
updateParticleLocation.f90\
fate.f90\
writeOutput2D.f90\
writeOutput3D.f90\
readAtmosphere.f90\
readOceanModelInput.f90\
readOcean2D.f90\
readOcean3D.f90\
createParticles.f90\
readNetcdfGrid.f90\
particleTracking.f90\
#
OBJECTS=$(FILES:.f=.o)
OBJECTS.=$(FILES:.f90=.o) 
#
.SUFFIXES: .o .f .F
#
all: $(BIN)
#
$(BIN): $(OBJECTS) 
	$(FC) $(OPENMP) -o $(BIN) $(OBJECTS) -I$(INCLUDES) -L$(LIBS) -lnetcdf -lnetcdff
	@echo
	@echo "************ particleTracking is now up to date!"
	@echo
#
clean:
	rm *.o *.mod $(BIN)
#
.f.o:
	$(FC) -c $(CFLAGS) $*.f
.F.o:
	$(FC) -c $(CFLAGS) $*.F
.f90.o:
	$(F90) -c $(CFLAGS90) $*.f90
