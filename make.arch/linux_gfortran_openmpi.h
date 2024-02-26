######################################################
# Include file with machine-specific definitions     #
# for building PDAF.                                 #
#                                                    #
# Variant for Linux with gfortran and OpenMPI        #
#                                                    #
# In the case of compilation without MPI, a dummy    #
# implementation of MPI, like provided in the        #
# directory nullmpi/ has to be linked when building  #
# an executable.                                     #
######################################################
# $Id: linux_gfortran_openmpi.h 1565 2015-02-28 17:04:41Z lnerger $


# Compiler, Linker, and Archiver
FC = mpif90
LD = $(FC)
AR = ar
RANLIB = ranlib

# C preprocessor
# (only required, if preprocessing is not performed via the compiler)
CPP = /usr/bin/cpp

# Definitions for CPP
# Define USE_PDAF to include PDAF
# Define BLOCKING_MPI_EXCHANGE to use blocking MPI commands to exchange data between model and PDAF
# Define PDAF_NO_UPDATE to deactivate the analysis step of the filter
# (if the compiler does not support get_command_argument()
# from Fortran 2003 you should define F77 here.)
CPP_DEFS = -DUSE_PDAF

# Optimization specs for compiler
# To use OpenMP parallelization in PDAF, specify it here (-fopenmp (gfortran) or -openmp (ifort))
#   (You should explicitly define double precision for floating point
#   variables in the compilation)  
OPT = -O3 -fdefault-real-8 -fopenmp

# Optimization specifications for Linker
OPT_LNK = $(OPT)

# Linking libraries (BLAS, LAPACK, if required: MPI)
#LINK_LIBS =-L/usr/lib -llapack  -lblas   -lm
LINK_LIBS = -L/usr/local/pace-apps/spack/packages/linux-rhel7-x86_64/gcc-4.8.5/openblas-0.3.18-nk2wzjljuj6r3fjwje7ofkmxhqer7gdq/lib -lopenblas -L/usr/local/pace-apps/spack/packages/linux-rhel7-x86_64/gcc-10.3.0/netlib-scalapack-2.2.0-2rv5fvduzxa5idnrbzlwbkoalz6dtvea/lib -lscalapack -lm

# Specifications for the archiver
AR_SPEC = 

# Specifications for ranlib
RAN_SPEC =

# Include path for MPI header file
MPI_INC = -I/usr/local/pace-apps/manual/packages/openmpi/4.1.4/gcc-10.3.0/include -pthread

# Object for nullMPI - if compiled without MPI library
OBJ_MPI =

# NetCDF (only required for Lorenz96)
NC_LIB   = 
NC_INC   = 
