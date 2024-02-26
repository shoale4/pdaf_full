#!/bin/bash

export PDAF_ARCH=macos_gfortran_openmpi
export headfilepath="/Users/shoale/PDAF3/make.arch/macos_gfortran_openmpi.h"

######## GENERATE INIT FILES + NETCDF STATE FILE ########
if grep -q "\-DUSE_PDAF" $headfilepath; then
	sed -i '' 's/^CPP_DEFS = -DUSE_PDAF/CPP_DEFS = #-DUSE_PDAF/' $headfilepath
fi
make clean
make model
./model -spinup 0 
./model -spinup 1 


######## GENERATE ENSEMBLE AND OBSERVATIONS ########
cd tools
make clean
gfortran -c -o parser_no_mpi.o parser_no_mpi.F90
gfortran -c -o output_bin.o output_bin.F90
make all
./gen_ens -ens_size 20   # sets ensemble size to 4
./gen_obs -obs_choice 1 -obs_spacing 6  # uses uniform observations (choice 1) at every 6th grid point


# ######## RUN ASSIMILATION ########
if grep -q "#-DUSE_PDAF" $headfilepath; then
	sed -i '' 's/^CPP_DEFS = #-DUSE_PDAF/CPP_DEFS = -DUSE_PDAF/' $headfilepath
fi
cd ..
make clean
make model_pdaf

# test run using ensemble size 4, estkf filter (filter option 6), uniform observations every 4 grid points
mpirun -np 4 ./model_pdaf -dim_ens 4 -filtertype 6 -filter_type estkf -exp_type obs_spacing_exp -obs_type uniform6

# test run using ensemble size 4, etkf filter (filter option 6), uniform observations every 4 grid points
mpirun -np 4 ./model_pdaf -dim_ens 4 -filtertype 4 -filter_type etkf -exp_type obs_spacing_exp -obs_type uniform6

# test run using ensemble size 4, enkf filter (filter option 2), uniform observations every 4 grid points
mpirun -np 4 ./model_pdaf -dim_ens 4 -filtertype 2 -filter_type enkf -exp_type obs_spacing_exp -obs_type uniform6

# test run using ensemble size 4, seik filter (filter option 1), uniform observations every 4 grid points
mpirun -np 4 ./model_pdaf -dim_ens 4 -filtertype 1 -filter_type seik -exp_type obs_spacing_exp -obs_type uniform6
