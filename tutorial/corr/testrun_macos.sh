#!/bin/bash

export PDAF_ARCH=macos_gfortran_openmpi
export makefilepath="/Users/shoale/PDAF3/make.arch/macos_gfortran_openmpi.h"

######## GENERATE INIT FILES + NETCDF STATE FILE ########
if grep -q "\-DUSE_PDAF" $makefilepath; then
	sed -i '' 's/^CPP_DEFS = -DUSE_PDAF/CPP_DEFS = #-DUSE_PDAF/' $makefilepath
fi
make clean
make model
./model -spinup 0 -fout_choice
./model -spinup 1 -fout_choice


######## GENERATE ENSEMBLE AND OBSERVATIONS ########
cd tools
make clean
gfortran -c -o parser_no_mpi.o parser_no_mpi.F90
gfortran -c -o output_txt.o output_txt.F90
make all
./gen_ens -ens_size 4   # sets ensemble size to 4
./gen_obs -obs_choice 1 -obs_spacing 6  # uses uniform observations (choice 1) at every 6th grid point


# ######## RUN ASSIMILATION ########
if grep -q "#-DUSE_PDAF" $makefilepath; then
	sed -i '' 's/^CPP_DEFS = #-DUSE_PDAF/CPP_DEFS = -DUSE_PDAF/' $makefilepath
fi
cd ..
make clean
make model_pdaf

# test run using ensemble size 4, estkf filter, uniform observations every 6 grid points
mpirun -np 4 ./model_pdaf -dim_ens 4 -exp_type obs_spacing_exp -filt_type 6 -filter_type estkf -obs_type uniform6