
# PDAF (Parallel Data Assimilation Framework) modified by Shoale Badr for Cardiac Research

Copyright 2004-2023, Lars Nerger, Alfred Wegener Institute, Helmholtz Center
for Polar and Marine Research, Bremerhaven, Germany. 
For license information, please see the file LICENSE.txt.

For full documentation and tutorial, see: http://pdaf.awi.de 


## How to Run

1. Clone this repo.
2. Ensure you have `make`, `gfortran/ifort`, `MPI`, `BLAS` and `LAPACK` installed on your machine.
3. Navigate to the header file in the `make.arch/` directory that suits your machine's needs.
4. Update the `MPI_INC` variable with the include path to your `MPI` header file.
5. Navigate to the `tutorial/corr/` directory.
6. Create a `txt_file_data/` directory.
7. Now, find `testrun_macos.sh`.
8. Update the `headfilepath` variable with the path to your chosen header file (from `make.arch/`).
9. Open a terminal and navigate to the `tutorial/corr/` directory.
10. Run `chmod +x testrun_macos.sh`.
11. Run `./testrun_macos.sh`.

