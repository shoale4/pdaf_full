
# PDAF (Parallel Data Assimilation Framework) modified by Shoale Badr for Cardiac Research

Copyright 2004-2023, Lars Nerger, Alfred Wegener Institute, Helmholtz Center
for Polar and Marine Research, Bremerhaven, Germany. 
For license information, please see the file LICENSE.txt.

For full documentation and tutorial, see: http://pdaf.awi.de 


## How to Run

1. Clone this repo by running `git clone https://github.com/shoale4/pdaf_full/master`
2. Ensure you have `cmake`, `gcc`, and `MPI` installed on your machine.
3. Navigate to the header file in the `make.arch/` directory that suits your machine's needs.
4. Update the `MPI_INC` variable with the include path to your `MPI` header file.
5. Navigate to the `tutorial/corr/` directory and find `testrun_macos.sh`.
6. Update the `headfilepath` variable with the path to your chosen header file (from `make.arch/`).
7. Open a terminal and navigate to the `tutorial/corr/` directory.
8. Run `chmod +x testrun_macos.sh`.
9. Run `./testrun_macos.sh'.

