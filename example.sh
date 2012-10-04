#!/bin/bash

#SBATCH --time         1:00        # keyword with regular argument
#SBATCH --dependency   singleton
#SBATCH --job-name     "Job name"  # keyword with quoted argument
#SBATCH --output       my file     # unproperly quoted argument
#SBATCH --exclusive                # keyword without argument
#SBATCH --ntaks        120         # misspelled keyword
##SBATCH --nodes        10          # directive commented out

# Regular comment

mpirun a.out




