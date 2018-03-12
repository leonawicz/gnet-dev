#!/bin/bash
#SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --account=snap
#SBATCH -p main
#SBATCH --ntasks=32
####SBATCH --nodelist=atlas01,atlas02
#SBATCH --job-name=GeoNet2
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32

# One CL variable passed take form, e.g., ID=1, see below for hardcoded R arguments.
# mpirun -np 1 Rscript /workspace/UA/mfleonawicz/leonawicz/GlobalNetworks/code/cities_sequences_batch_combine.R
mpirun -np 1 Rscript $1

