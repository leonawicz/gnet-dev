#!/bin/bash
#SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --account=snap
#SBATCH -p main
#SBATCH --ntasks=32
####SBATCH --nodelist=atlas01,atlas02
#SBATCH --job-name=GeoNet
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32

# Two CL variables passed take form, e.g., file.R ID=1, see below for hardcoded R arguments.
# mpirun -np 1 Rscript /workspace/UA/mfleonawicz/leonawicz/GlobalNetworks/code/cities_sequences_batch.R ID=1 &
# mpirun -np 1 Rscript /workspace/UA/mfleonawicz/leonawicz/GlobalNetworks/code/cities_sequences_batch.R $1
mpirun -np 1 Rscript $1 $2
