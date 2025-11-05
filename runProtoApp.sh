#!/bin/bash
#PBS -lselect=1:ncpus=1:mpiprocs=1:mem=8gb
#PBS -lwalltime=00:10:00

cd $HOME/PhD/nuopc-app-prototypes/AtmOcnPetListProto

module load ESMF/8.7.0-foss-2024a

mpirun -np 1 ./esmApp