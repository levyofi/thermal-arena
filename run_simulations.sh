#!/bin/bash

#the arguments that the program gets are: 1) the number of light bulbs that are always on. 
#                                         2) the name of the output file
#                                         3) the thermoregulation behavior of the lizard (can_move - thermoregulating, no_move - thermoconforming)
#                                         4) number of simulations to run
#                                         5) optional - the temperature of the grid in the simulation. Required and useful only when running with no light bulbs on. 

#the simulations presented in the paper:

#thermoregulating - high quality
./lizards.cpu 4 arena4with_thermo.txt can_move 10000

#thermoregulating - low quality
./lizards.cpu 1 arena1with_thermo.txt can_move 10000

#thermoconforming - high quality
./lizards.cpu 4 arena4no_thermo.txt no_move 10000

#thermoconforming - low quality
./lizards.cpu 1 arena1no_thermo.txt no_move 10000

echo finished

#the model can also run with constant temperature and no light bulbs:
#e.g., running a simulation where the temperature at all locations is 15C
./lizards.cpu 0 arenaLQ_homo_with_thermo.txt can_move 10000 15.
#e.g., running a simulation where the temperature at all locations is 30C
./lizards.cpu 0 arenaHQ_homo_with_thermo.txt can_move 10000 30.

