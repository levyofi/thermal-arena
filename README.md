# Thermal Arena Simulation
A Fortran simulation of an arena, where temperatures can change spatially every hour. This code was used to mimic a real experiment, where lizards where located in closed arenas, and their behavior was recorded to study their behavioral thermoregulation under different conditions. The details of the simulation and thermoregulation behaviours are described in [Basson et. al. 2017](http://onlinelibrary.wiley.com/doi/10.1111/1365-2435.12795/full).

### The design of the simulation
The simulation is written in Fortran 90, and was designed as an object oriented structure. The objects in the simulation are divided to different types (similar to classes on C++ and python), implemented in several files. All code files have the .f90 extension. I implemented the Observer design pattern in the code, in which an “Observer” overseas the simulation by controlling the thermoregulatory arena (“Grid” in the code) and the lizard. I also implemented a Factory design pattern to create the lizard in the simulation. The relationships between the different types of objects are in the diagram ![picture]https://user-images.githubusercontent.com/22244790/27008686-8cee0692-4e2d-11e7-9f58-c5b7e7f967cd.png
 
### Compiling the code 
I provide a BASH script (build.sh) that compiles the Fortran files and creates an execution file, “lizards.cpu”. 

### Running the simulation – 
I provide a BASH script (run_simulation.sh) with commands to run the simulations in our study, as well as few examples of how to run the simulations under other environmental conditions. 

### Plotting the results
Once the simulation is done. The text output files may be read and analysed. Here, I provide a R script that read the files and create comparison plots. 
