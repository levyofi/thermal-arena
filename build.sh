#!/bin/bash

ifort -O3 -xhost  -warn noalignments -check bounds -c -o"ran_mod.o" "./ran_mod.f90"
ifort -O3 -xhost -warn noalignments -check bounds -c -o"Object_module.o" "./Object_module.f90"
ifort -O3 -xhost  -warn noalignments -check bounds -c -o"List_module.o" "./List_module.f90"
ifort -O3 -xhost  -warn noalignments -check bounds -c -o"Factory_module.o" "./Factory_module.f90"
ifort -O3 -xhost  -warn noalignments -check bounds -c -o"Grid_module.o" "./Grid_module.f90"
ifort -O3 -xhost  -warn noalignments -check bounds -c -o"Observer_module.o" "./Observer_module.f90"
ifort -O3 -xhost  -warn noalignments -check bounds -c -o"Lizard_abstract_module.o" "./Lizard_abstract_module.f90"
ifort -O3 -xhost  -warn noalignments -check bounds -c -o"Lizard_abstract_module_energy.o" "./Lizard_abstract_module_energy.f90"
ifort -O3 -xhost  -warn noalignments -check bounds -c -o"Lizard_module.o" "./Lizard_module.f90"
ifort -O3 -xhost  -warn noalignments -check bounds -c -o"main.o" "./main.f90"

ifort  -O3 -xhost -check bounds -o"lizards.cpu"  ./Factory_module.o ./Grid_module.o ./List_module.o ./Lizard_module.o ./Lizard_abstract_module.o ./Lizard_abstract_module_energy.o ./Object_module.o ./Observer_module.o ./main.o ./ran_mod.o


