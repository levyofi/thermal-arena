! Author: Ofir Levy, levyofi@gmail.com
! This module implements the energetoc component of a lizard in the simulation
module Lizard_abstract_energy_module
    use List_module
    use Lizard_abstract_module

    implicit none
    public

    type, public, extends(Lizard_abstract),abstract :: Lizard_abstract_energy

        real(kind=4) :: e_lost = 0

    contains
        procedure :: step => step_lizard_abs_energy        
        procedure :: calculate_metabolic_rate
    end type Lizard_abstract_energy

contains
    
    !this subroutine is called every time step in the simulation and calculates the energy consumption of the lizard
    subroutine step_lizard_abs_energy(self)
        class(Lizard_abstract_energy) :: self

        call step_lizard_abs(self)
        self%e_lost = 0
        call self%calculate_metabolic_rate()
    end subroutine step_lizard_abs_energy

    !this subroutine calculates the resting (ew) and activity (ep) energy consumption of the lizard
    subroutine calculate_metabolic_rate(self)
        class(Lizard_abstract_energy) :: self
        real mlO2
        
        mlO2 = exp(-2.6791+0.0479*self%To)/3600.*30. ! see Fig S4a
        self%ew =  mlO2 * 20. ! assuming 20 J/mlO2 (Congdon et al. 1979; Gessaman & Nagy 1988)
        mlO2 = exp(-1.7679+0.0148*self%To)/3600.*30. ! see Fig S4b
        self%ep = mlO2 * 20. !assuming 20 J/mlO2 (Congdon et al. 1979; Gessaman & Nagy 1988)
    end subroutine calculate_metabolic_rate

end module Lizard_abstract_energy_module
