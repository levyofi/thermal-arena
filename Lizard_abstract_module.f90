! Author: Ofir Levy, levyofi@gmail.com
! This module implements the abstract lizard in the simulation, with preferred temperatures, and ability to move across the grid (the energetic and movement rules are implements in modules that extent this module)
module Lizard_abstract_module
    use List_module
    use Object_module
    use Observer_module
    use Factory_module
    use List_module
    use ran_mod
    use Grid_module 
    
    implicit none
    public

    type, public, extends(object),abstract :: Lizard_abstract
        
        !max, min and preferred temperatures, Basson and Clusella-Trullas, Physiol Biochem Zool. 2015 Jul-Aug;88(4):384-94. doi: 10.1086/682010.
        real(kind=4) :: Vtmax = 34.1    
        real(kind=4) :: Vtmin = 30.3   
        real(kind=4) :: Vtmean = 32.6 
        
        real(kind=4) :: ew = 0, ep = 0 !energy consumption for a resting (ew) and active (ep) lizards
        integer(kind=4) :: hours_of_model = 0
        real(kind=4) :: To !operative temperature
        class(Lizard_Patch), pointer :: l_patch 
        logical :: behavioral_thermoregulation
    contains
        procedure, pass(self) :: step => step_lizard_abs 
        procedure :: add_to_patch => add_lizard_to_patch
        procedure :: move_to_patch => move_lizard_to_patch
        procedure :: calculate_tb
        procedure (create_lizard_abs), pass(self), deferred :: create 
        
      end type Lizard_abstract

    type, public, extends(factory), abstract:: Lizard_factory_abstract
    contains
        procedure(create_lizard_factory_abs), nopass, deferred :: create 
        procedure(step_lizard_factory_abs), nopass, deferred :: step  
        procedure(clear_data_lizard_factory_abs), pass(self), deferred :: clear 
        procedure, pass(self) :: print_statistics => print_statistics_lizard_factory_abs
        procedure (init_lizard_factory_abstract), nopass, deferred :: init
    end type Lizard_factory_abstract

    real, parameter :: num_of_seconds_for_substep = 30.

contains

    subroutine init_lizard_factory_abstract()

    end subroutine

    !this subroutine creates the lizard factory
    subroutine create_lizard_factory_abs(m_object)
        class(object), pointer :: m_object       
    end subroutine
    
    !this subroutine adds the lizard to the grid
    subroutine add_lizard_to_patch(self, x, y)
        class(Lizard_abstract) :: self
        integer :: x, y

        self%x = x
        self%y = y
        self%l_patch => m_observer%m_grid%patches(x,y)
    end subroutine add_lizard_to_patch

    !this subroutine moves the lizard to a new patch on the grid
    subroutine move_lizard_to_patch(self, x, y)
        class(Lizard_abstract) :: self
        integer :: x, y
 
        call self%add_to_patch(x,y)
    end subroutine move_lizard_to_patch
    
    !this subroutine creates the lizard
    subroutine create_lizard_abs(self)
        class(Lizard_abstract) :: self
        character(len=30) :: use_thermoregulation
        
        call get_command_argument(6, use_thermoregulation)
        if (use_thermoregulation=="with_thermoregulation") then
          self%behavioral_thermoregulation=.true.
        else
          self%behavioral_thermoregulation=.false.
        end if
        
        self%To = self%Vtmean
        call self%calculate_tb()
    end subroutine
    
    !this subroutine is called every time step 
    subroutine step_lizard_abs(self)
        class(Lizard_abstract) :: self

        self%hours_of_model = self%hours_of_model + 1
        call self%calculate_tb()
    end subroutine step_lizard_abs

   !this subroutine assigns the operative temperature of the lizard based in the current location of the lizard in the grid
   subroutine calculate_tb(self)
        class(Lizard_abstract) :: self
        self%To = self%l_patch%temperature
    end subroutine calculate_tb

    !this subroutine is called every time step by the factory. Should be implemented in extending modules
    subroutine step_lizard_factory_abs(m_list)
        class(list) :: m_list        

    end subroutine step_lizard_factory_abs
    
    !this subroutine is called by the factory to clear the factory at the end of the simulation. Should be implemented in extending modules
    subroutine clear_data_lizard_factory_abs(self)
        class(Lizard_factory_abstract) :: self    
    end subroutine clear_data_lizard_factory_abs

    !this subroutine is called by the factory to print summary statistics at the end of the simulation. Should be implemented in extending modules
    subroutine print_statistics_lizard_factory_abs(self)
        class(Lizard_factory_abstract) :: self
    end subroutine print_statistics_lizard_factory_abs
         
end module Lizard_abstract_module
