! Author: Ofir Levy, levyofi@gmail.com
! This module implements the Observer as part of the Observer design pattern. The role of the observer is to hold all the components in a simulation, run, initialize and finalize the simulations.
module Observer_module
    use Object_module
    use List_module
    use Grid_module
    use Factory_module
    implicit none
    private

   public :: initialize
   public :: observer_finalize
   public :: m_observer

    type, public :: observer
        type (list) :: l_individuals !a list of lizards
        type (grid) :: m_grid !the grid
        class (factory), pointer :: m_lizard_factory !a lizard factory that interacts with the lizards in the simulations
        integer :: nsims_to_run 
    contains
        procedure :: populate
        procedure :: step => observer_step

    end type observer

    type (observer), pointer :: m_observer => null()

 contains

    !this subroutine creates the observer and the grid
    subroutine initialize()
       if (.not. associated(m_observer)) then
          allocate(m_observer)
          call m_observer%m_grid%grid_initialize()
       end if
    end subroutine initialize
    
    !this subroutine finishes a simulation
    subroutine observer_finalize()
       if (associated(m_observer)) then
          call m_observer%m_lizard_factory%print_statistics()
          call m_observer%m_lizard_factory%clear()
          deallocate(m_observer)
       end if
    end subroutine observer_finalize
    
    !this subroutine creates a lizard in the simulation
    subroutine populate(self)
        class (observer) :: self
        class (object), pointer :: new_lizard
        
        call self%m_lizard_factory%create(new_lizard)
        call new_lizard%create()
    end subroutine populate

   !this subroutine runs every hour of the simulation
    subroutine observer_step(self)
        class (observer) :: self
        call self%m_grid%change_lights()   
        call self%m_lizard_factory%step(self%l_individuals)
    end subroutine observer_step


end module Observer_module
