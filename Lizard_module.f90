! Author: Ofir Levy, levyofi@gmail.com
! This module implements the lizard in the simulation

module Lizard_module
    use List_module
    use Lizard_abstract_energy_module
    use Object_module
    use Observer_module
    use Factory_module    
    use ran_mod

    implicit none
    public

    type, public, extends(Lizard_abstract_energy) :: Lizard
            
        real(kind=4) :: e_net_lost        
        real(kind=4) :: moving_time
        real(kind=4) :: moving_distance
        real(kind=4) :: mean_tb = 0.  
        real(kind=4) :: nsteps = 0.
        logical :: is_moving = .false.
        logical :: can_move = .false.             

    contains
       procedure :: step => step_lizard
       procedure :: create => create_lizard
       procedure :: get_performance
    end type Lizard

    type, public, extends(Lizard_factory_abstract):: Lizard_factory
      integer :: n_sims = 0
    contains
        procedure, nopass :: create => create_lizard_factory
        procedure, nopass :: step => step_lizard_factory
        procedure, nopass :: init => init_lizard_factory
        procedure :: clear => clear_lizard_factory
        procedure :: print_statistics => print_statistics_lizard_factory
    end type Lizard_factory

    type (Lizard_factory), target :: m_lizard_factory    
    
contains
    
    !this subroutine initilizes the lizard factory in the observer
    subroutine init_lizard_factory()
        m_observer%m_lizard_factory => m_lizard_factory
    end subroutine init_lizard_factory

    !this subroutine allocate a new lizard object 
    subroutine create_lizard_factory(m_object)
        class(object), pointer :: m_object
        allocate(lizard::m_object)
    end subroutine create_lizard_factory
    
    !this subroutine creates a new lizard
    subroutine create_lizard(self)
        class(Lizard) :: self
        integer :: light_i, x, y
        character*500 :: outputfilename, str_can_move
        
        !add the lizard to the list of lizards
        call m_observer%l_individuals%add(self)
        !add the lizard to the grid
        call m_observer%m_grid%add_to_grid(self)
        !call abstract module creation subroutine
        call create_lizard_abs(self)
        !check input to see if the lizard is able to move for thermoregulation
        call get_command_argument(3, str_can_move)  
        if (str_can_move=="can_move") then
          self%can_move = .true.
        end if          
    end subroutine create_lizard
    
    !this subroutine is the "step" routine that is called every hour in the simulation 
    !At the beginning of each hour, the light bulbs (heat sources) in the simulation are switched on/off randomly.
    !During the hour, a thermoregulating lizard may change position on the grid based on its distance from its preferred temperature. 
    subroutine step_lizard(self)
        class(Lizard) :: self                
        real :: seconds, chance_of_movement        
        real :: r, delta, performance
        integer :: x,y,light_x,light_y
        character (len=6) :: str_seconds
        
        seconds = 0
        do seconds = 0, 3600, num_of_seconds_for_substep 
          call random_number(chance_of_movement)         
          call self%get_performance(performance)
          if (self%can_move .and. chance_of_movement<(.9-performance)) then ! a thermoregulating lizard will move if performance is lower that the chance of movement (+10% basal chance of movement)
            self%is_moving = .true.
            if (self%is_moving) then              
              self%moving_time = self%moving_time + 30
              x = self%x
              y=  self%y              
              call m_observer%m_grid%move_lizard_on_grid(self)
              self%moving_distance = self%moving_distance + sqrt((self%x - x)**2. + (self%y - y)**2.)*5.
            end if
          else
            self%is_moving = .false.
          end if
          
          !calculate energy consumption
          call step_lizard_abs_energy(self)          
          if (self%is_moving) then
            self%e_lost = self%ep
          else
            self%e_lost = self%ew            
          end if
          self%e_net_lost = self%e_net_lost + self%e_lost
          
          !calculate mean body temperature
          self%nsteps = self%nsteps + 1
          delta = self%To - self%mean_tb
          self%mean_tb = self%mean_tb + delta/real(self%nsteps)
        end do                
    end subroutine step_lizard    

    !this subroutine is the "step" routine of the lizard factory. It calls the step routine of each lizard in a list 
    subroutine step_lizard_factory(m_list)
        class(list) :: m_list !a list of lizards
        type(record), pointer :: cur_record, next_record
        class(object), pointer :: cur_lizard
        
        !in our simulation the list holds only one lizard. Hence, call the step routine of the first lizard in the list
        cur_record => m_list%head
        cur_lizard => cur_record%data
        select type(cur_lizard)
        type is (Lizard)
            call cur_lizard%step()                
        end select

    end subroutine step_lizard_factory

    !this subroutine writes the summary statistics of the lizard in the simulation to the output file
    subroutine print_statistics_lizard_factory(self)
        class(Lizard_factory) :: self
        integer :: julianday_i, iyear
        type(record), pointer :: log_record, linear_record
        class(object), pointer :: cur_lizard
        real :: delta
        
        !note that the simulation only has one lizard so no need to iterate through the list
        log_record => m_observer%l_individuals%head
        cur_lizard => log_record%data
        select type(cur_lizard)
        type is (lizard)
          write (3, '(4f12.1)') cur_lizard%moving_time, cur_lizard%moving_distance, cur_lizard%e_net_lost, cur_lizard%mean_tb
        end select
        self%n_sims = self%n_sims + 1
    end subroutine print_statistics_lizard_factory

    !this subroutine deletes all lizards in the factory
    subroutine clear_lizard_factory(self)
        class(Lizard_factory) :: self
        
        call m_observer%l_individuals%clear()
    end subroutine clear_lizard_factory

    !this subroutine calculates the relative performance of a lizard (a number from 0 to 1)
    subroutine get_performance(self,performance)
      class(Lizard) :: self    
      real, parameter :: pmax = 1.
      real :: beta, Topt, denominator, Tmin, Tmax
      real :: performance
      
      Tmin = self%Vtmin
      Tmax = self%Vtmax 
      Topt = self%Vtmean
      ! see equations 15-17 in Landsberg, J.J. (1977) Some useful equations for biological studies. Experimental Agriculture, 13, 273-286.
	    Beta = (Tmax-Topt)/(Topt-Tmin) 
    	denominator = (Topt-Tmin)*(Tmax-Topt)**Beta
    	performance = (pmax/denominator)*max((self%To-Tmin),0.)*max((Tmax-self%To),0.)**Beta 
    end subroutine get_performance
    
end module Lizard_module
