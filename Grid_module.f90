! Author: Ofir Levy, levyofi@gmail.com
! This module describes a grid of 36x36 cells, with up to 9 lights, and one lizard. 
! 

module Grid_module
    use List_module
    use Object_module
    use ran_mod
    implicit none

    !a light
    type, public :: Light_patch
        logical :: lighted !a light can be on or off
        integer :: x,y !the location of the light on the grid
        integer :: id !the id of the light
    end type Light_patch
    
    !a location on the grid that has a lizard
    type, public :: Lizard_Patch
        real :: temperature !the temperature of the lizard
        integer :: x,y !the location of the patch
    end type Lizard_Patch

    public :: size_of_grid
    integer, parameter :: size_of_grid = 36

    !the grid class
    type, public :: Grid
        type (Light_patch), dimension(3,3) :: light_patches !an array of 9 lights
        type (Lizard_patch), dimension(36,36) :: patches !an array of 36 locations
        real, dimension(36,36) :: distance_from_light !an array of values that gives the distance from the closest light for each patch
        integer :: nlights !the number of lights that are on in the grid
        real :: homog_temperature = 0. 
    contains
        procedure :: get_patch
        procedure :: grid_initialize
        procedure :: add_to_grid
        procedure :: move_lizard_on_grid
        procedure :: set_temperatures
        procedure :: change_lights
    end type Grid

contains
    
    !this subroutine adds a lizard object to a patch. 
    subroutine add_object(self, new_object)
        class(Lizard_Patch) :: self
        class(object) :: new_object

        call new_object%add_to_patch(self%x, self%y)
    end subroutine

    !this subroutine initializes a grid
    subroutine grid_initialize(self)
        class(Grid) :: self
        integer :: x, y, id
        character*5 :: input
        
        !locate the light evenly  throughout the grid
        id=9
        do x=1, 3
            do y=1, 3
                self%light_patches(x, y)%x = (60*x - 30)/5
                self%light_patches(x, y)%y = (60*y - 30)/5
                self%light_patches(x, y)%id = id
                id = id - 1
            end do
        end do
        
        !calculate the distance of each patch from the closest light
        do x=1, size_of_grid
            do y=1, size_of_grid
                self%patches(x, y)%x = x
                self%patches(x, y)%y = y
                self%distance_from_light(x,y) = sqrt((mod(x,12)-mod(self%light_patches(floor((x+11)/12.), floor((y+11)/12.))%x,12))**2. + (mod(y,12)-mod(self%light_patches(floor((x+11)/12.), floor((y+11)/12.))%y,12))**2.)
                !print *, floor((x+11)/12.), self%light_patches(floor((x+11)/12.), floor((y+11)/12.))%x
            end do
            !print '(36f5.1)' , self%distance_from_light(x,:)
        end do
        
        !get the number of lights that are on
        call get_command_argument(1, input)
        read (input, *) self%nlights
        
        !if no light are on, get the temperature of all patches
        if (self%nlights==0) then
          call get_command_argument(5, input)
          read (input, *) self%homog_temperature
        end if
        call self%change_lights()
    end subroutine
    
    !this function gets a location (x,y) and returns associated patch
    function get_patch(self, x, y) result(m_patch)
        class(Grid), target :: self
        integer :: x,y
        type (Lizard_patch), pointer :: m_patch
        m_patch => self%patches(x,y)
    end function get_patch

    !this subroutine gets a lizard and locates it on the grid
    subroutine add_to_grid(self, ilizard, ox, oy)
        real :: r
        integer, optional :: ox, oy !optional new location for the lizard
        integer :: x, y
        class(Grid) :: self
        class (object) :: ilizard !the lizard object
        
        !if values for ox and oy are included, put the lizard there
        if (present(ox)) then
            x=ox
            y=oy
        else ! otherwise, locate the lizard randomly on the grid
            call random_number(r)
            x = INT(r*(size_of_grid-1) + 1.5)
            call random_number(r)
            y = INT(r*(size_of_grid-1) + 1.5)
        end if
        call ilizard%add_to_patch(x,y)
    end subroutine add_to_grid

    !this subroutine gets a lizard and moves it to a new location on the grid
    subroutine move_lizard_on_grid(self, ilizard, new_x, new_y)
        class(Grid) :: self
        integer, optional :: new_x, new_y
        integer :: x, y
        real :: r
        class (object) :: ilizard

        !if values for new_x and new_y are included, put the lizard there
        if (present(new_x)) then
            x=new_x
            y=new_y
        else ! otherwise, locate the lizard randomly on the grid
            call random_number(r)
            x = INT(r*(size_of_grid-1) + 1.5)
            call random_number(r)
            y = INT(r*(size_of_grid-1) + 1.5)
        end if
        !print *, "moving to", x, y
        call ilizard%move_to_patch(x,y)
    end subroutine move_lizard_on_grid
    
    !this subroutine gets a grid and determines the temperature of each patch in the grid
    subroutine set_temperatures(self)
      class(Grid) :: self
      integer :: x, y, id, light_x, light_y
      
      !if no light is on, set the temperature of all patches to the homog_temperature variable  
      if (self%nlights==0) then
        self%patches%temperature = self%homog_temperature
        !print *, "constnat temperature", self%homog_temperature
      else !otherwise, check what is the distance of each patch from a light and set the temperature accordingly
        do x=1, size_of_grid
            do y=1, size_of_grid
                light_x = floor((x+11)/12.)
                light_y = floor((y+11)/12.)
                if (self%light_patches(light_x, light_y)%lighted) then !the closest light is on
                  !set the temperature according to the distance from the light
                  if (self%distance_from_light(x,y)<1.) then
                    self%patches(x, y)%temperature = 40.3
                  else if (self%distance_from_light(x,y)<2.) then
                    self%patches(x, y)%temperature = 36.3
                  else if (self%distance_from_light(x,y)<3.) then
                    self%patches(x, y)%temperature = 31.7
                  else if (self%distance_from_light(x,y)<4.) then
                    self%patches(x, y)%temperature = 28.6
                  else if (self%distance_from_light(x,y)<5.) then
                    self%patches(x, y)%temperature = 23.7
                  else if (self%distance_from_light(x,y)<6.) then
                    self%patches(x, y)%temperature = 20.3
                  else if (self%distance_from_light(x,y)<7.) then
                    self%patches(x, y)%temperature = 19.3
                  else
                    self%patches(x, y)%temperature = 16.2
                  end if
                else !the closest light is off, set the temperature to the lowest temperature 
                  self%patches(x, y)%temperature = 16.2
                end if
            end do
        end do
      end if
    end subroutine set_temperatures
    
    !this subroutine gets a grid and randomly turns on the lights in the grid
    subroutine change_lights(self)
      class(Grid) :: self
      integer :: x, y, i, lights_on(self%nlights)
      
      !turn off all lights
      do y=1, 3
        do x=1, 3
            self%light_patches(x, y)%lighted = .false. 
        end do
      end do
      
      !turn on new lights
      i=1
      do while (i<=self%nlights) !turn only the number of lights that were determined previously 
        x= floor(spread(1.,4.))
        y= floor(spread(1.,4.))
        if (.not. self%light_patches(x, y)%lighted) then
          self%light_patches(x, y)%lighted = .true.
          lights_on(i)= self%light_patches(x, y)%id
          i = i + 1
        end if
      end do
      call self%set_temperatures()
    end subroutine change_lights
    
end module Grid_module
