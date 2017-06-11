! Author: Ofir Levy, levyofi@gmail.com

program main
    use Object_module
    use List_module
    use Observer_module
    use Lizard_module
    implicit none
    
    integer :: nlights
    integer :: ihour, isim, nhours=6, nsims
    character*500 :: outputfilename, str_nsims
       
    !initiailize number generator    
    call init_random_seed()
    
    call get_command_argument(2, outputfilename) ! get the output file name
    call get_command_argument(4, str_nsims) !get the number of simulations
    read(str_nsims,*)  nsims    
    
    !create the output file and its header
    open(3, file = "summary_"//outputfilename, status = "REPLACE")
    write (3, '(a)') "time_moved distance_moved e_balance mean_tb"
    do isim = 1, nsims
      if (mod(isim,1000) == 0 ) then
        print *, isim
      end if
      !initialize the observer
      call initialize()
      m_observer%nsims_to_run = nsims
      
      !initialize the factory and create one lizard
      call m_lizard_factory%init()    
      call m_observer%populate()
      
      !run the simulation for each hour
      do ihour=1, nhours
          call m_observer%step()
      end do
      
      !finish the simulation
      call observer_finalize()
    end do !end of simulation
    close(3)
contains    
    SUBROUTINE init_random_seed()
            INTEGER :: i, n, clock
            INTEGER, DIMENSION(:), ALLOCATABLE :: seed

            CALL RANDOM_SEED(size = n)
            ALLOCATE(seed(n))

            CALL SYSTEM_CLOCK(COUNT=clock)

            seed = clock + 37 * (/ (i - 1, i = 1, n) /)
            CALL RANDOM_SEED(PUT = seed)

            DEALLOCATE(seed)
    END SUBROUTINE
end program main
