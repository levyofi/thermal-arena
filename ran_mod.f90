! Author: Ofir Levy, levyofi@gmail.com
! This module implements a function that generated a random number between min - max (the code of the function is from http://www.sdsc.edu/~tkaiser/f90.html)

module ran_mod

contains
    !this function returns a random number between min - max
    function spread(min,max)  
        implicit none
        real spread
        real min,max
        real ran1
        
        call random_number(ran1) 
        spread=(max - min) * ran1 + min
    end function spread
end module ran_mod
