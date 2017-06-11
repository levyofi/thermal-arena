! Author: Ofir Levy, levyofi@gmail.com
! This module implements an Object, which is an abstract module for all other organisms, such as a lizard in our case. 

module Object_module   
   
   type, public, abstract :: Object
        integer :: x, y
        
    contains
        procedure (step_abstract), pass(self), deferred :: step
        procedure (add_to_patch_abstract), pass(self), deferred :: add_to_patch
        procedure (move_to_patch_abstract), pass(self), deferred :: move_to_patch
        procedure (create_abstract), pass(self), deferred :: create
    end type Object

    contains
        subroutine step_abstract(self)
            class(Object) :: self
        end subroutine step_abstract

        subroutine add_to_patch_abstract(self, x, y)
            class(Object) :: self
            integer x, y
        end subroutine add_to_patch_abstract

        subroutine move_to_patch_abstract(self, x, y)
            class(Object) :: self
            integer x, y
        end subroutine move_to_patch_abstract

        subroutine create_abstract(self)
            class(Object) :: self
        end subroutine create_abstract

end module Object_module
