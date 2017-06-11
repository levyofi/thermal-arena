! Author: Ofir Levy, levyofi@gmail.com
! This module implements a Factory as part of the Factory design pattern.
! It is an abstract module, the parent class for the Lizard_factory.
! A factory should be able to create a new object, clear all objects, calculate summary statistics, and run a step routine at each time step
! 
module Factory_module
    use Object_module
    use List_module
    implicit none

    type, public, abstract :: Factory
    contains
        procedure (factory_create_abstract), nopass, deferred :: create
        procedure (factory_step_abstract), nopass, deferred :: step
        procedure (factory_clear_abstract), deferred :: clear
        procedure (factory_init_abstract), nopass, deferred :: init
        procedure (factory_statistics_abstract), deferred :: print_statistics
    end type Factory

    abstract interface
        subroutine factory_create_abstract(m_object)
            import :: Factory
            import :: Object
            class(object), pointer :: m_object
        end subroutine factory_create_abstract

        subroutine factory_step_abstract(m_list)
            import :: Factory
            import :: list
            class(list) :: m_list
        end subroutine factory_step_abstract

        subroutine factory_clear_abstract(self)
            import :: Factory
            class(factory) :: self
        end subroutine factory_clear_abstract

        subroutine factory_statistics_abstract(self)
            import :: Factory
            class(factory) :: self

        end subroutine factory_statistics_abstract
        
        subroutine factory_init_abstract()

        end subroutine factory_init_abstract

      end interface
end module Factory_module
