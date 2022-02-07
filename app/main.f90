module to_string_m
    use ryu, only: f2shortest, d2shortest, d2fixed, d2exp
    use iso_fortran_env, only: int32, real32, real64
    implicit none
    private
    public :: to_string

    interface to_string
        module procedure :: to_string_real32
        module procedure :: to_string_real64
    end interface

contains
    
    function to_string_real32(f, fmt) result(str)
        real(kind=real32), intent(in) :: f
        character(len=*), intent(in), optional :: fmt
        character(len=:), allocatable :: str

        if (.not. present(fmt)) then
            str = f2shortest(f)
        else
            block
                integer :: p, t
                call parse_fmt(fmt, p, t)
                if (t == 0) then
                    str = f2fixed(f, p)
                else
                    str = f2exp(f, p)
                end if
            end block
        end if

    end function to_string_real32

    function to_string_real64(f, fmt) result(str)
        real(kind=real64), intent(in) :: f
        character(len=*), intent(in), optional :: fmt
        character(len=:), allocatable :: str

        if (.not. present(fmt)) then
            str = d2shortest(f)
        else
            block
                integer :: p, t
                call parse_fmt(fmt, p, t)
                if (t == 0) then
                    str = d2fixed(f, p)
                else
                    str = d2exp(f, p)
                end if
            end block
        end if

    end function to_string_real64

    function f2fixed(f, precision_) result(str)
        real(kind=real32), intent(in) :: f
        integer(kind=int32), intent(in) :: precision_
        character(len=:), allocatable :: str

        str = d2fixed(real(f, real64), precision_)

    end function f2fixed
    
    function f2exp(f, precision_) result(str)
        real(kind=real32), intent(in) :: f
        integer(kind=int32), intent(in) :: precision_
        character(len=:), allocatable :: str

        str = d2exp(real(f, real64), precision_)

    end function f2exp

    subroutine parse_fmt(fmt, precision, type)
        character(len=*), intent(in) :: fmt ! %.xxf or %.xxe
        integer, intent(out) :: precision
        integer, intent(out) :: type ! 0: fixed; 1: exp
        integer :: sz

        ! check head
        if (fmt(1:2) /= "%.") error stop "Invalid format"

        sz = len(trim(fmt))
        ! check tail
        if (fmt(sz:sz) == 'f') then
            type = 0
        else if (fmt(sz:sz) == 'e') then
            type = 1
        else
            error stop "Invalid format"
        end if

        ! read precision
        read(fmt(3:sz-1),*) precision

    end subroutine parse_fmt

end module to_string_m

program main
    use to_string_m, only: to_string
    use iso_fortran_env, only: real32, real64
    implicit none

    write(*,"(A)") to_string(3.14_real32)
    write(*,"(A)") to_string(3.14_real32, "%.5f")
    write(*,"(A)") to_string(3.14_real32, "%.5e")
    write(*,"(A)") to_string(114514.1919810_real64)
    write(*,"(A)") to_string(114514.1919810_real64, "%.10f")
    write(*,"(A)") to_string(114514.1919810_real64, "%.10e")

end program main
