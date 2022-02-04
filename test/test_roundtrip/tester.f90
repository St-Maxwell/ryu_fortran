program main
    use ryu
    use iso_fortran_env, only: real32, real64
    implicit none

    call test_real32()
    write (*, "(A)")
    call test_real64()

contains

    subroutine test_real32()
        integer, parameter :: num_sample = 133879
        real(kind=real32) :: raw_float
        real(kind=real32) :: reverse_float
        character(len=:), allocatable :: string
        logical :: all_correct
        integer :: i, u, u2

        write (*, "('TEST CONVERSION FOR REAL32')")
        all_correct = .true.
        open (newunit=u, file="test/test_roundtrip/float_samples.txt", status="old")
        open (newunit=u2, file="test/test_roundtrip/check_first_50_float.txt")
        do i = 1, num_sample
            read (u, *) raw_float
            string = f2shortest(raw_float)
            read (string, *) reverse_float
            if (i >= 1 .and. i <= 50) then
                write (u2, "(ES16.7,2X,ES16.7,4X,A)") raw_float, reverse_float, string
            end if
            if (raw_float /= reverse_float) then
                write (*, "('Wrong single float number conversion: ',ES16.7, ' -> ',A)") raw_float, string
                all_correct = .false.
            end if
        end do
        close (u)
        close (u2)
        if (all_correct) write (*, "('All conversions are correct')")
        write (*, "('TEST OVER')")

    end subroutine test_real32

    subroutine test_real64()
        integer, parameter :: num_sample = 1000002
        real(kind=real64) :: raw_double
        real(kind=real64) :: reverse_double
        character(len=:), allocatable :: string
        logical :: all_correct
        integer :: i, u, u2

        write (*, "('TEST CONVERSION FOR REAL64')")
        all_correct = .true.
        open (newunit=u, file="test/test_roundtrip/double_samples.txt", status="old")
        open (newunit=u2, file="test/test_roundtrip/check_first_50_double.txt")
        do i = 1, num_sample
            read (u, *) raw_double
            string = d2shortest(raw_double)
            read (string, *) reverse_double
            if (i >= 1 .and. i <= 50) then
                write (u2, "(ES25.16,2X,ES25.16,4X,A)") raw_double, reverse_double, string
            end if
            if (raw_double /= reverse_double) then
                write (*, "('Wrong double float number conversion: ',ES25.16, ' -> ',A)") raw_double, string
                all_correct = .false.
            end if
        end do
        close (u)
        close (u2)
        if (all_correct) write (*, "('All conversions are correct')")
        write (*, "('TEST OVER')")

    end subroutine test_real64

end program main
