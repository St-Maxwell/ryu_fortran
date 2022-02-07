program main
    use testdrive
    use iso_fortran_env
    use test_d2fixed
    implicit none
    integer :: stat, is
    type(testsuite_type), dimension(:), allocatable :: testsuites
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite("test_basic", collect_basic), &
                 new_testsuite("test_zeros", collect_zeros), &
                 new_testsuite("test_max_and_min", collect_max_and_min), &
                 new_testsuite("test_round_to_even", collect_round_to_even), &
                 new_testsuite("test_round_to_even_integer", collect_round_to_even_integer), &
                 new_testsuite("test_non_round_to_even_scenarios", collect_non_round_to_even_scenarios), &
                 new_testsuite("test_varying_precision", collect_varying_precision), &
                 new_testsuite("test_carrying", collect_carrying), &
                 new_testsuite("test_rounding_result_zero", collect_rounding_result_zero), &
                 new_testsuite("test_regression", collect_regression) &
                 ]

    do is = 1, size(testsuites)
        write (error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
    
end program main