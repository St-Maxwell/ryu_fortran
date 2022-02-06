program main
    use testdrive
    use iso_fortran_env
    use test_d2shortest
    implicit none
    integer :: stat, is
    type(testsuite_type), dimension(:), allocatable :: testsuites
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite("test_basic", collect_basic), &
                 new_testsuite("test_subnormal", collect_subnormal), &
                 new_testsuite("test_max_and_min", collect_max_and_min), &
                 new_testsuite("test_trailing_zeros", collect_trailing_zeros), &
                 new_testsuite("test_regression", collect_regression), &
                 new_testsuite("test_looks_like_pow5", collect_looks_like_pow5), &
                 new_testsuite("test_output_length", collect_output_length), &
                 new_testsuite("test_min_max_shift", collect_min_max_shift), &
                 new_testsuite("test_small_integers", collect_small_integers) &
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