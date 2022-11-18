module test_f2shortest
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use real32_to_shortest
    use iso_fortran_env, only: int32, real32
    implicit none
    private
    public :: collect_basic, collect_subnormal, collect_max_and_min, collect_boundary_round_even
    public :: collect_exact_value_round_even, collect_trailing_zeros, collect_regression
    public :: collect_looks_like_pow5, collect_output_length

    real(kind=real32), parameter :: NaN = transfer(int(z'FF800001', int32), 1._real32)
    real(kind=real32), parameter :: plus_infinity = transfer(int(z'7F800000', int32), 1._real32)
    real(kind=real32), parameter :: minus_infinity = transfer(int(z'FF800000', int32), 1._real32)

contains

!> =============================================================================

    subroutine collect_basic(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("plus_zero", test_plus_zero), &
                    new_unittest("minus_zero", test_minus_zero), &
                    new_unittest("NaN", test_NaN), &
                    new_unittest("plus_infinity", test_plus_infinity), &
                    new_unittest("minus_infinity", test_minus_infinity) &
                    ]

    end subroutine collect_basic

    subroutine test_plus_zero(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(0._real32), "0.0")
        write (*, "(A,4X,A)") f2shortest(0._real32), "0.0"
        if (allocated(error)) return

    end subroutine test_plus_zero

    subroutine test_minus_zero(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(-0._real32), "-0.0")
        write (*, "(A,4X,A)") f2shortest(-0._real32), "-0.0"
        if (allocated(error)) return

    end subroutine test_minus_zero

    subroutine test_NaN(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(NaN), "NaN")
        write (*, "(A,4X,A)") f2shortest(NaN), "NaN"
        if (allocated(error)) return

    end subroutine test_NaN

    subroutine test_plus_infinity(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(plus_infinity), "Infinity")
        write (*, "(A,4X,A)") f2shortest(plus_infinity), "Infinity"
        if (allocated(error)) return

    end subroutine test_plus_infinity

    subroutine test_minus_infinity(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(minus_infinity), "-Infinity")
        write (*, "(A,4X,A)") f2shortest(minus_infinity), "-Infinity"
        if (allocated(error)) return

    end subroutine test_minus_infinity

!> =============================================================================

    subroutine collect_subnormal(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("subnormal", test_subnormal) &
                    ]

    end subroutine collect_subnormal

    subroutine test_subnormal(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(1.1754944e-38_real32), "1.1754944E-38")
        write (*, "(A,4X,A)") f2shortest(1.1754944e-38_real32), "1.1754944E-38"
        if (allocated(error)) return

    end subroutine test_subnormal

!> =============================================================================

    subroutine collect_max_and_min(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("max", test_max), &
                    new_unittest("min", test_min) &
                    ]

    end subroutine collect_max_and_min

    subroutine test_max(error)
        type(error_type), allocatable, intent(out) :: error
        real(kind=real32), parameter :: max = transfer(2139095039_4, 1._real32)

        call check(error, f2shortest(max), "3.4028235E38")
        write (*, "(A,4X,A)") f2shortest(max), "3.4028235E38"
        if (allocated(error)) return

    end subroutine test_max

    subroutine test_min(error)
        type(error_type), allocatable, intent(out) :: error
        real(kind=real32), parameter :: min = transfer(1_4, 1._real32)

        call check(error, f2shortest(min), "1.4E-45")
        write (*, "(A,4X,A)") f2shortest(min), "1.4E-45"
        if (allocated(error)) return

    end subroutine test_min

!> =============================================================================

    subroutine collect_boundary_round_even(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("value1", test_boundary_round_even_value1), &
                    new_unittest("value2", test_boundary_round_even_value2), &
                    new_unittest("value3", test_boundary_round_even_value3) &
                    ]

    end subroutine collect_boundary_round_even

    subroutine test_boundary_round_even_value1(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(3.355445e7_real32), "3.355445E7")
        write (*, "(A,4X,A)") f2shortest(3.355445e7_real32), "3.355445E7"
        if (allocated(error)) return

    end subroutine test_boundary_round_even_value1

    subroutine test_boundary_round_even_value2(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(8.999999e9_real32), "9.0E9")
        write (*, "(A,4X,A)") f2shortest(8.999999e9_real32), "9.0E9"
        if (allocated(error)) return

    end subroutine test_boundary_round_even_value2

    subroutine test_boundary_round_even_value3(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(3.4366717e10_real32), "3.436672E10")
        write (*, "(A,4X,A)") f2shortest(3.4366717e10_real32), "3.436672E10"
        if (allocated(error)) return

    end subroutine test_boundary_round_even_value3

!> =============================================================================

    subroutine collect_exact_value_round_even(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("value1", test_exact_value_round_even_value1), &
                    new_unittest("value2", test_exact_value_round_even_value2) &
                    ]

    end subroutine collect_exact_value_round_even

    subroutine test_exact_value_round_even_value1(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(3.0540412e5_real32), "305404.12")
        write (*, "(A,4X,A)") f2shortest(3.0540412e5_real32), "305404.12"
        if (allocated(error)) return

    end subroutine test_exact_value_round_even_value1

    subroutine test_exact_value_round_even_value2(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(8.0990312e3_real32), "8099.0312")
        write (*, "(A,4X,A)") f2shortest(8.0990312e3_real32), "8099.0312"
        if (allocated(error)) return

    end subroutine test_exact_value_round_even_value2

!> =============================================================================

    subroutine collect_trailing_zeros(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("value1", test_trailing_zeros_value1), &
                    new_unittest("value2", test_trailing_zeros_value2), &
                    new_unittest("value3", test_trailing_zeros_value3), &
                    new_unittest("value4", test_trailing_zeros_value4) &
                    ]

    end subroutine collect_trailing_zeros

    subroutine test_trailing_zeros_value1(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(2.4414062e-4_real32), "2.4414062E-4")
        write (*, "(A,4X,A)") f2shortest(2.4414062e-4_real32), "2.4414062E-4"
        if (allocated(error)) return

    end subroutine test_trailing_zeros_value1

    subroutine test_trailing_zeros_value2(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(2.4414062e-3_real32), "0.0024414062")
        write (*, "(A,4X,A)") f2shortest(2.4414062e-3_real32), "0.0024414062"
        if (allocated(error)) return

    end subroutine test_trailing_zeros_value2

    subroutine test_trailing_zeros_value3(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(4.3945312e-3_real32), "0.0043945312")
        write (*, "(A,4X,A)") f2shortest(4.3945312e-3_real32), "0.0043945312"
        if (allocated(error)) return

    end subroutine test_trailing_zeros_value3

    subroutine test_trailing_zeros_value4(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(6.3476562e-3_real32), "0.0063476562")
        write (*, "(A,4X,A)") f2shortest(6.3476562e-3_real32), "0.0063476562"
        if (allocated(error)) return

    end subroutine test_trailing_zeros_value4

!> =============================================================================

    subroutine collect_regression(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("value1", test_regression_value1), &
                    new_unittest("value2", test_regression_value2), &
                    new_unittest("value3", test_regression_value3), &
                    new_unittest("value4", test_regression_value4), &
                    new_unittest("value5", test_regression_value5), &
                    new_unittest("value6", test_regression_value6), &
                    new_unittest("value7", test_regression_value7), &
                    new_unittest("value8", test_regression_value8), &
                    new_unittest("value9", test_regression_value9), &
                    new_unittest("value10", test_regression_value10), &
                    new_unittest("value11", test_regression_value11), &
                    new_unittest("value12", test_regression_value12), &
                    new_unittest("value13", test_regression_value13), &
                    new_unittest("value14", test_regression_value14), &
                    new_unittest("value15", test_regression_value15), &
                    new_unittest("value16", test_regression_value16), &
                    new_unittest("value17", test_regression_value17), &
                    new_unittest("value18", test_regression_value18), &
                    new_unittest("value19", test_regression_value19), &
                    new_unittest("value20", test_regression_value20), &
                    new_unittest("value21", test_regression_value21), &
                    new_unittest("value22", test_regression_value22), &
                    new_unittest("value23", test_regression_value23), &
                    new_unittest("value24", test_regression_value24), &
                    new_unittest("value25", test_regression_value25), &
                    new_unittest("value26", test_regression_value26), &
                    new_unittest("value27", test_regression_value27), &
                    new_unittest("value28", test_regression_value28), &
                    new_unittest("value29", test_regression_value29) &
                    ]

    end subroutine collect_regression

    subroutine test_regression_value1(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(4.7223665e21_real32), "4.7223665E21")
        write (*, "(A,4X,A)") f2shortest(4.7223665e21_real32), "4.7223665E21"
        if (allocated(error)) return

    end subroutine test_regression_value1

    subroutine test_regression_value2(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(8388608.0_real32), "8388608.0")
        write (*, "(A,4X,A)") f2shortest(8388608.0_real32), "8388608.0"
        if (allocated(error)) return

    end subroutine test_regression_value2

    subroutine test_regression_value3(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(1.6777216e7_real32), "1.6777216E7")
        write (*, "(A,4X,A)") f2shortest(1.6777216e7_real32), "1.6777216E7"
        if (allocated(error)) return

    end subroutine test_regression_value3
    subroutine test_regression_value4(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(3.3554436e7_real32), "3.3554436E7")
        write (*, "(A,4X,A)") f2shortest(3.3554436e7_real32), "3.3554436E7"
        if (allocated(error)) return

    end subroutine test_regression_value4
    subroutine test_regression_value5(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(6.7131496e7_real32), "6.7131496E7")
        write (*, "(A,4X,A)") f2shortest(6.7131496e7_real32), "6.7131496E7"
        if (allocated(error)) return

    end subroutine test_regression_value5
    subroutine test_regression_value6(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(1.9310392e-38_real32), "1.9310392E-38")
        write (*, "(A,4X,A)") f2shortest(1.9310392e-38_real32), "1.9310392E-38"
        if (allocated(error)) return

    end subroutine test_regression_value6
    subroutine test_regression_value7(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(-2.47e-43_real32), "-2.47E-43")
        write (*, "(A,4X,A)") f2shortest(-2.47e-43_real32), "-2.47E-43"
        if (allocated(error)) return

    end subroutine test_regression_value7

    subroutine test_regression_value8(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(1.993244e-38_real32), "1.993244E-38")
        write (*, "(A,4X,A)") f2shortest(1.993244e-38_real32), "1.993244E-38"
        if (allocated(error)) return

    end subroutine test_regression_value8

    subroutine test_regression_value9(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(4103.9003_real32), "4103.9004")
        write (*, "(A,4X,A)") f2shortest(4103.9003_real32), "4103.9004"
        if (allocated(error)) return

    end subroutine test_regression_value9

    subroutine test_regression_value10(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(5.3399997e9_real32), "5.3399997E9")
        write (*, "(A,4X,A)") f2shortest(5.3399997e9_real32), "5.3399997E9"
        if (allocated(error)) return

    end subroutine test_regression_value10

    subroutine test_regression_value11(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(6.0898e-39_real32), "6.0898E-39")
        write (*, "(A,4X,A)") f2shortest(6.0898e-39_real32), "6.0898E-39"
        if (allocated(error)) return

    end subroutine test_regression_value11

    subroutine test_regression_value12(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(0.0010310042_real32), "0.0010310042")
        write (*, "(A,4X,A)") f2shortest(0.0010310042_real32), "0.0010310042"
        if (allocated(error)) return

    end subroutine test_regression_value12

    subroutine test_regression_value13(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(2.8823261e17_real32), "2.882326E17")
        write (*, "(A,4X,A)") f2shortest(2.8823261e17_real32), "2.882326E17"
        if (allocated(error)) return

    end subroutine test_regression_value13

    subroutine test_regression_value14(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(7.0385309e-26_real32), "7.038531E-26")
        write (*, "(A,4X,A)") f2shortest(7.0385309e-26_real32), "7.038531E-26"
        if (allocated(error)) return

    end subroutine test_regression_value14

    subroutine test_regression_value15(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(9.2234038e17_real32), "9.223404E17")
        write (*, "(A,4X,A)") f2shortest(9.2234038e17_real32), "9.223404E17"
        if (allocated(error)) return

    end subroutine test_regression_value15

    subroutine test_regression_value16(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(6.7108872e7_real32), "6.710887E7")
        write (*, "(A,4X,A)") f2shortest(6.7108872e7_real32), "6.710887E7"
        if (allocated(error)) return

    end subroutine test_regression_value16

    subroutine test_regression_value17(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(1.0e-44_real32), "1.0E-44")
        write (*, "(A,4X,A)") f2shortest(1.0e-44_real32), "1.0E-44"
        if (allocated(error)) return

    end subroutine test_regression_value17

    subroutine test_regression_value18(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(2.816025e14_real32), "2.816025E14")
        write (*, "(A,4X,A)") f2shortest(2.816025e14_real32), "2.816025E14"
        if (allocated(error)) return

    end subroutine test_regression_value18

    subroutine test_regression_value19(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(9.223372e18_real32), "9.223372E18")
        write (*, "(A,4X,A)") f2shortest(9.223372e18_real32), "9.223372E18"
        if (allocated(error)) return

    end subroutine test_regression_value19

    subroutine test_regression_value20(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(1.5846085e29_real32), "1.5846086E29")
        write (*, "(A,4X,A)") f2shortest(1.5846085e29_real32), "1.5846086E29"
        if (allocated(error)) return

    end subroutine test_regression_value20

    subroutine test_regression_value21(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(1.1811161e19_real32), "1.1811161E19")
        write (*, "(A,4X,A)") f2shortest(1.1811161e19_real32), "1.1811161E19"
        if (allocated(error)) return

    end subroutine test_regression_value21

    subroutine test_regression_value22(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(5.368709e18_real32), "5.368709E18")
        write (*, "(A,4X,A)") f2shortest(5.368709e18_real32), "5.368709E18"
        if (allocated(error)) return

    end subroutine test_regression_value22

    subroutine test_regression_value23(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(4.6143165e18_real32), "4.6143166E18")
        write (*, "(A,4X,A)") f2shortest(4.6143165e8_real32), "4.6143166E18"
        if (allocated(error)) return

    end subroutine test_regression_value23

    subroutine test_regression_value24(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(0.007812537_real32), "0.007812537")
        write (*, "(A,4X,A)") f2shortest(0.007812537_real32), "0.007812537"
        if (allocated(error)) return

    end subroutine test_regression_value24

    subroutine test_regression_value25(error)
        type(error_type), allocatable, intent(out) :: error
        real(kind=real32) :: f

        ! literal constant value '1.4E-45' can not be correctly converted to binary representation
        f = transfer(1_int32, 1._real32)

        call check(error, f2shortest(f), "1.4E-45")
        write (*, "(A,4X,A)") f2shortest(f), "1.4E-45"
        if (allocated(error)) return

    end subroutine test_regression_value25

    subroutine test_regression_value26(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(1.18697724e20_real32), "1.18697725E20")
        write (*, "(A,4X,A)") f2shortest(1.18697724e20_real32), "1.18697725E20"
        if (allocated(error)) return

    end subroutine test_regression_value26

    subroutine test_regression_value27(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(1.00014165e-36_real32), "1.00014165E-36")
        write (*, "(A,4X,A)") f2shortest(1.00014165e-36_real32), "1.00014165E-36"
        if (allocated(error)) return

    end subroutine test_regression_value27

    subroutine test_regression_value28(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(200.0_real32), "200.0")
        write (*, "(A,4X,A)") f2shortest(200.0_real32), "200.0"
        if (allocated(error)) return

    end subroutine test_regression_value28

    subroutine test_regression_value29(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(3.3554432e7_real32), "3.3554432E7")
        write (*, "(A,4X,A)") f2shortest(3.3554432e7_real32), "3.3554432E7"
        if (allocated(error)) return

    end subroutine test_regression_value29

!> =============================================================================

    subroutine collect_looks_like_pow5(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("value1", test_looks_like_pow5_value1), &
                    new_unittest("value2", test_looks_like_pow5_value2), &
                    new_unittest("value3", test_looks_like_pow5_value3) &
                    ]

    end subroutine collect_looks_like_pow5

    subroutine test_looks_like_pow5_value1(error)
        type(error_type), allocatable, intent(out) :: error
        real(kind=real32) :: f

        f = transfer(1561658105_int32, 1._real32)

        call check(error, f2shortest(f), "6.7108864E17")
        write (*, "(A,4X,A)") f2shortest(f), "6.7108864E17"
        if (allocated(error)) return

    end subroutine test_looks_like_pow5_value1

    subroutine test_looks_like_pow5_value2(error)
        type(error_type), allocatable, intent(out) :: error
        real(kind=real32) :: f

        f = transfer(1570046713_int32, 1._real32)

        call check(error, f2shortest(f), "1.3421773E18")
        write (*, "(A,4X,A)") f2shortest(f), "1.3421773E18"
        if (allocated(error)) return

    end subroutine test_looks_like_pow5_value2

    subroutine test_looks_like_pow5_value3(error)
        type(error_type), allocatable, intent(out) :: error
        real(kind=real32) :: f

        f = transfer(1578435321_int32, 1._real32)

        call check(error, f2shortest(f), "2.6843546E18")
        write (*, "(A,4X,A)") f2shortest(f), "2.6843546E18"
        if (allocated(error)) return

    end subroutine test_looks_like_pow5_value3

!> =============================================================================

    subroutine collect_output_length(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("value1", test_output_length_value1), &
                    new_unittest("value2", test_output_length_value2), &
                    new_unittest("value3", test_output_length_value3), &
                    new_unittest("value4", test_output_length_value4), &
                    new_unittest("value5", test_output_length_value5), &
                    new_unittest("value6", test_output_length_value6), &
                    new_unittest("value7", test_output_length_value7), &
                    new_unittest("value8", test_output_length_value8), &
                    new_unittest("value9", test_output_length_value9) &
                    ]

    end subroutine collect_output_length

    subroutine test_output_length_value1(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(1.0_real32), "1.0")
        write (*, "(A,4X,A)") f2shortest(1.0_real32), "1.0"
        if (allocated(error)) return

    end subroutine test_output_length_value1

    subroutine test_output_length_value2(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(1.2_real32), "1.2")
        write (*, "(A,4X,A)") f2shortest(1.2_real32), "1.2"
        if (allocated(error)) return

    end subroutine test_output_length_value2

    subroutine test_output_length_value3(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(1.23_real32), "1.23")
        write (*, "(A,4X,A)") f2shortest(1.23_real32), "1.23"
        if (allocated(error)) return

    end subroutine test_output_length_value3

    subroutine test_output_length_value4(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(1.234_real32), "1.234")
        write (*, "(A,4X,A)") f2shortest(1.234_real32), "1.234"
        if (allocated(error)) return

    end subroutine test_output_length_value4

    subroutine test_output_length_value5(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(1.2345_real32), "1.2345")
        write (*, "(A,4X,A)") f2shortest(1.2345_real32), "1.2345"
        if (allocated(error)) return

    end subroutine test_output_length_value5

    subroutine test_output_length_value6(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(1.23456_real32), "1.23456")
        write (*, "(A,4X,A)") f2shortest(1.23456_real32), "1.23456"
        if (allocated(error)) return

    end subroutine test_output_length_value6

    subroutine test_output_length_value7(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(1.234567_real32), "1.234567")
        write (*, "(A,4X,A)") f2shortest(1.234567_real32), "1.234567"
        if (allocated(error)) return

    end subroutine test_output_length_value7

    subroutine test_output_length_value8(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(1.2345678_real32), "1.2345678")
        write (*, "(A,4X,A)") f2shortest(1.2345678_real32), "1.2345678"
        if (allocated(error)) return

    end subroutine test_output_length_value8

    subroutine test_output_length_value9(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2shortest(1.23456735e-36_real32), "1.23456735E-36")
        write (*, "(A,4X,A)") f2shortest(1.23456735e-36_real32), "1.23456735E-36"
        if (allocated(error)) return

    end subroutine test_output_length_value9

end module test_f2shortest
