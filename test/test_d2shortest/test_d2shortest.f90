module test_d2shortest
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use real64_to_shortest
    use iso_fortran_env, only: int32, int64, real64
    implicit none
    private
    public :: collect_basic, collect_subnormal, collect_max_and_min, collect_trailing_zeros
    public :: collect_regression, collect_looks_like_pow5, collect_output_length
    public :: collect_min_max_shift, collect_small_integers

    real(kind=real64), parameter :: NaN = transfer(int(z'7FF0000000000001', int64), 1._real64)
    real(kind=real64), parameter :: plus_infinity = transfer(int(z'7FF0000000000000', int64), 1._real64)
    real(kind=real64), parameter :: minus_infinity = transfer(int(z'FFF0000000000000', int64), 1._real64)
    integer(kind=int64), parameter :: max_mantissa = shiftl(1_int64, 53) - 1

contains

    function ieee_part_to_real64(sign, ieee_exponent, ieee_mantissa) result(d)
        logical(kind=int32), intent(in) :: sign
        integer(kind=int32), intent(in) :: ieee_exponent
        integer(kind=int64), intent(in) :: ieee_mantissa
        real(kind=real64) :: d
        integer(kind=int64) :: i

        if (sign) then
            i = ior(shiftl(1_int64, 63), ior(shiftl(int(ieee_exponent, int64), 52), ieee_mantissa))
        else
            i = ior(shiftl(0_int64, 63), ior(shiftl(int(ieee_exponent, int64), 52), ieee_mantissa))
        end if

        d = transfer(i, 1._real64)

    end function ieee_part_to_real64

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

        call check(error, d2shortest(0._real64), "0.0")
        write (*, "(A,4X,A)") d2shortest(0._real64), "0.0"
        if (allocated(error)) return

    end subroutine test_plus_zero

    subroutine test_minus_zero(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(-0._real64), "-0.0")
        write (*, "(A,4X,A)") d2shortest(-0._real64), "-0.0"
        if (allocated(error)) return

    end subroutine test_minus_zero

    subroutine test_NaN(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(NaN), "NaN")
        write (*, "(A,4X,A)") d2shortest(NaN), "NaN"
        if (allocated(error)) return

    end subroutine test_NaN

    subroutine test_plus_infinity(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(plus_infinity), "Infinity")
        write (*, "(A,4X,A)") d2shortest(plus_infinity), "Infinity"
        if (allocated(error)) return

    end subroutine test_plus_infinity

    subroutine test_minus_infinity(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(minus_infinity), "-Infinity")
        write (*, "(A,4X,A)") d2shortest(minus_infinity), "-Infinity"
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

        call check(error, d2shortest(2.2250738585072014e-308_real64), "2.2250738585072014E-308")
        write (*, "(A,4X,A)") d2shortest(2.2250738585072014e-308_real64), "2.2250738585072014E-308"
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
        real(kind=real64), parameter :: max = transfer(int(z'7fefffffffffffff', int64), 1._real64)

        call check(error, d2shortest(max), "1.7976931348623157E308")
        write (*, "(A,4X,A)") d2shortest(max), "1.7976931348623157E308"
        if (allocated(error)) return

    end subroutine test_max

    subroutine test_min(error)
        type(error_type), allocatable, intent(out) :: error
        real(kind=real64), parameter :: min = transfer(1_int64, 1._real64)

        call check(error, d2shortest(min), "4.9E-324")
        write (*, "(A,4X,A)") d2shortest(min), "4.9E-324"
        if (allocated(error)) return

    end subroutine test_min

!> =============================================================================

    subroutine collect_trailing_zeros(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("value1", test_trailing_zeros_value1) &
                    ]

    end subroutine collect_trailing_zeros

    subroutine test_trailing_zeros_value1(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(2.98023223876953125e-8_real64), "2.9802322387695312E-8")
        write (*, "(A,4X,A)") d2shortest(2.98023223876953125e-8_real64), "2.9802322387695312E-8"
        if (allocated(error)) return

    end subroutine test_trailing_zeros_value1

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
                    new_unittest("value8", test_regression_value8) &
                    ]

    end subroutine collect_regression

    subroutine test_regression_value1(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(-2.109808898695963e16_real64), "-2.109808898695963E16")
        write (*, "(A,4X,A)") d2shortest(-2.109808898695963e16_real64), "-2.109808898695963E16"
        if (allocated(error)) return

    end subroutine test_regression_value1

    subroutine test_regression_value2(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(4.940656e-318_real64), "4.940656E-318")
        write (*, "(A,4X,A)") d2shortest(4.940656e-318_real64), "4.940656E-318"
        if (allocated(error)) return

    end subroutine test_regression_value2

    subroutine test_regression_value3(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(1.18575755e-316_real64), "1.18575755E-316")
        write (*, "(A,4X,A)") d2shortest(1.18575755e-316_real64), "1.18575755E-316"
        if (allocated(error)) return

    end subroutine test_regression_value3
    subroutine test_regression_value4(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(2.989102097996e-312_real64), "2.989102097996E-312")
        write (*, "(A,4X,A)") d2shortest(2.989102097996e-312_real64), "2.989102097996E-312"
        if (allocated(error)) return

    end subroutine test_regression_value4
    subroutine test_regression_value5(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(9.0608011534336e15_real64), "9.0608011534336E15")
        write (*, "(A,4X,A)") d2shortest(9.0608011534336e15_real64), "9.0608011534336E15"
        if (allocated(error)) return

    end subroutine test_regression_value5
    subroutine test_regression_value6(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(4.708356024711512e18_real64), "4.708356024711512E18")
        write (*, "(A,4X,A)") d2shortest(4.708356024711512e18_real64), "4.708356024711512E18"
        if (allocated(error)) return

    end subroutine test_regression_value6
    subroutine test_regression_value7(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(9.409340012568248e18_real64), "9.409340012568248E18")
        write (*, "(A,4X,A)") d2shortest(9.409340012568248e18_real64), "9.409340012568248E18"
        if (allocated(error)) return

    end subroutine test_regression_value7

    subroutine test_regression_value8(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(1.2345678_real64), "1.2345678")
        write (*, "(A,4X,A)") d2shortest(1.2345678_real64), "1.2345678"
        if (allocated(error)) return

    end subroutine test_regression_value8

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
        real(kind=real64) :: f

        f = transfer(int(z'4830F0CF064DD592', int64), 1._real64)

        call check(error, d2shortest(f), "5.764607523034235E39")
        write (*, "(A,4X,A)") d2shortest(f), "5.764607523034235E39"
        if (allocated(error)) return

    end subroutine test_looks_like_pow5_value1

    subroutine test_looks_like_pow5_value2(error)
        type(error_type), allocatable, intent(out) :: error
        real(kind=real64) :: f

        f = transfer(int(z'4840F0CF064DD592', int64), 1._real64)

        call check(error, d2shortest(f), "1.152921504606847E40")
        write (*, "(A,4X,A)") d2shortest(f), "1.152921504606847E40"
        if (allocated(error)) return

    end subroutine test_looks_like_pow5_value2

    subroutine test_looks_like_pow5_value3(error)
        type(error_type), allocatable, intent(out) :: error
        real(kind=real64) :: f

        f = transfer(int(z'4850F0CF064DD592', int64), 1._real64)

        call check(error, d2shortest(f), "2.305843009213694E40")
        write (*, "(A,4X,A)") d2shortest(f), "2.305843009213694E40"
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
                    new_unittest("value9", test_output_length_value9), &
                    new_unittest("value10", test_output_length_value10), &
                    new_unittest("value11", test_output_length_value11), &
                    new_unittest("value12", test_output_length_value12), &
                    new_unittest("value13", test_output_length_value13), &
                    new_unittest("value14", test_output_length_value14), &
                    new_unittest("value15", test_output_length_value15), &
                    new_unittest("value16", test_output_length_value16), &
                    new_unittest("value17", test_output_length_value17), &
                    new_unittest("value18", test_output_length_value18), &
                    new_unittest("value19", test_output_length_value19), &
                    new_unittest("value20", test_output_length_value20), &
                    new_unittest("value21", test_output_length_value21), &
                    new_unittest("value22", test_output_length_value22) &
                    ]

    end subroutine collect_output_length

    subroutine test_output_length_value1(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(1.0_real64), "1.0")
        write (*, "(A,4X,A)") d2shortest(1.0_real64), "1.0"
        if (allocated(error)) return

    end subroutine test_output_length_value1

    subroutine test_output_length_value2(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(1.2_real64), "1.2")
        write (*, "(A,4X,A)") d2shortest(1.2_real64), "1.2"
        if (allocated(error)) return

    end subroutine test_output_length_value2

    subroutine test_output_length_value3(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(1.23_real64), "1.23")
        write (*, "(A,4X,A)") d2shortest(1.23_real64), "1.23"
        if (allocated(error)) return

    end subroutine test_output_length_value3

    subroutine test_output_length_value4(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(1.234_real64), "1.234")
        write (*, "(A,4X,A)") d2shortest(1.234_real64), "1.234"
        if (allocated(error)) return

    end subroutine test_output_length_value4

    subroutine test_output_length_value5(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(1.2345_real64), "1.2345")
        write (*, "(A,4X,A)") d2shortest(1.2345_real64), "1.2345"
        if (allocated(error)) return

    end subroutine test_output_length_value5

    subroutine test_output_length_value6(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(1.23456_real64), "1.23456")
        write (*, "(A,4X,A)") d2shortest(1.23456_real64), "1.23456"
        if (allocated(error)) return

    end subroutine test_output_length_value6

    subroutine test_output_length_value7(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(1.234567_real64), "1.234567")
        write (*, "(A,4X,A)") d2shortest(1.234567_real64), "1.234567"
        if (allocated(error)) return

    end subroutine test_output_length_value7

    subroutine test_output_length_value8(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(1.2345678_real64), "1.2345678")
        write (*, "(A,4X,A)") d2shortest(1.2345678_real64), "1.2345678"
        if (allocated(error)) return

    end subroutine test_output_length_value8

    subroutine test_output_length_value9(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(1.23456789_real64), "1.23456789")
        write (*, "(A,4X,A)") d2shortest(1.23456789_real64), "1.23456789"
        if (allocated(error)) return

    end subroutine test_output_length_value9

    subroutine test_output_length_value10(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(1.234567895_real64), "1.234567895")
        write (*, "(A,4X,A)") d2shortest(1.234567895_real64), "1.234567895"
        if (allocated(error)) return

    end subroutine test_output_length_value10

    subroutine test_output_length_value11(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(1.2345678901_real64), "1.2345678901")
        write (*, "(A,4X,A)") d2shortest(1.2345678901_real64), "1.2345678901"
        if (allocated(error)) return

    end subroutine test_output_length_value11

    subroutine test_output_length_value12(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(1.23456789012_real64), "1.23456789012")
        write (*, "(A,4X,A)") d2shortest(1.23456789012_real64), "1.23456789012"
        if (allocated(error)) return

    end subroutine test_output_length_value12

    subroutine test_output_length_value13(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(1.234567890123_real64), "1.234567890123")
        write (*, "(A,4X,A)") d2shortest(1.234567890123_real64), "1.234567890123"
        if (allocated(error)) return

    end subroutine test_output_length_value13

    subroutine test_output_length_value14(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(1.2345678901234_real64), "1.2345678901234")
        write (*, "(A,4X,A)") d2shortest(1.2345678901234_real64), "1.2345678901234"
        if (allocated(error)) return

    end subroutine test_output_length_value14

    subroutine test_output_length_value15(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(1.23456789012345_real64), "1.23456789012345")
        write (*, "(A,4X,A)") d2shortest(1.23456789012345_real64), "1.23456789012345"
        if (allocated(error)) return

    end subroutine test_output_length_value15

    subroutine test_output_length_value16(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(1.234567890123456_real64), "1.234567890123456")
        write (*, "(A,4X,A)") d2shortest(1.234567890123456_real64), "1.234567890123456"
        if (allocated(error)) return

    end subroutine test_output_length_value16

    subroutine test_output_length_value17(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(1.2345678901234567_real64), "1.2345678901234567")
        write (*, "(A,4X,A)") d2shortest(1.2345678901234567_real64), "1.2345678901234567"
        if (allocated(error)) return

    end subroutine test_output_length_value17

    subroutine test_output_length_value18(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(4.294967294_real64), "4.294967294")
        write (*, "(A,4X,A)") d2shortest(4.294967294_real64), "4.294967294"
        if (allocated(error)) return

    end subroutine test_output_length_value18

    subroutine test_output_length_value19(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(4.294967295_real64), "4.294967295")
        write (*, "(A,4X,A)") d2shortest(4.294967295_real64), "4.294967295"
        if (allocated(error)) return

    end subroutine test_output_length_value19

    subroutine test_output_length_value20(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(4.294967296_real64), "4.294967296")
        write (*, "(A,4X,A)") d2shortest(4.294967296_real64), "4.294967296"
        if (allocated(error)) return

    end subroutine test_output_length_value20

    subroutine test_output_length_value21(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(4.294967297_real64), "4.294967297")
        write (*, "(A,4X,A)") d2shortest(4.294967297_real64), "4.294967297"
        if (allocated(error)) return

    end subroutine test_output_length_value21

    subroutine test_output_length_value22(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(4.294967298_real64), "4.294967298")
        write (*, "(A,4X,A)") d2shortest(4.294967298_real64), "4.294967298"
        if (allocated(error)) return

    end subroutine test_output_length_value22

!> =============================================================================

    subroutine collect_min_max_shift(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("value1", test_min_max_shift_value1), &
                    new_unittest("value2", test_min_max_shift_value2), &
                    new_unittest("value3", test_min_max_shift_value3), &
                    new_unittest("value4", test_min_max_shift_value4), &
                    new_unittest("value5", test_min_max_shift_value5), &
                    new_unittest("value6", test_min_max_shift_value6), &
                    new_unittest("value7", test_min_max_shift_value7), &
                    new_unittest("value8", test_min_max_shift_value8), &
                    new_unittest("value9", test_min_max_shift_value9) &
                    ]

    end subroutine collect_min_max_shift

    subroutine test_min_max_shift_value1(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(ieee_part_to_real64(.false., 4, 0_int64)), "1.7800590868057611E-307")
        write (*, "(A,4X,A)") d2shortest(ieee_part_to_real64(.false., 4, 0_int64)), "1.7800590868057611E-307"
        if (allocated(error)) return

    end subroutine test_min_max_shift_value1

    subroutine test_min_max_shift_value2(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(ieee_part_to_real64(.false., 6, max_mantissa)), "2.8480945388892175E-306")
        write (*, "(A,4X,A)") d2shortest(ieee_part_to_real64(.false., 6, max_mantissa)), "2.8480945388892175E-306"
        if (allocated(error)) return

    end subroutine test_min_max_shift_value2

    subroutine test_min_max_shift_value3(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(ieee_part_to_real64(.false., 41, 0_int64)), "2.446494580089078E-296")
        write (*, "(A,4X,A)") d2shortest(ieee_part_to_real64(.false., 41, 0_int64)), "2.446494580089078E-296"
        if (allocated(error)) return

    end subroutine test_min_max_shift_value3

    subroutine test_min_max_shift_value4(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(ieee_part_to_real64(.false., 40, max_mantissa)), "4.8929891601781557E-296")
        write (*, "(A,4X,A)") d2shortest(ieee_part_to_real64(.false., 40, max_mantissa)), "4.8929891601781557E-296"
        if (allocated(error)) return

    end subroutine test_min_max_shift_value4

    subroutine test_min_max_shift_value5(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(ieee_part_to_real64(.false., 1077, 0_int64)), "1.8014398509481984E16")
        write (*, "(A,4X,A)") d2shortest(ieee_part_to_real64(.false., 1077, 0_int64)), "1.8014398509481984E16"
        if (allocated(error)) return

    end subroutine test_min_max_shift_value5

    subroutine test_min_max_shift_value6(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(ieee_part_to_real64(.false., 1076, max_mantissa)), "3.6028797018963964E16")
        write (*, "(A,4X,A)") d2shortest(ieee_part_to_real64(.false., 1076, max_mantissa)), "3.6028797018963964E16"
        if (allocated(error)) return

    end subroutine test_min_max_shift_value6

    subroutine test_min_max_shift_value7(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(ieee_part_to_real64(.false., 307, 0_int64)), "2.900835519859558E-216")
        write (*, "(A,4X,A)") d2shortest(ieee_part_to_real64(.false., 307, 0_int64)), "2.900835519859558E-216"
        if (allocated(error)) return

    end subroutine test_min_max_shift_value7

    subroutine test_min_max_shift_value8(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(ieee_part_to_real64(.false., 306, max_mantissa)), "5.801671039719115E-216")
        write (*, "(A,4X,A)") d2shortest(ieee_part_to_real64(.false., 306, max_mantissa)), "5.801671039719115E-216"
        if (allocated(error)) return

    end subroutine test_min_max_shift_value8

    subroutine test_min_max_shift_value9(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2shortest(ieee_part_to_real64(.false., 934, int(z'000FA7161A4D6E0C', int64))), &
                   "3.196104012172126E-27")
        write (*, "(A,4X,A)") d2shortest(ieee_part_to_real64(.false., 934, int(z'000FA7161A4D6E0C', int64))), &
            "3.196104012172126E-27"
        if (allocated(error)) return

    end subroutine test_min_max_shift_value9

!> =============================================================================

    subroutine collect_small_integers(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("value1", test_small_integers_value1), &
                    new_unittest("value2", test_small_integers_value2), &
                    new_unittest("value3", test_small_integers_value3), &
                    new_unittest("value4", test_small_integers_value4), &
                    new_unittest("value5", test_small_integers_value5), &
                    new_unittest("value6", test_small_integers_value6), &
                    new_unittest("value7", test_small_integers_value7), &
                    new_unittest("value8", test_small_integers_value8), &
                    new_unittest("value9", test_small_integers_value9), &
                    new_unittest("value10", test_small_integers_value10), &
                    new_unittest("value11", test_small_integers_value11), &
                    new_unittest("value12", test_small_integers_value12), &
                    new_unittest("value13", test_small_integers_value13), &
                    new_unittest("value14", test_small_integers_value14), &
                    new_unittest("value15", test_small_integers_value15), &
                    new_unittest("value16", test_small_integers_value16), &
                    new_unittest("value17", test_small_integers_value17), &
                    new_unittest("value18", test_small_integers_value18), &
                    new_unittest("value19", test_small_integers_value19), &
                    new_unittest("value20", test_small_integers_value20), &
                    new_unittest("value21", test_small_integers_value21), &
                    new_unittest("value22", test_small_integers_value22), &
                    new_unittest("value23", test_small_integers_value23), &
                    new_unittest("value24", test_small_integers_value24), &
                    new_unittest("value25", test_small_integers_value25), &
                    new_unittest("value26", test_small_integers_value26), &
                    new_unittest("value27", test_small_integers_value27), &
                    new_unittest("value28", test_small_integers_value28), &
                    new_unittest("value29", test_small_integers_value29), &
                    new_unittest("value30", test_small_integers_value30), &
                    new_unittest("value31", test_small_integers_value31), &
                    new_unittest("value32", test_small_integers_value32), &
                    new_unittest("value33", test_small_integers_value33), &
                    new_unittest("value34", test_small_integers_value34), &
                    new_unittest("value35", test_small_integers_value35), &
                    new_unittest("value36", test_small_integers_value36), &
                    new_unittest("value37", test_small_integers_value37), &
                    new_unittest("value38", test_small_integers_value38), &
                    new_unittest("value39", test_small_integers_value39), &
                    new_unittest("value40", test_small_integers_value40), &
                    new_unittest("value41", test_small_integers_value41), &
                    new_unittest("value42", test_small_integers_value42), &
                    new_unittest("value43", test_small_integers_value43), &
                    new_unittest("value44", test_small_integers_value44), &
                    new_unittest("value45", test_small_integers_value45), &
                    new_unittest("value46", test_small_integers_value46), &
                    new_unittest("value47", test_small_integers_value47), &
                    new_unittest("value48", test_small_integers_value48), &
                    new_unittest("value49", test_small_integers_value49), &
                    new_unittest("value50", test_small_integers_value50), &
                    new_unittest("value51", test_small_integers_value51), &
                    new_unittest("value52", test_small_integers_value52), &
                    new_unittest("value53", test_small_integers_value53), &
                    new_unittest("value54", test_small_integers_value54), &
                    new_unittest("value55", test_small_integers_value55), &
                    new_unittest("value56", test_small_integers_value56), &
                    new_unittest("value57", test_small_integers_value57), &
                    new_unittest("value58", test_small_integers_value58), &
                    new_unittest("value59", test_small_integers_value59), &
                    new_unittest("value60", test_small_integers_value60), &
                    new_unittest("value61", test_small_integers_value61), &
                    new_unittest("value62", test_small_integers_value62), &
                    new_unittest("value63", test_small_integers_value63), &
                    new_unittest("value64", test_small_integers_value64), &
                    new_unittest("value65", test_small_integers_value65), &
                    new_unittest("value66", test_small_integers_value66), &
                    new_unittest("value67", test_small_integers_value67), &
                    new_unittest("value68", test_small_integers_value68), &
                    new_unittest("value69", test_small_integers_value69), &
                    new_unittest("value70", test_small_integers_value70), &
                    new_unittest("value71", test_small_integers_value71), &
                    new_unittest("value72", test_small_integers_value72), &
                    new_unittest("value73", test_small_integers_value73), &
                    new_unittest("value74", test_small_integers_value74), &
                    new_unittest("value75", test_small_integers_value75), &
                    new_unittest("value76", test_small_integers_value76), &
                    new_unittest("value77", test_small_integers_value77), &
                    new_unittest("value78", test_small_integers_value78), &
                    new_unittest("value79", test_small_integers_value79) &
                    ]

    end subroutine collect_small_integers

    subroutine test_small_integers_value1(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(9007199254740991.0_real64), "9.007199254740991E15")
        write (*, "(A,4X,A)") d2shortest(9007199254740991.0_real64), "9.007199254740991E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value1

    subroutine test_small_integers_value2(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(9007199254740992.0_real64), "9.007199254740992E15")
        write (*, "(A,4X,A)") d2shortest(9007199254740992.0_real64), "9.007199254740992E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value2

    subroutine test_small_integers_value3(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+0_real64), "1.0")
        write (*, "(A,4X,A)") d2shortest(1.0e+0_real64), "1.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value3

    subroutine test_small_integers_value4(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.2e+1_real64), "12.0")
        write (*, "(A,4X,A)") d2shortest(1.2e+1_real64), "12.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value4

    subroutine test_small_integers_value5(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.23e+2_real64), "123.0")
        write (*, "(A,4X,A)") d2shortest(1.23e+2_real64), "123.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value5

    subroutine test_small_integers_value6(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.234e+3_real64), "1234.0")
        write (*, "(A,4X,A)") d2shortest(1.234e+3_real64), "1234.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value6

    subroutine test_small_integers_value7(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.2345e+4_real64), "12345.0")
        write (*, "(A,4X,A)") d2shortest(1.2345e+4_real64), "12345.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value7

    subroutine test_small_integers_value8(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.23456e+5_real64), "123456.0")
        write (*, "(A,4X,A)") d2shortest(1.23456e+5_real64), "123456.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value8

    subroutine test_small_integers_value9(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.234567e+6_real64), "1234567.0")
        write (*, "(A,4X,A)") d2shortest(1.234567e+6_real64), "1234567.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value9

    subroutine test_small_integers_value10(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.2345678e+7_real64), "1.2345678E7")
        write (*, "(A,4X,A)") d2shortest(1.2345678e+7_real64), "1.2345678E7"
        if (allocated(error)) return
    end subroutine test_small_integers_value10

    subroutine test_small_integers_value11(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.23456789e+8_real64), "1.23456789E8")
        write (*, "(A,4X,A)") d2shortest(1.23456789e+8_real64), "1.23456789E8"
        if (allocated(error)) return
    end subroutine test_small_integers_value11

    subroutine test_small_integers_value12(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.23456789e+9_real64), "1.23456789E9")
        write (*, "(A,4X,A)") d2shortest(1.23456789e+9_real64), "1.23456789E9"
        if (allocated(error)) return
    end subroutine test_small_integers_value12

    subroutine test_small_integers_value13(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.234567895e+9_real64), "1.234567895E9")
        write (*, "(A,4X,A)") d2shortest(1.234567895e+9_real64), "1.234567895E9"
        if (allocated(error)) return
    end subroutine test_small_integers_value13

    subroutine test_small_integers_value14(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.2345678901e+10_real64), "1.2345678901E10")
        write (*, "(A,4X,A)") d2shortest(1.2345678901e+10_real64), "1.2345678901E10"
        if (allocated(error)) return
    end subroutine test_small_integers_value14

    subroutine test_small_integers_value15(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.23456789012e+11_real64), "1.23456789012E11")
        write (*, "(A,4X,A)") d2shortest(1.23456789012e+11_real64), "1.23456789012E11"
        if (allocated(error)) return
    end subroutine test_small_integers_value15

    subroutine test_small_integers_value16(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.234567890123e+12_real64), "1.234567890123E12")
        write (*, "(A,4X,A)") d2shortest(1.234567890123e+12_real64), "1.234567890123E12"
        if (allocated(error)) return
    end subroutine test_small_integers_value16

    subroutine test_small_integers_value17(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.2345678901234e+13_real64), "1.2345678901234E13")
        write (*, "(A,4X,A)") d2shortest(1.2345678901234e+13_real64), "1.2345678901234E13"
        if (allocated(error)) return
    end subroutine test_small_integers_value17

    subroutine test_small_integers_value18(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.23456789012345e+14_real64), "1.23456789012345E14")
        write (*, "(A,4X,A)") d2shortest(1.23456789012345e+14_real64), "1.23456789012345E14"
        if (allocated(error)) return
    end subroutine test_small_integers_value18

    subroutine test_small_integers_value19(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.234567890123456e+15_real64), "1.234567890123456E15")
        write (*, "(A,4X,A)") d2shortest(1.234567890123456e+15_real64), "1.234567890123456E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value19

    subroutine test_small_integers_value20(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+0_real64), "1.0")
        write (*, "(A,4X,A)") d2shortest(1.0e+0_real64), "1.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value20

    subroutine test_small_integers_value21(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+1_real64), "10.0")
        write (*, "(A,4X,A)") d2shortest(1.0e+1_real64), "10.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value21

    subroutine test_small_integers_value22(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+2_real64), "100.0")
        write (*, "(A,4X,A)") d2shortest(1.0e+2_real64), "100.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value22

    subroutine test_small_integers_value23(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+3_real64), "1000.0")
        write (*, "(A,4X,A)") d2shortest(1.0e+3_real64), "1000.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value23

    subroutine test_small_integers_value24(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+4_real64), "10000.0")
        write (*, "(A,4X,A)") d2shortest(1.0e+4_real64), "10000.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value24

    subroutine test_small_integers_value25(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+5_real64), "100000.0")
        write (*, "(A,4X,A)") d2shortest(1.0e+5_real64), "100000.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value25

    subroutine test_small_integers_value26(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+6_real64), "1000000.0")
        write (*, "(A,4X,A)") d2shortest(1.0e+6_real64), "1000000.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value26

    subroutine test_small_integers_value27(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+7_real64), "1.0E7")
        write (*, "(A,4X,A)") d2shortest(1.0e+7_real64), "1E7"
        if (allocated(error)) return
    end subroutine test_small_integers_value27

    subroutine test_small_integers_value28(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+8_real64), "1.0E8")
        write (*, "(A,4X,A)") d2shortest(1.0e+8_real64), "1E8"
        if (allocated(error)) return
    end subroutine test_small_integers_value28

    subroutine test_small_integers_value29(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+9_real64), "1.0E9")
        write (*, "(A,4X,A)") d2shortest(1.0e+9_real64), "1.0E9"
        if (allocated(error)) return
    end subroutine test_small_integers_value29

    subroutine test_small_integers_value30(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+10_real64), "1.0E10")
        write (*, "(A,4X,A)") d2shortest(1.0e+10_real64), "1.0E10"
        if (allocated(error)) return
    end subroutine test_small_integers_value30

    subroutine test_small_integers_value31(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+11_real64), "1.0E11")
        write (*, "(A,4X,A)") d2shortest(1.0e+11_real64), "1.0E11"
        if (allocated(error)) return
    end subroutine test_small_integers_value31

    subroutine test_small_integers_value32(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+12_real64), "1.0E12")
        write (*, "(A,4X,A)") d2shortest(1.0e+12_real64), "1.0E12"
        if (allocated(error)) return
    end subroutine test_small_integers_value32

    subroutine test_small_integers_value33(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+13_real64), "1.0E13")
        write (*, "(A,4X,A)") d2shortest(1.0e+13_real64), "1.0E13"
        if (allocated(error)) return
    end subroutine test_small_integers_value33

    subroutine test_small_integers_value34(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+14_real64), "1.0E14")
        write (*, "(A,4X,A)") d2shortest(1.0e+14_real64), "1.0E14"
        if (allocated(error)) return
    end subroutine test_small_integers_value34

    subroutine test_small_integers_value35(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+15_real64), "1.0E15")
        write (*, "(A,4X,A)") d2shortest(1.0e+15_real64), "1.0E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value35

    subroutine test_small_integers_value36(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+15_real64 + 1.0e+0_real64), "1.000000000000001E15")
        write (*, "(A,4X,A)") d2shortest(1.0e+15_real64 + 1.0e+0_real64), "1.000000000000001E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value36

    subroutine test_small_integers_value37(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+15_real64 + 1.0e+1_real64), "1.00000000000001E15")
        write (*, "(A,4X,A)") d2shortest(1.0e+15_real64 + 1.0e+1_real64), "1.00000000000001E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value37

    subroutine test_small_integers_value38(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+15_real64 + 1.0e+2_real64), "1.0000000000001E15")
        write (*, "(A,4X,A)") d2shortest(1.0e+15_real64 + 1.0e+2_real64), "1.0000000000001E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value38

    subroutine test_small_integers_value39(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+15_real64 + 1.0e+3_real64), "1.000000000001E15")
        write (*, "(A,4X,A)") d2shortest(1.0e+15_real64 + 1.0e+3_real64), "1.000000000001E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value39

    subroutine test_small_integers_value40(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+15_real64 + 1.0e+4_real64), "1.00000000001E15")
        write (*, "(A,4X,A)") d2shortest(1.0e+15_real64 + 1.0e+4_real64), "1.00000000001E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value40

    subroutine test_small_integers_value41(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+15_real64 + 1.0e+5_real64), "1.0000000001E15")
        write (*, "(A,4X,A)") d2shortest(1.0e+15_real64 + 1.0e+5_real64), "1.0000000001E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value41

    subroutine test_small_integers_value42(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+15_real64 + 1.0e+6_real64), "1.000000001E15")
        write (*, "(A,4X,A)") d2shortest(1.0e+15_real64 + 1.0e+6_real64), "1.000000001E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value42

    subroutine test_small_integers_value43(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+15_real64 + 1.0e+7_real64), "1.00000001E15")
        write (*, "(A,4X,A)") d2shortest(1.0e+15_real64 + 1.0e+7_real64), "1.00000001E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value43

    subroutine test_small_integers_value44(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+15_real64 + 1.0e+8_real64), "1.0000001E15")
        write (*, "(A,4X,A)") d2shortest(1.0e+15_real64 + 1.0e+8_real64), "1.0000001E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value44

    subroutine test_small_integers_value45(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+15_real64 + 1.0e+9_real64), "1.000001E15")
        write (*, "(A,4X,A)") d2shortest(1.0e+15_real64 + 1.0e+9_real64), "1.000001E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value45

    subroutine test_small_integers_value46(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+15_real64 + 1.0e+10_real64), "1.00001E15")
        write (*, "(A,4X,A)") d2shortest(1.0e+15_real64 + 1.0e+10_real64), "1.00001E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value46

    subroutine test_small_integers_value47(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+15_real64 + 1.0e+11_real64), "1.0001E15")
        write (*, "(A,4X,A)") d2shortest(1.0e+15_real64 + 1.0e+11_real64), "1.0001E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value47

    subroutine test_small_integers_value48(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+15_real64 + 1.0e+12_real64), "1.001E15")
        write (*, "(A,4X,A)") d2shortest(1.0e+15_real64 + 1.0e+12_real64), "1.001E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value48

    subroutine test_small_integers_value49(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+15_real64 + 1.0e+13_real64), "1.01E15")
        write (*, "(A,4X,A)") d2shortest(1.0e+15_real64 + 1.0e+13_real64), "1.01E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value49

    subroutine test_small_integers_value50(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(1.0e+15_real64 + 1.0e+14_real64), "1.1E15")
        write (*, "(A,4X,A)") d2shortest(1.0e+15_real64 + 1.0e+14_real64), "1.1E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value50

    subroutine test_small_integers_value51(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(8.0_real64), "8.0")
        write (*, "(A,4X,A)") d2shortest(8.0_real64), "8.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value51

    subroutine test_small_integers_value52(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(64.0_real64), "64.0")
        write (*, "(A,4X,A)") d2shortest(64.0_real64), "64.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value52

    subroutine test_small_integers_value53(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(512.0_real64), "512.0")
        write (*, "(A,4X,A)") d2shortest(512.0_real64), "512.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value53

    subroutine test_small_integers_value54(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(8192.0_real64), "8192.0")
        write (*, "(A,4X,A)") d2shortest(8192.0_real64), "8192.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value54

    subroutine test_small_integers_value55(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(65536.0_real64), "65536.0")
        write (*, "(A,4X,A)") d2shortest(65536.0_real64), "65536.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value55

    subroutine test_small_integers_value56(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(524288.0_real64), "524288.0")
        write (*, "(A,4X,A)") d2shortest(524288.0_real64), "524288.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value56

    subroutine test_small_integers_value57(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(8388608.0_real64), "8388608.0")
        write (*, "(A,4X,A)") d2shortest(8388608.0_real64), "8388608.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value57

    subroutine test_small_integers_value58(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(67108864.0_real64), "6.7108864E7")
        write (*, "(A,4X,A)") d2shortest(67108864.0_real64), "6.7108864E7"
        if (allocated(error)) return
    end subroutine test_small_integers_value58

    subroutine test_small_integers_value59(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(536870912.0_real64), "5.36870912E8")
        write (*, "(A,4X,A)") d2shortest(536870912.0_real64), "5.36870912E8"
        if (allocated(error)) return
    end subroutine test_small_integers_value59

    subroutine test_small_integers_value60(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(8589934592.0_real64), "8.589934592E9")
        write (*, "(A,4X,A)") d2shortest(8589934592.0_real64), "8.589934592E9"
        if (allocated(error)) return
    end subroutine test_small_integers_value60

    subroutine test_small_integers_value61(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(68719476736.0_real64), "6.8719476736E10")
        write (*, "(A,4X,A)") d2shortest(68719476736.0_real64), "6.8719476736E10"
        if (allocated(error)) return
    end subroutine test_small_integers_value61

    subroutine test_small_integers_value62(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(549755813888.0_real64), "5.49755813888E11")
        write (*, "(A,4X,A)") d2shortest(549755813888.0_real64), "5.49755813888E11"
        if (allocated(error)) return
    end subroutine test_small_integers_value62

    subroutine test_small_integers_value63(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(8796093022208.0_real64), "8.796093022208E12")
        write (*, "(A,4X,A)") d2shortest(8796093022208.0_real64), "8.796093022208E12"
        if (allocated(error)) return
    end subroutine test_small_integers_value63

    subroutine test_small_integers_value64(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(70368744177664.0_real64), "7.0368744177664E13")
        write (*, "(A,4X,A)") d2shortest(70368744177664.0_real64), "7.0368744177664E13"
        if (allocated(error)) return
    end subroutine test_small_integers_value64

    subroutine test_small_integers_value65(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(562949953421312.0_real64), "5.62949953421312E14")
        write (*, "(A,4X,A)") d2shortest(562949953421312.0_real64), "5.62949953421312E14"
        if (allocated(error)) return
    end subroutine test_small_integers_value65

    subroutine test_small_integers_value66(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(9007199254740992.0_real64), "9.007199254740992E15")
        write (*, "(A,4X,A)") d2shortest(9007199254740992.0_real64), "9.007199254740992E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value66

    subroutine test_small_integers_value67(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(8.0e+3_real64), "8000.0")
        write (*, "(A,4X,A)") d2shortest(8.0e+3_real64), "8000.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value67

    subroutine test_small_integers_value68(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(64.0e+3_real64), "64000.0")
        write (*, "(A,4X,A)") d2shortest(64.0e+3_real64), "64000.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value68

    subroutine test_small_integers_value69(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(512.0e+3_real64), "512000.0")
        write (*, "(A,4X,A)") d2shortest(512.0e+3_real64), "512000.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value69

    subroutine test_small_integers_value70(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(8192.0e+3_real64), "8192000.0")
        write (*, "(A,4X,A)") d2shortest(8192.0e+3_real64), "8192000.0"
        if (allocated(error)) return
    end subroutine test_small_integers_value70

    subroutine test_small_integers_value71(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(65536.0e+3_real64), "6.5536E7")
        write (*, "(A,4X,A)") d2shortest(65536.0e+3_real64), "6.5536E7"
        if (allocated(error)) return
    end subroutine test_small_integers_value71

    subroutine test_small_integers_value72(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(524288.0e+3_real64), "5.24288E8")
        write (*, "(A,4X,A)") d2shortest(524288.0e+3_real64), "5.24288E8"
        if (allocated(error)) return
    end subroutine test_small_integers_value72

    subroutine test_small_integers_value73(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(8388608.0e+3_real64), "8.388608E9")
        write (*, "(A,4X,A)") d2shortest(8388608.0e+3_real64), "8.388608E9"
        if (allocated(error)) return
    end subroutine test_small_integers_value73

    subroutine test_small_integers_value74(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(67108864.0e+3_real64), "6.7108864E10")
        write (*, "(A,4X,A)") d2shortest(67108864.0e+3_real64), "6.7108864E10"
        if (allocated(error)) return
    end subroutine test_small_integers_value74

    subroutine test_small_integers_value75(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(536870912.0e+3_real64), "5.36870912E11")
        write (*, "(A,4X,A)") d2shortest(536870912.0e+3_real64), "5.36870912E11"
        if (allocated(error)) return
    end subroutine test_small_integers_value75

    subroutine test_small_integers_value76(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(8589934592.0e+3_real64), "8.589934592E12")
        write (*, "(A,4X,A)") d2shortest(8589934592.0e+3_real64), "8.589934592E12"
        if (allocated(error)) return
    end subroutine test_small_integers_value76

    subroutine test_small_integers_value77(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(68719476736.0e+3_real64), "6.8719476736E13")
        write (*, "(A,4X,A)") d2shortest(68719476736.0e+3_real64), "6.8719476736E13"
        if (allocated(error)) return
    end subroutine test_small_integers_value77

    subroutine test_small_integers_value78(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(549755813888.0e+3_real64), "5.49755813888E14")
        write (*, "(A,4X,A)") d2shortest(549755813888.0e+3_real64), "5.49755813888E14"
        if (allocated(error)) return
    end subroutine test_small_integers_value78

    subroutine test_small_integers_value79(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2shortest(8796093022208.0e+3_real64), "8.796093022208E15")
        write (*, "(A,4X,A)") d2shortest(8796093022208.0e+3_real64), "8.796093022208E15"
        if (allocated(error)) return
    end subroutine test_small_integers_value79

end module test_d2shortest
