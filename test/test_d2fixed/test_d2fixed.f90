module test_d2fixed
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use real64_to_fixed
    use iso_fortran_env, only: int32, int64, real64
    implicit none
    private
    public :: collect_basic, collect_zeros, collect_max_and_min, collect_round_to_even
    public :: collect_round_to_even_integer, collect_non_round_to_even_scenarios
    public :: collect_varying_precision, collect_carrying, collect_regression
    public :: collect_rounding_result_zero

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
                    new_unittest("value1", test_basic_value1) &
                    ]

    end subroutine collect_basic

    subroutine test_basic_value1(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(ieee_part_to_real64(.false., 1234, 99999_int64), 0), &
                   "3291009114715486435425664845573426149758869524108446525879746560")
        if (allocated(error)) return

    end subroutine test_basic_value1

!> =============================================================================

    subroutine collect_zeros(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("value1", test_zeros_value1), &
                    new_unittest("value2", test_zeros_value2), &
                    new_unittest("value3", test_zeros_value3), &
                    new_unittest("value4", test_zeros_value4), &
                    new_unittest("value5", test_zeros_value5) &
                    ]

    end subroutine collect_zeros

    subroutine test_zeros_value1(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(0.0_real64, 4), "0.0000")
        if (allocated(error)) return

    end subroutine test_zeros_value1

    subroutine test_zeros_value2(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(0.0_real64, 3), "0.000")
        if (allocated(error)) return

    end subroutine test_zeros_value2

    subroutine test_zeros_value3(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(0.0_real64, 2), "0.00")
        if (allocated(error)) return

    end subroutine test_zeros_value3

    subroutine test_zeros_value4(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(0.0_real64, 1), "0.0")
        if (allocated(error)) return

    end subroutine test_zeros_value4

    subroutine test_zeros_value5(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(0.0_real64, 0), "0")
        if (allocated(error)) return

    end subroutine test_zeros_value5

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

        call check(error, d2fixed(max, 0), &
                   "179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558"// &
                   "632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245"// &
                   "490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168"// &
                   "738177180919299881250404026184124858368")
        if (allocated(error)) return

    end subroutine test_max

    subroutine test_min(error)
        type(error_type), allocatable, intent(out) :: error
        real(kind=real64), parameter :: min = transfer(1_int64, 1._real64)

        call check(error, d2fixed(min, 1074), &
                   "0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"// &
                   "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"// &
                   "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"// &
                   "000000000000000000000000000000000000000000000000000000049406564584124654417656879286822137"// &
                   "236505980261432476442558568250067550727020875186529983636163599237979656469544571773092665"// &
                   "671035593979639877479601078187812630071319031140452784581716784898210368871863605699873072"// &
                   "305000638740915356498438731247339727316961514003171538539807412623856559117102665855668676"// &
                   "818703956031062493194527159149245532930545654440112748012970999954193198940908041656332452"// &
                   "475714786901472678015935523861155013480352649347201937902681071074917033322268447533357208"// &
                   "324319360923828934583680601060115061698097530783422773183292479049825247307763759272478746"// &
                   "560847782037344696995336470179726777175851256605511991315048911014510378627381672509558373"// &
                   "89733598993664809941164205702637090279242767544565229087538682506419718265533447265625")
        if (allocated(error)) return

    end subroutine test_min

!> =============================================================================

    subroutine collect_round_to_even(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("value1", test_round_to_even_value1), &
                    new_unittest("value2", test_round_to_even_value2), &
                    new_unittest("value3", test_round_to_even_value3), &
                    new_unittest("value4", test_round_to_even_value4) &
                    ]

    end subroutine collect_round_to_even

    subroutine test_round_to_even_value1(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(0.125_real64, 3), "0.125")
        if (allocated(error)) return

    end subroutine test_round_to_even_value1

    subroutine test_round_to_even_value2(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(0.125_real64, 2), "0.12")
        if (allocated(error)) return

    end subroutine test_round_to_even_value2

    subroutine test_round_to_even_value3(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(0.375_real64, 3), "0.375")
        if (allocated(error)) return

    end subroutine test_round_to_even_value3

    subroutine test_round_to_even_value4(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(0.375_real64, 2), "0.38")
        if (allocated(error)) return

    end subroutine test_round_to_even_value4

!> =============================================================================

    subroutine collect_round_to_even_integer(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("value1", test_round_to_even_integer_value1), &
                    new_unittest("value2", test_round_to_even_integer_value2), &
                    new_unittest("value3", test_round_to_even_integer_value3), &
                    new_unittest("value4", test_round_to_even_integer_value4) &
                    ]

    end subroutine collect_round_to_even_integer

    subroutine test_round_to_even_integer_value1(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(2.5_real64, 1), "2.5")
        if (allocated(error)) return

    end subroutine test_round_to_even_integer_value1

    subroutine test_round_to_even_integer_value2(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(2.5_real64, 0), "2")
        if (allocated(error)) return

    end subroutine test_round_to_even_integer_value2

    subroutine test_round_to_even_integer_value3(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(3.5_real64, 1), "3.5")
        if (allocated(error)) return

    end subroutine test_round_to_even_integer_value3

    subroutine test_round_to_even_integer_value4(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(3.5_real64, 0), "4")
        if (allocated(error)) return

    end subroutine test_round_to_even_integer_value4

!> =============================================================================

    subroutine collect_non_round_to_even_scenarios(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("value1", test_non_round_to_even_scenarios_value1), &
                    new_unittest("value2", test_non_round_to_even_scenarios_value2), &
                    new_unittest("value3", test_non_round_to_even_scenarios_value3), &
                    new_unittest("value4", test_non_round_to_even_scenarios_value4), &
                    new_unittest("value5", test_non_round_to_even_scenarios_value5), &
                    new_unittest("value6", test_non_round_to_even_scenarios_value6), &
                    new_unittest("value7", test_non_round_to_even_scenarios_value7), &
                    new_unittest("value8", test_non_round_to_even_scenarios_value8), &
                    new_unittest("value9", test_non_round_to_even_scenarios_value9), &
                    new_unittest("value10", test_non_round_to_even_scenarios_value10) &
                    ]

    end subroutine collect_non_round_to_even_scenarios

    subroutine test_non_round_to_even_scenarios_value1(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(0.748046875_real64, 3), "0.748")
        if (allocated(error)) return

    end subroutine test_non_round_to_even_scenarios_value1

    subroutine test_non_round_to_even_scenarios_value2(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(0.748046875_real64, 2), "0.75")
        if (allocated(error)) return

    end subroutine test_non_round_to_even_scenarios_value2

    subroutine test_non_round_to_even_scenarios_value3(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(0.748046875_real64, 1), "0.7")
        if (allocated(error)) return

    end subroutine test_non_round_to_even_scenarios_value3

    subroutine test_non_round_to_even_scenarios_value4(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(0.2509765625_real64, 3), "0.251")
        if (allocated(error)) return

    end subroutine test_non_round_to_even_scenarios_value4

    subroutine test_non_round_to_even_scenarios_value5(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(0.2509765625_real64, 2), "0.25")
        if (allocated(error)) return

    end subroutine test_non_round_to_even_scenarios_value5

    subroutine test_non_round_to_even_scenarios_value6(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(0.2509765625_real64, 1), "0.3")
        if (allocated(error)) return

    end subroutine test_non_round_to_even_scenarios_value6

    subroutine test_non_round_to_even_scenarios_value7(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(ieee_part_to_real64(.false., 1021, 1_int64), 54), &
                   "0.250000000000000055511151231257827021181583404541015625")
        if (allocated(error)) return

    end subroutine test_non_round_to_even_scenarios_value7

    subroutine test_non_round_to_even_scenarios_value8(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(ieee_part_to_real64(.false., 1021, 1_int64), 3), "0.250")
        if (allocated(error)) return

    end subroutine test_non_round_to_even_scenarios_value8

    subroutine test_non_round_to_even_scenarios_value9(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(ieee_part_to_real64(.false., 1021, 1_int64), 2), "0.25")
        if (allocated(error)) return

    end subroutine test_non_round_to_even_scenarios_value9

    subroutine test_non_round_to_even_scenarios_value10(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(ieee_part_to_real64(.false., 1021, 1_int64), 1), "0.3")
        if (allocated(error)) return

    end subroutine test_non_round_to_even_scenarios_value10

!> =============================================================================

    subroutine collect_varying_precision(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("value1", test_varying_precision_value1), &
                    new_unittest("value2", test_varying_precision_value2), &
                    new_unittest("value3", test_varying_precision_value3), &
                    new_unittest("value4", test_varying_precision_value4), &
                    new_unittest("value5", test_varying_precision_value5), &
                    new_unittest("value6", test_varying_precision_value6), &
                    new_unittest("value7", test_varying_precision_value7), &
                    new_unittest("value8", test_varying_precision_value8), &
                    new_unittest("value9", test_varying_precision_value9), &
                    new_unittest("value10", test_varying_precision_value10), &
                    new_unittest("value11", test_varying_precision_value11), &
                    new_unittest("value12", test_varying_precision_value12), &
                    new_unittest("value13", test_varying_precision_value13), &
                    new_unittest("value14", test_varying_precision_value14), &
                    new_unittest("value15", test_varying_precision_value15), &
                    new_unittest("value16", test_varying_precision_value16), &
                    new_unittest("value17", test_varying_precision_value17), &
                    new_unittest("value18", test_varying_precision_value18), &
                    new_unittest("value19", test_varying_precision_value19), &
                    new_unittest("value20", test_varying_precision_value20), &
                    new_unittest("value21", test_varying_precision_value21), &
                    new_unittest("value22", test_varying_precision_value22), &
                    new_unittest("value23", test_varying_precision_value23), &
                    new_unittest("value24", test_varying_precision_value24), &
                    new_unittest("value25", test_varying_precision_value25), &
                    new_unittest("value26", test_varying_precision_value26), &
                    new_unittest("value27", test_varying_precision_value27), &
                    new_unittest("value28", test_varying_precision_value28), &
                    new_unittest("value29", test_varying_precision_value29), &
                    new_unittest("value30", test_varying_precision_value30), &
                    new_unittest("value31", test_varying_precision_value31), &
                    new_unittest("value32", test_varying_precision_value32), &
                    new_unittest("value33", test_varying_precision_value33), &
                    new_unittest("value34", test_varying_precision_value34), &
                    new_unittest("value35", test_varying_precision_value35), &
                    new_unittest("value36", test_varying_precision_value36), &
                    new_unittest("value37", test_varying_precision_value37), &
                    new_unittest("value38", test_varying_precision_value38), &
                    new_unittest("value39", test_varying_precision_value39), &
                    new_unittest("value40", test_varying_precision_value40), &
                    new_unittest("value41", test_varying_precision_value41), &
                    new_unittest("value42", test_varying_precision_value42), &
                    new_unittest("value43", test_varying_precision_value43), &
                    new_unittest("value44", test_varying_precision_value44), &
                    new_unittest("value45", test_varying_precision_value45), &
                    new_unittest("value46", test_varying_precision_value46), &
                    new_unittest("value47", test_varying_precision_value47), &
                    new_unittest("value48", test_varying_precision_value48) &
                    ]

    end subroutine collect_varying_precision

    subroutine test_varying_precision_value1(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 47), &
                   "1729.14285714285711037518922239542007446289062500000")
        if (allocated(error)) return
    end subroutine test_varying_precision_value1

    subroutine test_varying_precision_value2(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 46), &
                   "1729.1428571428571103751892223954200744628906250000")
        if (allocated(error)) return
    end subroutine test_varying_precision_value2

    subroutine test_varying_precision_value3(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 45), &
                   "1729.142857142857110375189222395420074462890625000")
        if (allocated(error)) return
    end subroutine test_varying_precision_value3

    subroutine test_varying_precision_value4(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 44), &
                   "1729.14285714285711037518922239542007446289062500")
        if (allocated(error)) return
    end subroutine test_varying_precision_value4

    subroutine test_varying_precision_value5(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 43), &
                   "1729.1428571428571103751892223954200744628906250")
        if (allocated(error)) return
    end subroutine test_varying_precision_value5

    subroutine test_varying_precision_value6(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 42), &
                   "1729.142857142857110375189222395420074462890625")
        if (allocated(error)) return
    end subroutine test_varying_precision_value6

    subroutine test_varying_precision_value7(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 41), "1729.14285714285711037518922239542007446289062")
        if (allocated(error)) return
    end subroutine test_varying_precision_value7

    subroutine test_varying_precision_value8(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 40), "1729.1428571428571103751892223954200744628906")
        if (allocated(error)) return
    end subroutine test_varying_precision_value8

    subroutine test_varying_precision_value9(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 39), "1729.142857142857110375189222395420074462891")
        if (allocated(error)) return
    end subroutine test_varying_precision_value9

    subroutine test_varying_precision_value10(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 38), "1729.14285714285711037518922239542007446289")
        if (allocated(error)) return
    end subroutine test_varying_precision_value10

    subroutine test_varying_precision_value11(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 37), "1729.1428571428571103751892223954200744629")
        if (allocated(error)) return
    end subroutine test_varying_precision_value11

    subroutine test_varying_precision_value12(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 36), "1729.142857142857110375189222395420074463")
        if (allocated(error)) return
    end subroutine test_varying_precision_value12

    subroutine test_varying_precision_value13(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 35), "1729.14285714285711037518922239542007446")
        if (allocated(error)) return
    end subroutine test_varying_precision_value13

    subroutine test_varying_precision_value14(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 34), "1729.1428571428571103751892223954200745")
        if (allocated(error)) return
    end subroutine test_varying_precision_value14

    subroutine test_varying_precision_value15(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 33), "1729.142857142857110375189222395420074")
        if (allocated(error)) return
    end subroutine test_varying_precision_value15

    subroutine test_varying_precision_value16(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 32), "1729.14285714285711037518922239542007")
        if (allocated(error)) return
    end subroutine test_varying_precision_value16

    subroutine test_varying_precision_value17(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 31), "1729.1428571428571103751892223954201")
        if (allocated(error)) return
    end subroutine test_varying_precision_value17

    subroutine test_varying_precision_value18(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 30), "1729.142857142857110375189222395420")
        if (allocated(error)) return
    end subroutine test_varying_precision_value18

    subroutine test_varying_precision_value19(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 29), "1729.14285714285711037518922239542")
        if (allocated(error)) return
    end subroutine test_varying_precision_value19

    subroutine test_varying_precision_value20(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 28), "1729.1428571428571103751892223954")
        if (allocated(error)) return
    end subroutine test_varying_precision_value20

    subroutine test_varying_precision_value21(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 27), "1729.142857142857110375189222395")
        if (allocated(error)) return
    end subroutine test_varying_precision_value21

    subroutine test_varying_precision_value22(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 26), "1729.14285714285711037518922240")
        if (allocated(error)) return
    end subroutine test_varying_precision_value22

    subroutine test_varying_precision_value23(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 25), "1729.1428571428571103751892224")
        if (allocated(error)) return
    end subroutine test_varying_precision_value23

    subroutine test_varying_precision_value24(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 24), "1729.142857142857110375189222")
        if (allocated(error)) return
    end subroutine test_varying_precision_value24

    subroutine test_varying_precision_value25(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 23), "1729.14285714285711037518922")
        if (allocated(error)) return
    end subroutine test_varying_precision_value25

    subroutine test_varying_precision_value26(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 22), "1729.1428571428571103751892")
        if (allocated(error)) return
    end subroutine test_varying_precision_value26

    subroutine test_varying_precision_value27(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 21), "1729.142857142857110375189")
        if (allocated(error)) return
    end subroutine test_varying_precision_value27

    subroutine test_varying_precision_value28(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 20), "1729.14285714285711037519")
        if (allocated(error)) return
    end subroutine test_varying_precision_value28

    subroutine test_varying_precision_value29(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 19), "1729.1428571428571103752")
        if (allocated(error)) return
    end subroutine test_varying_precision_value29

    subroutine test_varying_precision_value30(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 18), "1729.142857142857110375")
        if (allocated(error)) return
    end subroutine test_varying_precision_value30

    subroutine test_varying_precision_value31(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 17), "1729.14285714285711038")
        if (allocated(error)) return
    end subroutine test_varying_precision_value31

    subroutine test_varying_precision_value32(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 16), "1729.1428571428571104")
        if (allocated(error)) return
    end subroutine test_varying_precision_value32

    subroutine test_varying_precision_value33(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 15), "1729.142857142857110")
        if (allocated(error)) return
    end subroutine test_varying_precision_value33

    subroutine test_varying_precision_value34(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 14), "1729.14285714285711")
        if (allocated(error)) return
    end subroutine test_varying_precision_value34

    subroutine test_varying_precision_value35(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 13), "1729.1428571428571")
        if (allocated(error)) return
    end subroutine test_varying_precision_value35

    subroutine test_varying_precision_value36(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 12), "1729.142857142857")
        if (allocated(error)) return
    end subroutine test_varying_precision_value36

    subroutine test_varying_precision_value37(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 11), "1729.14285714286")
        if (allocated(error)) return
    end subroutine test_varying_precision_value37

    subroutine test_varying_precision_value38(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 10), "1729.1428571429")
        if (allocated(error)) return
    end subroutine test_varying_precision_value38

    subroutine test_varying_precision_value39(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 9), "1729.142857143")
        if (allocated(error)) return
    end subroutine test_varying_precision_value39

    subroutine test_varying_precision_value40(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 8), "1729.14285714")
        if (allocated(error)) return
    end subroutine test_varying_precision_value40

    subroutine test_varying_precision_value41(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 7), "1729.1428571")
        if (allocated(error)) return
    end subroutine test_varying_precision_value41

    subroutine test_varying_precision_value42(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 6), "1729.142857")
        if (allocated(error)) return
    end subroutine test_varying_precision_value42

    subroutine test_varying_precision_value43(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 5), "1729.14286")
        if (allocated(error)) return
    end subroutine test_varying_precision_value43

    subroutine test_varying_precision_value44(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 4), "1729.1429")
        if (allocated(error)) return
    end subroutine test_varying_precision_value44

    subroutine test_varying_precision_value45(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 3), "1729.143")
        if (allocated(error)) return
    end subroutine test_varying_precision_value45

    subroutine test_varying_precision_value46(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 2), "1729.14")
        if (allocated(error)) return
    end subroutine test_varying_precision_value46

    subroutine test_varying_precision_value47(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 1), "1729.1")
        if (allocated(error)) return
    end subroutine test_varying_precision_value47

    subroutine test_varying_precision_value48(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(1729.142857142857_real64, 0), "1729")
        if (allocated(error)) return
    end subroutine test_varying_precision_value48

!> =============================================================================

    subroutine collect_carrying(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("value1", test_carrying_value1), &
                    new_unittest("value2", test_carrying_value2), &
                    new_unittest("value3", test_carrying_value3), &
                    new_unittest("value4", test_carrying_value4), &
                    new_unittest("value5", test_carrying_value5), &
                    new_unittest("value6", test_carrying_value6), &
                    new_unittest("value7", test_carrying_value7), &
                    new_unittest("value8", test_carrying_value8), &
                    new_unittest("value9", test_carrying_value9), &
                    new_unittest("value10", test_carrying_value10), &
                    new_unittest("value11", test_carrying_value11), &
                    new_unittest("value12", test_carrying_value12), &
                    new_unittest("value13", test_carrying_value13), &
                    new_unittest("value14", test_carrying_value14), &
                    new_unittest("value15", test_carrying_value15), &
                    new_unittest("value16", test_carrying_value16), &
                    new_unittest("value17", test_carrying_value17), &
                    new_unittest("value18", test_carrying_value18), &
                    new_unittest("value19", test_carrying_value19), &
                    new_unittest("value20", test_carrying_value20), &
                    new_unittest("value21", test_carrying_value21), &
                    new_unittest("value22", test_carrying_value22) &
                    ]

    end subroutine collect_carrying

    subroutine test_carrying_value1(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.0009_real64, 4), "0.0009")
        if (allocated(error)) return
    end subroutine test_carrying_value1

    subroutine test_carrying_value2(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.0009_real64, 3), "0.001")
        if (allocated(error)) return
    end subroutine test_carrying_value2

    subroutine test_carrying_value3(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.0029_real64, 4), "0.0029")
        if (allocated(error)) return
    end subroutine test_carrying_value3

    subroutine test_carrying_value4(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.0029_real64, 3), "0.003")
        if (allocated(error)) return
    end subroutine test_carrying_value4

    subroutine test_carrying_value5(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.0099_real64, 4), "0.0099")
        if (allocated(error)) return
    end subroutine test_carrying_value5

    subroutine test_carrying_value6(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.0099_real64, 3), "0.010")
        if (allocated(error)) return
    end subroutine test_carrying_value6

    subroutine test_carrying_value7(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.0299_real64, 4), "0.0299")
        if (allocated(error)) return
    end subroutine test_carrying_value7

    subroutine test_carrying_value8(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.0299_real64, 3), "0.030")
        if (allocated(error)) return
    end subroutine test_carrying_value8

    subroutine test_carrying_value9(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.0999_real64, 4), "0.0999")
        if (allocated(error)) return
    end subroutine test_carrying_value9

    subroutine test_carrying_value10(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.0999_real64, 3), "0.100")
        if (allocated(error)) return
    end subroutine test_carrying_value10

    subroutine test_carrying_value11(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.2999_real64, 4), "0.2999")
        if (allocated(error)) return
    end subroutine test_carrying_value11

    subroutine test_carrying_value12(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.2999_real64, 3), "0.300")
        if (allocated(error)) return
    end subroutine test_carrying_value12

    subroutine test_carrying_value13(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.9999_real64, 4), "0.9999")
        if (allocated(error)) return
    end subroutine test_carrying_value13

    subroutine test_carrying_value14(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.9999_real64, 3), "1.000")
        if (allocated(error)) return
    end subroutine test_carrying_value14

    subroutine test_carrying_value15(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(2.9999_real64, 4), "2.9999")
        if (allocated(error)) return
    end subroutine test_carrying_value15

    subroutine test_carrying_value16(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(2.9999_real64, 3), "3.000")
        if (allocated(error)) return
    end subroutine test_carrying_value16

    subroutine test_carrying_value17(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(9.9999_real64, 4), "9.9999")
        if (allocated(error)) return
    end subroutine test_carrying_value17

    subroutine test_carrying_value18(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(9.9999_real64, 3), "10.000")
        if (allocated(error)) return
    end subroutine test_carrying_value18

    subroutine test_carrying_value19(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(29.9999_real64, 4), "29.9999")
        if (allocated(error)) return
    end subroutine test_carrying_value19

    subroutine test_carrying_value20(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(29.9999_real64, 3), "30.000")
        if (allocated(error)) return
    end subroutine test_carrying_value20

    subroutine test_carrying_value21(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(99.9999_real64, 4), "99.9999")
        if (allocated(error)) return
    end subroutine test_carrying_value21

    subroutine test_carrying_value22(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(99.9999_real64, 3), "100.000")
        if (allocated(error)) return
    end subroutine test_carrying_value22

    subroutine test_carrying_value23(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(299.9999_real64, 4), "299.9999")
        if (allocated(error)) return
    end subroutine test_carrying_value23

    subroutine test_carrying_value24(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(299.9999_real64, 3), "300.000")
        if (allocated(error)) return
    end subroutine test_carrying_value24

    subroutine test_carrying_value25(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.09_real64, 2), "0.09")
        if (allocated(error)) return
    end subroutine test_carrying_value25

    subroutine test_carrying_value26(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.09_real64, 1), "0.1")
        if (allocated(error)) return
    end subroutine test_carrying_value26

    subroutine test_carrying_value27(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.29_real64, 2), "0.29")
        if (allocated(error)) return
    end subroutine test_carrying_value27

    subroutine test_carrying_value28(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.29_real64, 1), "0.3")
        if (allocated(error)) return
    end subroutine test_carrying_value28

    subroutine test_carrying_value29(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.99_real64, 2), "0.99")
        if (allocated(error)) return
    end subroutine test_carrying_value29

    subroutine test_carrying_value30(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.99_real64, 1), "1.0")
        if (allocated(error)) return
    end subroutine test_carrying_value30

    subroutine test_carrying_value31(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(2.99_real64, 2), "2.99")
        if (allocated(error)) return
    end subroutine test_carrying_value31

    subroutine test_carrying_value32(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(2.99_real64, 1), "3.0")
        if (allocated(error)) return
    end subroutine test_carrying_value32

    subroutine test_carrying_value33(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(9.99_real64, 2), "9.99")
        if (allocated(error)) return
    end subroutine test_carrying_value33

    subroutine test_carrying_value34(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(9.99_real64, 1), "10.0")
        if (allocated(error)) return
    end subroutine test_carrying_value34

    subroutine test_carrying_value35(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(29.99_real64, 2), "29.99")
        if (allocated(error)) return
    end subroutine test_carrying_value35

    subroutine test_carrying_value36(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(29.99_real64, 1), "30.0")
        if (allocated(error)) return
    end subroutine test_carrying_value36

    subroutine test_carrying_value37(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(99.99_real64, 2), "99.99")
        if (allocated(error)) return
    end subroutine test_carrying_value37

    subroutine test_carrying_value38(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(99.99_real64, 1), "100.0")
        if (allocated(error)) return
    end subroutine test_carrying_value38

    subroutine test_carrying_value39(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(299.99_real64, 2), "299.99")
        if (allocated(error)) return
    end subroutine test_carrying_value39

    subroutine test_carrying_value40(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(299.99_real64, 1), "300.0")
        if (allocated(error)) return
    end subroutine test_carrying_value40

    subroutine test_carrying_value41(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.9_real64, 1), "0.9")
        if (allocated(error)) return
    end subroutine test_carrying_value41

    subroutine test_carrying_value42(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.9_real64, 0), "1")
        if (allocated(error)) return
    end subroutine test_carrying_value42

    subroutine test_carrying_value43(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(2.9_real64, 1), "2.9")
        if (allocated(error)) return
    end subroutine test_carrying_value43

    subroutine test_carrying_value44(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(2.9_real64, 0), "3")
        if (allocated(error)) return
    end subroutine test_carrying_value44

    subroutine test_carrying_value45(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(9.9_real64, 1), "9.9")
        if (allocated(error)) return
    end subroutine test_carrying_value45

    subroutine test_carrying_value46(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(9.9_real64, 0), "10")
        if (allocated(error)) return
    end subroutine test_carrying_value46

    subroutine test_carrying_value47(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(29.9_real64, 1), "29.9")
        if (allocated(error)) return
    end subroutine test_carrying_value47

    subroutine test_carrying_value48(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(29.9_real64, 0), "30")
        if (allocated(error)) return
    end subroutine test_carrying_value48

!> =============================================================================

    subroutine collect_rounding_result_zero(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("value1", test_rounding_result_zero_value1), &
                    new_unittest("value2", test_rounding_result_zero_value2), &
                    new_unittest("value3", test_rounding_result_zero_value3), &
                    new_unittest("value4", test_rounding_result_zero_value4), &
                    new_unittest("value5", test_rounding_result_zero_value5), &
                    new_unittest("value6", test_rounding_result_zero_value6) &
                    ]

    end subroutine collect_rounding_result_zero

    subroutine test_rounding_result_zero_value1(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.004_real64, 3), "0.004")
        if (allocated(error)) return
    end subroutine test_rounding_result_zero_value1

    subroutine test_rounding_result_zero_value2(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.004_real64, 2), "0.00")
        if (allocated(error)) return
    end subroutine test_rounding_result_zero_value2

    subroutine test_rounding_result_zero_value3(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.4_real64, 1), "0.4")
        if (allocated(error)) return
    end subroutine test_rounding_result_zero_value3

    subroutine test_rounding_result_zero_value4(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.4_real64, 0), "0")
        if (allocated(error)) return
    end subroutine test_rounding_result_zero_value4

    subroutine test_rounding_result_zero_value5(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.5_real64, 1), "0.5")
        if (allocated(error)) return
    end subroutine test_rounding_result_zero_value5

    subroutine test_rounding_result_zero_value6(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2fixed(0.5_real64, 0), "0")
        if (allocated(error)) return
    end subroutine test_rounding_result_zero_value6

!> =============================================================================

    subroutine collect_regression(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("value1", test_regression_value1) &
                    ]

    end subroutine collect_regression

    subroutine test_regression_value1(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2fixed(7.018232e-82_real64, 6), "0.000000")
        if (allocated(error)) return

    end subroutine test_regression_value1

end module test_d2fixed
