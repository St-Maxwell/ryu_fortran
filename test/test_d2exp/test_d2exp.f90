module test_d2exp
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use real64_to_exp
    use iso_fortran_env, only: int32, int64, real64
    implicit none
    private
    public :: collect_basic, collect_zeros, collect_max_and_min, collect_round_to_even
    public :: collect_round_to_even_integer, collect_non_round_to_even_scenarios
    public :: collect_varying_precision, collect_carrying, collect_exponents
    public :: collect_print_decimal_point

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

        call check(error, d2exp(ieee_part_to_real64(.false., 1234, 99999_int64), 62), &
                   "3.29100911471548643542566484557342614975886952410844652587974656E+63")
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

        call check(error, d2exp(0.0_real64, 4), "0.0000E+00")
        if (allocated(error)) return

    end subroutine test_zeros_value1

    subroutine test_zeros_value2(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2exp(0.0_real64, 3), "0.000E+00")
        if (allocated(error)) return

    end subroutine test_zeros_value2

    subroutine test_zeros_value3(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2exp(0.0_real64, 2), "0.00E+00")
        if (allocated(error)) return

    end subroutine test_zeros_value3

    subroutine test_zeros_value4(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2exp(0.0_real64, 1), "0.0E+00")
        if (allocated(error)) return

    end subroutine test_zeros_value4

    subroutine test_zeros_value5(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2exp(0.0_real64, 0), "0E+00")
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

        call check(error, d2exp(max, 308), &
                   "1.7976931348623157081452742373170435679807056752584499659891747680315726078002853876058955"// &
                   "863276687817154045895351438246423432132688946418276846754670353751698604991057655128207624"// &
                   "549009038932894407586850845513394230458323690322294816580855933212334827479782620414472316"// &
                   "8738177180919299881250404026184124858368E+308")
        if (allocated(error)) return

    end subroutine test_max

    subroutine test_min(error)
        type(error_type), allocatable, intent(out) :: error
        real(kind=real64), parameter :: min = transfer(1_int64, 1._real64)

        call check(error, d2exp(min, 750), &
                   "4.9406564584124654417656879286822137236505980261432476442558568250067550727020875186529983"// &
                   "636163599237979656469544571773092665671035593979639877479601078187812630071319031140452784"// &
                   "581716784898210368871863605699873072305000638740915356498438731247339727316961514003171538"// &
                   "539807412623856559117102665855668676818703956031062493194527159149245532930545654440112748"// &
                   "012970999954193198940908041656332452475714786901472678015935523861155013480352649347201937"// &
                   "902681071074917033322268447533357208324319360923828934583680601060115061698097530783422773"// &
                   "183292479049825247307763759272478746560847782037344696995336470179726777175851256605511991"// &
                   "315048911014510378627381672509558373897335989936648099411642057026370902792427675445652290"// &
                   "87538682506419718265533447265625E-324")
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

        call check(error, d2exp(0.125_real64, 2), "1.25E-01")
        if (allocated(error)) return

    end subroutine test_round_to_even_value1

    subroutine test_round_to_even_value2(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2exp(0.125_real64, 1), "1.2E-01")
        if (allocated(error)) return

    end subroutine test_round_to_even_value2

    subroutine test_round_to_even_value3(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2exp(0.375_real64, 2), "3.75E-01")
        if (allocated(error)) return

    end subroutine test_round_to_even_value3

    subroutine test_round_to_even_value4(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2exp(0.375_real64, 1), "3.8E-01")
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

        call check(error, d2exp(2.5_real64, 1), "2.5E+00")
        if (allocated(error)) return

    end subroutine test_round_to_even_integer_value1

    subroutine test_round_to_even_integer_value2(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2exp(2.5_real64, 0), "2E+00")
        if (allocated(error)) return

    end subroutine test_round_to_even_integer_value2

    subroutine test_round_to_even_integer_value3(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2exp(3.5_real64, 1), "3.5E+00")
        if (allocated(error)) return

    end subroutine test_round_to_even_integer_value3

    subroutine test_round_to_even_integer_value4(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2exp(3.5_real64, 0), "4E+00")
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

        call check(error, d2exp(0.748046875_real64, 2), "7.48E-01")
        if (allocated(error)) return

    end subroutine test_non_round_to_even_scenarios_value1

    subroutine test_non_round_to_even_scenarios_value2(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2exp(0.748046875_real64, 1), "7.5E-01")
        if (allocated(error)) return

    end subroutine test_non_round_to_even_scenarios_value2

    subroutine test_non_round_to_even_scenarios_value3(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2exp(0.748046875_real64, 0), "7E-01")
        if (allocated(error)) return

    end subroutine test_non_round_to_even_scenarios_value3

    subroutine test_non_round_to_even_scenarios_value4(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2exp(0.2509765625_real64, 2), "2.51E-01")
        if (allocated(error)) return

    end subroutine test_non_round_to_even_scenarios_value4

    subroutine test_non_round_to_even_scenarios_value5(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2exp(0.2509765625_real64, 1), "2.5E-01")
        if (allocated(error)) return

    end subroutine test_non_round_to_even_scenarios_value5

    subroutine test_non_round_to_even_scenarios_value6(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2exp(0.2509765625_real64, 0), "3E-01")
        if (allocated(error)) return

    end subroutine test_non_round_to_even_scenarios_value6

    subroutine test_non_round_to_even_scenarios_value7(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2exp(ieee_part_to_real64(.false., 1021, 1_int64), 53), &
                   "2.50000000000000055511151231257827021181583404541015625E-01")
        if (allocated(error)) return

    end subroutine test_non_round_to_even_scenarios_value7

    subroutine test_non_round_to_even_scenarios_value8(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2exp(ieee_part_to_real64(.false., 1021, 1_int64), 2), "2.50E-01")
        if (allocated(error)) return

    end subroutine test_non_round_to_even_scenarios_value8

    subroutine test_non_round_to_even_scenarios_value9(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2exp(ieee_part_to_real64(.false., 1021, 1_int64), 1), "2.5E-01")
        if (allocated(error)) return

    end subroutine test_non_round_to_even_scenarios_value9

    subroutine test_non_round_to_even_scenarios_value10(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2exp(ieee_part_to_real64(.false., 1021, 1_int64), 0), "3E-01")
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
                    new_unittest("value48", test_varying_precision_value48), &
                    new_unittest("value49", test_varying_precision_value49), &
                    new_unittest("value50", test_varying_precision_value50), &
                    new_unittest("value51", test_varying_precision_value51) &
                    ]

    end subroutine collect_varying_precision

    subroutine test_varying_precision_value1(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 50), &
                   "1.72914285714285711037518922239542007446289062500000E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value1

    subroutine test_varying_precision_value2(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 49), &
                   "1.7291428571428571103751892223954200744628906250000E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value2

    subroutine test_varying_precision_value3(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 48), &
                   "1.729142857142857110375189222395420074462890625000E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value3

    subroutine test_varying_precision_value4(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 47), "1.72914285714285711037518922239542007446289062500E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value4

    subroutine test_varying_precision_value5(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 46), "1.7291428571428571103751892223954200744628906250E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value5

    subroutine test_varying_precision_value6(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 45), "1.729142857142857110375189222395420074462890625E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value6

    subroutine test_varying_precision_value7(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 44), "1.72914285714285711037518922239542007446289062E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value7

    subroutine test_varying_precision_value8(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 43), "1.7291428571428571103751892223954200744628906E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value8

    subroutine test_varying_precision_value9(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 42), "1.729142857142857110375189222395420074462891E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value9

    subroutine test_varying_precision_value10(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 41), "1.72914285714285711037518922239542007446289E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value10

    subroutine test_varying_precision_value11(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 40), "1.7291428571428571103751892223954200744629E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value11

    subroutine test_varying_precision_value12(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 39), "1.729142857142857110375189222395420074463E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value12

    subroutine test_varying_precision_value13(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 38), "1.72914285714285711037518922239542007446E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value13

    subroutine test_varying_precision_value14(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 37), "1.7291428571428571103751892223954200745E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value14

    subroutine test_varying_precision_value15(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 36), "1.729142857142857110375189222395420074E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value15

    subroutine test_varying_precision_value16(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 35), "1.72914285714285711037518922239542007E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value16

    subroutine test_varying_precision_value17(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 34), "1.7291428571428571103751892223954201E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value17

    subroutine test_varying_precision_value18(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 33), "1.729142857142857110375189222395420E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value18

    subroutine test_varying_precision_value19(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 32), "1.72914285714285711037518922239542E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value19

    subroutine test_varying_precision_value20(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 31), "1.7291428571428571103751892223954E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value20

    subroutine test_varying_precision_value21(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 30), "1.729142857142857110375189222395E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value21

    subroutine test_varying_precision_value22(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 29), "1.72914285714285711037518922240E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value22

    subroutine test_varying_precision_value23(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 28), "1.7291428571428571103751892224E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value23

    subroutine test_varying_precision_value24(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 27), "1.729142857142857110375189222E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value24

    subroutine test_varying_precision_value25(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 26), "1.72914285714285711037518922E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value25

    subroutine test_varying_precision_value26(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 25), "1.7291428571428571103751892E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value26

    subroutine test_varying_precision_value27(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 24), "1.729142857142857110375189E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value27

    subroutine test_varying_precision_value28(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 23), "1.72914285714285711037519E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value28

    subroutine test_varying_precision_value29(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 22), "1.7291428571428571103752E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value29

    subroutine test_varying_precision_value30(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 21), "1.729142857142857110375E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value30

    subroutine test_varying_precision_value31(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 20), "1.72914285714285711038E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value31

    subroutine test_varying_precision_value32(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 19), "1.7291428571428571104E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value32

    subroutine test_varying_precision_value33(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 18), "1.729142857142857110E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value33

    subroutine test_varying_precision_value34(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 17), "1.72914285714285711E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value34

    subroutine test_varying_precision_value35(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 16), "1.7291428571428571E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value35

    subroutine test_varying_precision_value36(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 15), "1.729142857142857E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value36

    subroutine test_varying_precision_value37(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 14), "1.72914285714286E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value37

    subroutine test_varying_precision_value38(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 13), "1.7291428571429E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value38

    subroutine test_varying_precision_value39(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 12), "1.729142857143E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value39

    subroutine test_varying_precision_value40(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 11), "1.72914285714E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value40

    subroutine test_varying_precision_value41(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 10), "1.7291428571E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value41

    subroutine test_varying_precision_value42(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 9), "1.729142857E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value42

    subroutine test_varying_precision_value43(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 8), "1.72914286E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value43

    subroutine test_varying_precision_value44(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 7), "1.7291429E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value44

    subroutine test_varying_precision_value45(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 6), "1.729143E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value45

    subroutine test_varying_precision_value46(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 5), "1.72914E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value46

    subroutine test_varying_precision_value47(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 4), "1.7291E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value47

    subroutine test_varying_precision_value48(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 3), "1.729E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value48

    subroutine test_varying_precision_value49(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 2), "1.73E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value49

    subroutine test_varying_precision_value50(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 1), "1.7E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value50

    subroutine test_varying_precision_value51(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1729.142857142857_real64, 0), "2E+03")
        if (allocated(error)) return
    end subroutine test_varying_precision_value51

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
                    new_unittest("value22", test_carrying_value22), &
                    new_unittest("value23", test_carrying_value23), &
                    new_unittest("value24", test_carrying_value24), &
                    new_unittest("value25", test_carrying_value25), &
                    new_unittest("value26", test_carrying_value26), &
                    new_unittest("value27", test_carrying_value27), &
                    new_unittest("value28", test_carrying_value28) &
                    ]

    end subroutine collect_carrying

    subroutine test_carrying_value1(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.0009_real64, 4), "2.0009E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value1

    subroutine test_carrying_value2(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.0009_real64, 3), "2.001E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value2

    subroutine test_carrying_value3(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.0029_real64, 4), "2.0029E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value3

    subroutine test_carrying_value4(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.0029_real64, 3), "2.003E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value4

    subroutine test_carrying_value5(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.0099_real64, 4), "2.0099E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value5

    subroutine test_carrying_value6(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.0099_real64, 3), "2.010E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value6

    subroutine test_carrying_value7(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.0299_real64, 4), "2.0299E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value7

    subroutine test_carrying_value8(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.0299_real64, 3), "2.030E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value8

    subroutine test_carrying_value9(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.0999_real64, 4), "2.0999E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value9

    subroutine test_carrying_value10(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.0999_real64, 3), "2.100E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value10

    subroutine test_carrying_value11(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.2999_real64, 4), "2.2999E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value11

    subroutine test_carrying_value12(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.2999_real64, 3), "2.300E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value12

    subroutine test_carrying_value13(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.9999_real64, 4), "2.9999E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value13

    subroutine test_carrying_value14(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.9999_real64, 3), "3.000E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value14

    subroutine test_carrying_value15(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.9999_real64, 4), "9.9999E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value15

    subroutine test_carrying_value16(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.9999_real64, 3), "1.000E+01")
        if (allocated(error)) return
    end subroutine test_carrying_value16

    subroutine test_carrying_value17(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.09_real64, 2), "2.09E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value17

    subroutine test_carrying_value18(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.09_real64, 1), "2.1E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value18

    subroutine test_carrying_value19(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.29_real64, 2), "2.29E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value19

    subroutine test_carrying_value20(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.29_real64, 1), "2.3E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value20

    subroutine test_carrying_value21(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.99_real64, 2), "2.99E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value21

    subroutine test_carrying_value22(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.99_real64, 1), "3.0E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value22

    subroutine test_carrying_value23(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99_real64, 2), "9.99E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value23

    subroutine test_carrying_value24(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99_real64, 1), "1.0E+01")
        if (allocated(error)) return
    end subroutine test_carrying_value24

    subroutine test_carrying_value25(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.9_real64, 1), "2.9E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value25

    subroutine test_carrying_value26(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(2.9_real64, 0), "3E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value26

    subroutine test_carrying_value27(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.9_real64, 1), "9.9E+00")
        if (allocated(error)) return
    end subroutine test_carrying_value27

    subroutine test_carrying_value28(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.9_real64, 0), "1E+01")
        if (allocated(error)) return
    end subroutine test_carrying_value28

!> =============================================================================

    subroutine collect_exponents(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("value1", test_exponents_value1), &
                    new_unittest("value2", test_exponents_value2), &
                    new_unittest("value3", test_exponents_value3), &
                    new_unittest("value4", test_exponents_value4), &
                    new_unittest("value5", test_exponents_value5), &
                    new_unittest("value6", test_exponents_value6), &
                    new_unittest("value7", test_exponents_value7), &
                    new_unittest("value8", test_exponents_value8), &
                    new_unittest("value9", test_exponents_value9), &
                    new_unittest("value10", test_exponents_value10), &
                    new_unittest("value11", test_exponents_value11), &
                    new_unittest("value12", test_exponents_value12), &
                    new_unittest("value13", test_exponents_value13), &
                    new_unittest("value14", test_exponents_value14), &
                    new_unittest("value15", test_exponents_value15), &
                    new_unittest("value16", test_exponents_value16), &
                    new_unittest("value17", test_exponents_value17), &
                    new_unittest("value18", test_exponents_value18), &
                    new_unittest("value19", test_exponents_value19), &
                    new_unittest("value20", test_exponents_value20), &
                    new_unittest("value21", test_exponents_value21), &
                    new_unittest("value22", test_exponents_value22) &
                    ]

    end subroutine collect_exponents

    subroutine test_exponents_value1(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E-100_real64, 2), "9.99E-100")
        if (allocated(error)) return
    end subroutine test_exponents_value1

    subroutine test_exponents_value2(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E-99_real64, 2), "9.99E-99")
        if (allocated(error)) return
    end subroutine test_exponents_value2

    subroutine test_exponents_value3(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E-10_real64, 2), "9.99E-10")
        if (allocated(error)) return
    end subroutine test_exponents_value3

    subroutine test_exponents_value4(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E-09_real64, 2), "9.99E-09")
        if (allocated(error)) return
    end subroutine test_exponents_value4

    subroutine test_exponents_value5(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E-01_real64, 2), "9.99E-01")
        if (allocated(error)) return
    end subroutine test_exponents_value5

    subroutine test_exponents_value6(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E+00_real64, 2), "9.99E+00")
        if (allocated(error)) return
    end subroutine test_exponents_value6

    subroutine test_exponents_value7(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E+01_real64, 2), "9.99E+01")
        if (allocated(error)) return
    end subroutine test_exponents_value7

    subroutine test_exponents_value8(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E+09_real64, 2), "9.99E+09")
        if (allocated(error)) return
    end subroutine test_exponents_value8

    subroutine test_exponents_value9(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E+10_real64, 2), "9.99E+10")
        if (allocated(error)) return
    end subroutine test_exponents_value9

    subroutine test_exponents_value10(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E+99_real64, 2), "9.99E+99")
        if (allocated(error)) return
    end subroutine test_exponents_value10

    subroutine test_exponents_value11(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E+100_real64, 2), "9.99E+100")
        if (allocated(error)) return
    end subroutine test_exponents_value11

    subroutine test_exponents_value12(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E-100_real64, 1), "1.0E-99")
        if (allocated(error)) return
    end subroutine test_exponents_value12

    subroutine test_exponents_value13(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E-99_real64, 1), "1.0E-98")
        if (allocated(error)) return
    end subroutine test_exponents_value13

    subroutine test_exponents_value14(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E-10_real64, 1), "1.0E-09")
        if (allocated(error)) return
    end subroutine test_exponents_value14

    subroutine test_exponents_value15(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E-09_real64, 1), "1.0E-08")
        if (allocated(error)) return
    end subroutine test_exponents_value15

    subroutine test_exponents_value16(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E-01_real64, 1), "1.0E+00")
        if (allocated(error)) return
    end subroutine test_exponents_value16

    subroutine test_exponents_value17(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E+00_real64, 1), "1.0E+01")
        if (allocated(error)) return
    end subroutine test_exponents_value17

    subroutine test_exponents_value18(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E+01_real64, 1), "1.0E+02")
        if (allocated(error)) return
    end subroutine test_exponents_value18

    subroutine test_exponents_value19(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E+09_real64, 1), "1.0E+10")
        if (allocated(error)) return
    end subroutine test_exponents_value19

    subroutine test_exponents_value20(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E+10_real64, 1), "1.0E+11")
        if (allocated(error)) return
    end subroutine test_exponents_value20

    subroutine test_exponents_value21(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E+99_real64, 1), "1.0E+100")
        if (allocated(error)) return
    end subroutine test_exponents_value21

    subroutine test_exponents_value22(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(9.99E+100_real64, 1), "1.0E+101")
        if (allocated(error)) return
    end subroutine test_exponents_value22

!> =============================================================================

    subroutine collect_print_decimal_point(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("value1", test_print_decimal_point_value1), &
                    new_unittest("value2", test_print_decimal_point_value2), &
                    new_unittest("value3", test_print_decimal_point_value3), &
                    new_unittest("value4", test_print_decimal_point_value4), &
                    new_unittest("value5", test_print_decimal_point_value5), &
                    new_unittest("value6", test_print_decimal_point_value6) &
                    ]

    end subroutine collect_print_decimal_point

    subroutine test_print_decimal_point_value1(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1e+54_real64, 0), "1E+54")
        if (allocated(error)) return
    end subroutine test_print_decimal_point_value1
    
    subroutine test_print_decimal_point_value2(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1e+54_real64, 1), "1.0E+54")
        if (allocated(error)) return
    end subroutine test_print_decimal_point_value2
    
    subroutine test_print_decimal_point_value3(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1e-63_real64, 0), "1E-63")
        if (allocated(error)) return
    end subroutine test_print_decimal_point_value3
    
    subroutine test_print_decimal_point_value4(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1e-63_real64, 1), "1.0E-63")
        if (allocated(error)) return
    end subroutine test_print_decimal_point_value4
    
    subroutine test_print_decimal_point_value5(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1e+83_real64, 0), "1E+83")
        if (allocated(error)) return
    end subroutine test_print_decimal_point_value5
    
    subroutine test_print_decimal_point_value6(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, d2exp(1e+83_real64, 1), "1.0E+83")
        if (allocated(error)) return
    end subroutine test_print_decimal_point_value6
end module test_d2exp
