module real32_to_shortest
    use ryu_utils, only: FLOAT_MANTISSA_BITS, FLOAT_MANTISSA_MASK, FLOAT_EXPONENT_BITS, &
                         FLOAT_EXPONENT_MASK, FLOAT_EXPONENT_BIAS, &
                         accept_lower_bound, accept_upper_bound
    use iso_fortran_env, only: int32, int64, real32
    use ieee_arithmetic
    implicit none
    private
    public :: f2shortest

    integer(kind=int64), parameter :: LOG10_2_DENOMINATOR = 10000000_int64
    integer(kind=int64), parameter :: LOG10_2_NUMERATOR = int(LOG10_2_DENOMINATOR*log10(2.), int64)
    integer(kind=int64), parameter :: LOG10_5_DENOMINATOR = 10000000_int64
    integer(kind=int64), parameter :: LOG10_5_NUMERATOR = int(LOG10_5_DENOMINATOR*log10(5.), int64)
    integer(kind=int64), parameter :: LOG2_5_DENOMINATOR = 10000000_int64
    integer(kind=int64), parameter :: LOG2_5_NUMERATOR = int(LOG2_5_DENOMINATOR*(log(5.)/log(2.)), int64)
    integer(kind=int32), parameter :: POW5_BITCOUNT = 61_int32
    integer(kind=int32), parameter :: POW5_HALF_BITCOUNT = 31_int32
    integer(kind=int32), parameter :: POW5_INV_BITCOUNT = 59_int32
    integer(kind=int32), parameter :: POW5_INV_HALF_BITCOUNT = 31_int32
    integer(kind=int32), parameter :: POW5_ARRAY_NCOL = 2

    integer(kind=int64), dimension(*), parameter :: POW5_SPLIT = [ &
                                                    536870912, 0, &
                                                    671088640, 0, &
                                                    838860800, 0, &
                                                    1048576000, 0, &
                                                    655360000, 0, &
                                                    819200000, 0, &
                                                    1024000000, 0, &
                                                    640000000, 0, &
                                                    800000000, 0, &
                                                    1000000000, 0, &
                                                    625000000, 0, &
                                                    781250000, 0, &
                                                    976562500, 0, &
                                                    610351562, 1073741824, &
                                                    762939453, 268435456, &
                                                    953674316, 872415232, &
                                                    596046447, 1619001344, &
                                                    745058059, 1486880768, &
                                                    931322574, 1321730048, &
                                                    582076609, 289210368, &
                                                    727595761, 898383872, &
                                                    909494701, 1659850752, &
                                                    568434188, 1305842176, &
                                                    710542735, 1632302720, &
                                                    888178419, 1503507488, &
                                                    555111512, 671256724, &
                                                    693889390, 839070905, &
                                                    867361737, 2122580455, &
                                                    542101086, 521306416, &
                                                    677626357, 1725374844, &
                                                    847032947, 546105819, &
                                                    1058791184, 145761362, &
                                                    661744490, 91100851, &
                                                    827180612, 1187617888, &
                                                    1033975765, 1484522360, &
                                                    646234853, 1196261931, &
                                                    807793566, 2032198326, &
                                                    1009741958, 1466506084, &
                                                    631088724, 379695390, &
                                                    788860905, 474619238, &
                                                    986076131, 1130144959, &
                                                    616297582, 437905143, &
                                                    770371977, 1621123253, &
                                                    962964972, 415791331, &
                                                    601853107, 1333611405, &
                                                    752316384, 1130143345, &
                                                    940395480, 1412679181 &
                                                    ]

    integer(kind=int64), dimension(*), parameter :: POW5_INV_SPLIT = [ &
                                                    268435456, 1, &
                                                    214748364, 1717986919, &
                                                    171798691, 1803886265, &
                                                    137438953, 1013612282, &
                                                    219902325, 1192282922, &
                                                    175921860, 953826338, &
                                                    140737488, 763061070, &
                                                    225179981, 791400982, &
                                                    180143985, 203624056, &
                                                    144115188, 162899245, &
                                                    230584300, 1978625710, &
                                                    184467440, 1582900568, &
                                                    147573952, 1266320455, &
                                                    236118324, 308125809, &
                                                    188894659, 675997377, &
                                                    151115727, 970294631, &
                                                    241785163, 1981968139, &
                                                    193428131, 297084323, &
                                                    154742504, 1955654377, &
                                                    247588007, 1840556814, &
                                                    198070406, 613451992, &
                                                    158456325, 61264864, &
                                                    253530120, 98023782, &
                                                    202824096, 78419026, &
                                                    162259276, 1780722139, &
                                                    259614842, 1990161963, &
                                                    207691874, 733136111, &
                                                    166153499, 1016005619, &
                                                    265845599, 337118801, &
                                                    212676479, 699191770, &
                                                    170141183, 988850146 &
                                                    ]

contains

    function f2shortest(fn) result(str)
        real(kind=real32), intent(in) :: fn
        character(len=:), allocatable :: str
        integer(kind=int32) :: bits
        integer(kind=int32) :: ieee_exponent, ieee_mantissa
        logical(kind=int32) :: ieee_sign
        integer(kind=int32) :: e2, m2
        logical(kind=int32) :: even
        integer(kind=int32) :: mv, mp, mm
        integer(kind=int32) :: dp, dv, dm, e10
        integer(kind=int32) :: q, k, i, j, l
        logical(kind=int32) :: dp_is_trailing_zeros, dv_is_trailing_zeros, dm_is_trailing_zeros
        integer(kind=int32) :: last_remove_digit
        integer(kind=int32) :: dplength
        integer(kind=int32) :: expn
        logical(kind=int32) :: scientific_notation
        integer(kind=int32) :: removed
        logical(kind=int32) :: done
        integer(kind=int32) :: output
        integer(kind=int32) :: olength
        integer(kind=int32) :: idx
        integer(kind=int32) :: c
        integer(kind=int32) :: current

        if (ieee_is_nan(fn)) then
            str = "NaN"
            return
        end if

        if (ieee_class(fn) == ieee_positive_inf) then
            str = "Infinity"
            return
        end if

        if (ieee_class(fn) == ieee_negative_inf) then
            str = "-Infinity"
            return
        end if

        if (ieee_class(fn) == ieee_positive_zero) then
            str = "0.0"
            return
        end if

        if (ieee_class(fn) == ieee_negative_zero) then
            str = "-0.0"
            return
        end if

        !! step 1
        bits = transfer(fn, 1_int32)
        ieee_sign = bits < 0
        ieee_exponent = iand(ishft(bits, -FLOAT_MANTISSA_BITS), FLOAT_EXPONENT_MASK)
        ieee_mantissa = iand(bits, FLOAT_MANTISSA_MASK)

        if (ieee_exponent == 0) then
            e2 = 1 - FLOAT_EXPONENT_BIAS - FLOAT_MANTISSA_BITS - 2
            m2 = ieee_mantissa
        else
            e2 = ieee_exponent - FLOAT_EXPONENT_BIAS - FLOAT_MANTISSA_BITS - 2
            m2 = ior(ieee_mantissa, ishft(1, FLOAT_MANTISSA_BITS))
        end if


        !! step 2
        even = iand(m2, 1) == 0
        mv = 4*m2
        mp = 4*m2 + 2
        if (m2 /= ishft(1_int64, FLOAT_MANTISSA_BITS) .or. ieee_exponent <= 1) then
            mm = 4*m2 - 2
        else
            mm = 4*m2 - 1
        end if

        !! step 3
        last_remove_digit = 0_int32
        dv_is_trailing_zeros = .false.
        dm_is_trailing_zeros = .false.

        if (e2 >= 0) then
            q = int(e2*LOG10_2_NUMERATOR/LOG10_2_DENOMINATOR, int32)
            k = POW5_INV_BITCOUNT + pow5bits(q) - 1
            i = -e2 + q + k
            dv = int(mulPow5InvDivPow2(mv, q, i), int32)
            dp = int(mulPow5InvDivPow2(mp, q, i), int32)
            dm = int(mulPow5InvDivPow2(mm, q, i), int32)
            if (q /= 0 .and. (dp - 1)/10 <= dm/10) then
                l = POW5_INV_BITCOUNT + pow5bits(q - 1) - 1
                last_remove_digit = int(mod(mulPow5InvDivPow2(mv, q - 1, -e2 + q - 1 + l), 10), int32)
            end if
            e10 = q
            dp_is_trailing_zeros = pow5Factor(mp) >= q
            dv_is_trailing_zeros = pow5Factor(mv) >= q
            dm_is_trailing_zeros = pow5Factor(mm) >= q
        else
            q = int(-e2*LOG10_5_NUMERATOR/LOG10_5_DENOMINATOR, int32)
            i = -e2 - q
            k = pow5bits(i) - POW5_BITCOUNT
            j = q - k

            dv = int(mulPow5divPow2(mv, i, j), int32)
            dp = int(mulPow5divPow2(mp, i, j), int32)
            dm = int(mulPow5divPow2(mm, i, j), int32)
            if (q /= 0 .and. (dp - 1)/10 <= dm/10) then
                j = q - 1 - (pow5bits(i + 1) - POW5_BITCOUNT)
                last_remove_digit = int(mod(mulPow5divPow2(mv, i + 1, j), 10), int32)
            end if
            e10 = q + e2
            dp_is_trailing_zeros = 1 >= q
            dm_is_trailing_zeros = merge(0, 1, mod(mm, 2) == 1) >= q
            if (q <= 1) then
                dv_is_trailing_zeros = .true.
            else if (q < 31) then
                dv_is_trailing_zeros = iand(mv, shiftl(1, q-1) - 1) == 0
            end if
        end if

        !! step 4
        dplength = decimal_length(dp)
        expn = e10 + dplength - 1
        scientific_notation = .not. (expn >= -3 .and. expn < 7)
        removed = 0
        if (dp_is_trailing_zeros .and. .not. accept_upper_bound(even)) dp = dp - 1

        done = .false.
        do while (dp/10 > dm/10 .and. .not. done)
            if (dp < 100 .and. scientific_notation) then
                done = .true.
            else
                dm_is_trailing_zeros = dm_is_trailing_zeros .and. (mod(dm, 10) == 0)
                dp = dp/10
                last_remove_digit = mod(dv, 10)
                dv = dv/10
                dm = dm/10
                removed = removed + 1
            end if
        end do

        if (dm_is_trailing_zeros .and. accept_lower_bound(even)) then
            done = .false.
            do while (mod(dm, 10) == 0 .and. .not. done)
                if (dp < 100 .and. scientific_notation) then
                    done = .true.
                else
                    dp = dp/10
                    last_remove_digit = mod(dv, 10)
                    dv = dv/10
                    dm = dm/10
                    removed = removed + 1
                end if
            end do
        end if

        if (dv_is_trailing_zeros .and. last_remove_digit == 5 .and. mod(dv, 2) == 0) &
            last_remove_digit = 4

        if ((dv == dm .and. (.not. dm_is_trailing_zeros .or. .not. accept_lower_bound(even))) &
            .or. last_remove_digit >= 5) then
            output = dv + 1
        else
            output = dv
        end if

        olength = dplength - removed

        !! step 5
        str = repeat(' ', 15)

        idx = 1
        if (ieee_sign) then
            str(idx:idx) = '-'
            idx = idx + 1
        end if

        if (scientific_notation) then
            do i = 0, olength - 2
                c = mod(output, 10)
                output = output/10
                str(idx + olength - i:idx + olength - i) = char(48 + c) ! 48 is ascii '0'
            end do
            str(idx:idx) = char(48 + mod(output, 10))
            str(idx + 1:idx + 1) = '.'
            idx = idx + olength + 1
            if (olength == 1) then
                str(idx:idx) = '0'
                idx = idx + 1
            end if
            str(idx:idx) = 'E'
            idx = idx + 1
            if (expn < 0) then
                str(idx:idx) = '-'
                idx = idx + 1
                expn = -expn
            end if
            if (expn >= 10) then
                str(idx:idx) = char(48 + expn/10)
                idx = idx + 1
            end if
            str(idx:idx) = char(48 + mod(expn, 10))
        else
            if (expn < 0) then
                str(idx:idx) = '0'
                idx = idx + 1
                str(idx:idx) = '.'
                idx = idx + 1
                i = -1
                do while (i > expn)
                    str(idx:idx) = '0'
                    idx = idx + 1
                    i = i - 1
                end do
                current = idx
                do i = 0, olength - 1
                    str(current + olength - i - 1:current + olength - i - 1) = char(48 + mod(output, 10))
                    output = output/10
                    idx = idx + 1
                end do
                idx = idx - 1
            else if (expn + 1 >= olength) then
                do i = 0, olength - 1
                    str(idx + olength - i - 1:idx + olength - i - 1) = char(48 + mod(output, 10))
                    output = output/10
                end do
                idx = idx + olength
                do i = 0, expn - olength
                    str(idx:idx) = '0'
                    idx = idx + 1
                end do
                str(idx:idx + 1) = ".0"
                idx = idx + 1
            else
                current = idx
                do i = 0, olength - 1
                    if (olength - i - 1 == expn) then
                        str(current + olength - i:current + olength - i) = '.'
                        current = current - 1
                    end if
                    str(current + olength - i:current + olength - i) = char(48 + mod(output, 10))
                    output = output/10
                end do
                idx = idx + olength
            end if
        end if

        str = str(1:idx)

    end function f2shortest

    pure function pow5bits(e) result(r)
        integer(kind=int32), intent(in) :: e
        integer(kind=int32) :: r

        if (e == 0) then
            r = 1
        else
            r = int((e*LOG2_5_NUMERATOR + LOG2_5_DENOMINATOR - 1)/LOG2_5_DENOMINATOR, int32)
        end if

    end function pow5bits

    pure function pow5Factor(v) result(count)
        integer(kind=int32), intent(in) :: v
        integer(kind=int32) :: count
        integer(kind=int32) :: v_

        v_ = v
        count = 0

        do while (v_ > 0)
            if (mod(v_, 5) /= 0) return
            v_ = v_/5
            count = count + 1
        end do

        error stop "Illegal Argument"

    end function pow5Factor

    pure function mulPow5divPow2(m, q, j) result(r)
        integer(kind=int32), intent(in) :: m
        integer(kind=int32), intent(in) :: q
        integer(kind=int32), intent(in) :: j
        integer(kind=int64) :: r

        integer(kind=int64) :: bits0, bits1

        bits0 = m*POW5_SPLIT(q*POW5_ARRAY_NCOL + 1)
        bits1 = m*POW5_SPLIT(q*POW5_ARRAY_NCOL + 2)

        r = shiftr(bits0 + shiftr(bits1, POW5_HALF_BITCOUNT), j - POW5_HALF_BITCOUNT)

    end function mulPow5divPow2

    pure function mulPow5InvDivPow2(m, q, j) result(r)
        integer(kind=int32), intent(in) :: m
        integer(kind=int32), intent(in) :: q
        integer(kind=int32), intent(in) :: j
        integer(kind=int64) :: r

        integer(kind=int64) :: bits0, bits1

        bits0 = m*POW5_INV_SPLIT(q*POW5_ARRAY_NCOL + 1)
        bits1 = m*POW5_INV_SPLIT(q*POW5_ARRAY_NCOL + 2)

        r = shiftr(bits0 + shiftr(bits1, POW5_INV_HALF_BITCOUNT), j - POW5_INV_HALF_BITCOUNT)

    end function mulPow5InvDivPow2

    pure function decimal_length(v) result(r)
        integer(kind=int32), intent(in) :: v
        integer(kind=int32) :: r
        integer(kind=int32) :: factor
        logical(kind=int32) :: done

        r = 10_int32
        factor = 1000000000_int32
        done = .false.
        do while (r > 0 .and. (.not. done))
            if (v >= factor) then
                done = .true.
            else
                factor = factor/10
                r = r - 1
            end if
        end do

    end function decimal_length

end module real32_to_shortest
