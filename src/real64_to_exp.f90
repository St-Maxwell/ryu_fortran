module real64_to_exp
    use ryu_utils
    use ryu_lookup_table
    use iso_fortran_env, only: int32, int64, real64
    use ieee_arithmetic
    implicit none
    private
    public :: d2exp

contains

    function d2exp(d, precision_) result(str)
        real(kind=real64), intent(in) :: d
        integer(kind=int32), intent(in) :: precision_
        character(len=:), allocatable :: str

        character(len=2000) :: buffer
        integer(kind=int32) :: precision
        integer(kind=int64) :: bits
        integer(kind=int32) :: ieee_exponent
        integer(kind=int64) :: ieee_mantissa
        logical(kind=int32) :: ieee_sign
        integer(kind=int32) :: e2
        integer(kind=int64) :: m2
        logical(kind=int32) :: print_decimal_point
        integer(kind=int32) :: index
        integer(kind=int32) :: digits
        integer(kind=int32) :: printed_digits
        integer(kind=int32) :: available_digits
        integer(kind=int32) :: expn
        integer(kind=int32) :: idx
        integer(kind=int32) :: p10bits
        integer(kind=int32) :: len
        integer(kind=int32) :: i, j, k, p
        integer(kind=int32) :: maximum
        integer(kind=int32) :: last_digit
        integer(kind=int32) :: roundup
        integer(kind=int32) :: rexp
        integer(kind=int32) :: required_twos
        logical(kind=int32) :: trailing_zeros
        integer(kind=int32) :: required_fives
        integer(kind=int32) :: round_index
        character(len=1) :: chr
        integer(kind=int32) :: c

        ! deal with special cases
        if (ieee_is_nan(d)) then
            str = "NaN"
            return
        end if

        if (ieee_class(d) == ieee_positive_inf) then
            str = "Infinity"
            return
        end if

        if (ieee_class(d) == ieee_negative_inf) then
            str = "-Infinity"
            return
        end if

        if (ieee_class(d) == ieee_positive_zero) then
            if (precision_ > 0) then
                str = "0." // repeat('0', precision_) // "E+00"
            else
                str = '0E+00'
            end if
            return
        end if

        if (ieee_class(d) == ieee_negative_zero) then
            if (precision_ > 0) then
                str = "-0." // repeat('0', precision_) // "E+00"
            else
                str = "-0E+00"
            end if
            return
        end if

        bits = transfer(d, 1_int64)
        ieee_sign = bits < 0
        ieee_exponent = int(iand(shiftr(bits, DOUBLE_MANTISSA_BITS), int(DOUBLE_EXPONENT_MASK, int64)), int32)
        ieee_mantissa = iand(bits, DOUBLE_MANTISSA_MASK)

        if (ieee_exponent == 0) then
            e2 = 1 - DOUBLE_EXPONENT_BIAS - DOUBLE_MANTISSA_BITS
            m2 = ieee_mantissa
        else
            e2 = ieee_exponent - DOUBLE_EXPONENT_BIAS - DOUBLE_MANTISSA_BITS
            m2 = ior(ieee_mantissa, shiftl(1_int64, DOUBLE_MANTISSA_BITS))
        end if

        precision = precision_
        print_decimal_point = precision > 0
        precision = precision + 1

        index = 1
        if (ieee_sign) then
            buffer(index:index) = '-'
            index = index + 1
        end if
        digits = 0
        printed_digits = 0
        available_digits = 0
        expn = 0

        if (e2 >= -52) then
            if (e2 < 0) then
                idx = 0
            else
                idx = index_for_expn(e2)
            end if
            p10bits = pow10_bits_for_index(idx)
            len = length_for_index(idx)

            do i = len - 1, 0, -1
                j = p10bits - e2
                digits = mulShift_mod1e9(shiftl(m2, 8), &
                                         POW10_SPLIT(:, POW10_OFFSET(idx + 1) + i + 1), &
                                         j + 8)

                if (printed_digits /= 0) then
                    if (printed_digits + 9 > precision) then
                        available_digits = 9
                        exit
                    end if
                    call append_nine_digits(digits, buffer(index:))
                    index = index + 9
                    printed_digits = printed_digits + 9
                else if (digits /= 0) then
                    available_digits = decimal_length9(digits)
                    expn = i*9 + available_digits - 1
                    if (available_digits > precision) exit
                    if (print_decimal_point) then
                        call append_d_digits(available_digits, digits, buffer(index:))
                        index = index + available_digits + 1
                    else
                        buffer(index:index) = char(48 + digits)
                        index = index + 1
                    end if
                    printed_digits = available_digits
                    available_digits = 0
                end if
            end do
        end if

        if (e2 < 0 .and. available_digits == 0) then
            idx = -e2/16
            do i = MIN_BLOCK_2(idx + 1), 199
                j = ADDITIONAL_BITS_2 + (-e2 - 16*idx)
                p = POW10_OFFSET_2(idx + 1) + i - MIN_BLOCK_2(idx + 1)
                if (p >= POW10_OFFSET_2(idx + 2)) then
                    digits = 0
                else
                    digits = mulShift_mod1e9(shiftl(m2, 8), &
                                             POW10_SPLIT_2(:, p + 1), &
                                             j + 8)
                end if
                if (printed_digits /= 0) then
                    if (printed_digits + 9 > precision) then
                        available_digits = 9
                        exit
                    end if
                    call append_nine_digits(digits, buffer(index:))
                    index = index + 9
                    printed_digits = printed_digits + 9
                else if (digits /= 0) then
                    available_digits = decimal_length9(digits)
                    expn = -(i + 1)*9 + available_digits - 1
                    if (available_digits > precision) exit
                    if (print_decimal_point) then
                        call append_d_digits(available_digits, digits, buffer(index:))
                        index = index + available_digits + 1
                    else
                        buffer(index:index) = char(48 + digits)
                        index = index + 1
                    end if
                    printed_digits = available_digits
                    available_digits = 0
                end if
            end do
        end if

        maximum = precision - printed_digits

        if (available_digits == 0) digits = 0

        last_digit = 0
        if (available_digits > maximum) then
            do k = 1, available_digits - maximum
                last_digit = mod(digits, 10)
                digits = digits/10
            end do
        end if

        roundup = 0
        if (last_digit /= 5) then
            roundup = merge(1, 0, last_digit > 5)
        else
            rexp = precision - expn
            required_twos = -e2 - rexp
            trailing_zeros = required_twos <= 0 &
                             .or. (required_twos < 60 .and. multiple_of_power_of2(m2, required_twos))
            if (rexp < 0) then
                required_fives = -rexp
                trailing_zeros = trailing_zeros .and. multiple_of_power_of5(m2, required_fives)
            end if
            roundup = merge(2, 1, trailing_zeros)
        end if

        if (printed_digits /= 0) then
            if (digits == 0) then
                buffer(index:index + maximum - 1) = repeat('0', maximum)
            else
                call append_c_digits(maximum, digits, buffer(index:))
            end if
            index = index + maximum
        else
            if (print_decimal_point) then
                call append_d_digits(maximum, digits, buffer(index:))
                index = index + maximum + 1
            else
                buffer(index:index) = char(48 + digits)
                index = index + 1
            end if
        end if

        if (roundup /= 0) then
            round_index = index
            do
                round_index = round_index - 1
                if (round_index /= 0) chr = buffer(round_index:round_index)
                if (round_index == 0 .or. chr == '-') then
                    buffer(round_index + 1:round_index + 1) = '1'
                    expn = expn + 1
                    exit
                end if
                if (chr == '.') then
                    cycle
                else if (chr == '9') then
                    buffer(round_index:round_index) = '0'
                    roundup = 1
                    cycle
                else
                    if (roundup == 2 .and. mod(ichar(chr), 2) == 0) exit
                    buffer(round_index:round_index) = char(ichar(chr) + 1)
                    exit
                end if
            end do
        end if

        buffer(index:index) = 'E'
        index = index + 1
        if (expn < 0) then
            buffer(index:index) = '-'
            expn = -expn
            index = index + 1
        else
            buffer(index:index) = '+'
            index = index + 1
        end if

        if (expn >= 100) then
            c = mod(expn, 10)
            buffer(index:index + 1) = DIGIT_TABLE(2*(expn/10) + 1:2*(expn/10) + 2)
            buffer(index + 2:index + 2) = char(48 + c)
            index = index + 2
        else
            buffer(index:index + 1) = DIGIT_TABLE(2*expn + 1:2*expn + 2)
            index = index + 1
        end if

        str = buffer(:index)

    end function d2exp

end module real64_to_exp
