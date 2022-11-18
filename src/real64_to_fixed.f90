module real64_to_fixed
    use ryu_utils
    use ryu_lookup_table
    use iso_fortran_env, only: int32, int64, real64
    use ieee_arithmetic
    implicit none
    private
    public :: d2fixed

contains

    function d2fixed(d, precision_) result(str)
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
        integer(kind=int32) :: index
        logical(kind=int32) :: nonzero
        integer(kind=int32) :: digits
        integer(kind=int32) :: olength
        integer(kind=int32) :: blocks
        integer(kind=int32) :: fill
        integer(kind=int32) :: idx
        integer(kind=int32) :: p10bits
        integer(kind=int32) :: len
        integer(kind=int32) :: i, j, k, p
        integer(kind=int32) :: maximum
        integer(kind=int32) :: last_digit
        integer(kind=int32) :: roundup
        integer(kind=int32) :: required_twos
        logical(kind=int32) :: trailing_zeros
        integer(kind=int32) :: round_index
        integer(kind=int32) :: dot_index
        character(len=1) :: chr

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
                str = "0." // repeat('0', precision_)
            else
                str = '0'
            end if
            return
        end if

        if (ieee_class(d) == ieee_negative_zero) then
            if (precision_ > 0) then
                str = "-0." // repeat('0', precision_)
            else
                str = "-0"
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

        index = 1
        nonzero = .false.
        if (ieee_sign) then
            buffer(index:index) = '-'
            index = index + 1
        end if

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

                if (nonzero) then
                    call append_nine_digits(digits, buffer(index:))
                    index = index + 9
                else if (digits /= 0) then
                    olength = decimal_length9(digits)
                    call append_n_digits(olength, digits, buffer(index:))
                    index = index + olength
                    nonzero = .true.
                end if

            end do
        end if

        if (.not. nonzero) then
            buffer(index:index) = '0'
            index = index + 1
        end if
        if (precision > 0) then
            buffer(index:index) = '.'
            index = index + 1
        end if

        if (e2 < 0) then
            idx = -e2/16
            blocks = precision/9 + 1
            roundup = 0
            i = 0

            if (blocks <= MIN_BLOCK_2(idx + 1)) then
                i = blocks
                buffer(index:index + precision - 1) = repeat('0', precision)
                index = index + precision
            else if (i < MIN_BLOCK_2(idx + 1)) then
                i = MIN_BLOCK_2(idx + 1)
                buffer(index:index + 9*i - 1) = repeat('0', 9*i)
                index = index + 9*i
            end if

            do while (i < blocks)
                j = ADDITIONAL_BITS_2 + (-e2 - 16*idx)
                p = POW10_OFFSET_2(idx + 1) + i - MIN_BLOCK_2(idx + 1)

                if (p >= POW10_OFFSET_2(idx + 2)) then
                    fill = precision - 9*i
                    buffer(index:index + fill - 1) = repeat('0', fill)
                    index = index + fill
                    exit
                end if

                digits = mulShift_mod1e9(shiftl(m2, 8), &
                                         POW10_SPLIT_2(:, p + 1), &
                                         j + 8)

                if (i < blocks - 1) then
                    call append_nine_digits(digits, buffer(index:))
                    index = index + 9
                else
                    maximum = precision - 9*i
                    last_digit = 0
                    do k = 0, 8 - maximum
                        last_digit = mod(digits, 10)
                        digits = digits/10
                    end do

                    if (last_digit /= 5) then
                        roundup = merge(1, 0, last_digit > 5)
                    else
                        required_twos = -e2 - precision - 1
                        trailing_zeros = required_twos <= 0 &
                                         .or. (required_twos < 60 .and. multiple_of_power_of2(m2, required_twos))
                        roundup = merge(2, 1, trailing_zeros)
                    end if
                    if (maximum > 0) then
                        call append_c_digits(maximum, digits, buffer(index:))
                        index = index + maximum
                    end if
                    exit
                end if

                i = i + 1
            end do

            if (roundup /= 0) then
                round_index = index
                dot_index = 1
                do
                    round_index = round_index - 1
                    if (round_index /= 0) chr = buffer(round_index:round_index)
                    if (round_index == 0 .or. chr == '-') then
                        buffer(round_index + 1:round_index + 1) = '1'
                        if (dot_index > 1) then
                            buffer(dot_index:dot_index + 1) = "0."
                        end if
                        buffer(index:index) = '0'
                        index = index + 1
                        exit
                    end if
                    if (chr == '.') then
                        dot_index = round_index
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
        else
            buffer(index:index + precision - 1) = repeat('0', precision)
            index = index + precision
        end if

        index = index - 1
        str = buffer(:index)

    end function d2fixed

end module real64_to_fixed