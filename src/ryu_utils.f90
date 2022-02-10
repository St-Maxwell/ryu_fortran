module ryu_utils
    use iso_fortran_env, only: int32, int64
    use ryu_lookup_table, only: DIGIT_TABLE
    implicit none
    private
    public :: FLOAT_MANTISSA_BITS, FLOAT_MANTISSA_MASK, FLOAT_EXPONENT_BITS
    public :: FLOAT_EXPONENT_MASK, FLOAT_EXPONENT_BIAS
    public :: DOUBLE_MANTISSA_BITS, DOUBLE_MANTISSA_MASK, DOUBLE_EXPONENT_BITS
    public :: DOUBLE_EXPONENT_MASK, DOUBLE_EXPONENT_BIAS
    public :: index_for_expn, pow10_bits_for_index, length_for_index, log10Pow2
    public :: mulShift_mod1e9, mod1e9, decimal_length9
    public :: multiple_of_power_of2, pow5Factor, multiple_of_power_of5
    public :: append_nine_digits, append_c_digits, append_d_digits, append_n_digits
    public :: accept_lower_bound, accept_upper_bound

    integer(kind=int32), parameter :: FLOAT_MANTISSA_BITS = 23_int32
    integer(kind=int32), parameter :: FLOAT_MANTISSA_MASK = ishft(1, FLOAT_MANTISSA_BITS) - 1
    integer(kind=int32), parameter :: FLOAT_EXPONENT_BITS = 8_int32
    integer(kind=int32), parameter :: FLOAT_EXPONENT_MASK = ishft(1, FLOAT_EXPONENT_BITS) - 1
    integer(kind=int32), parameter :: FLOAT_EXPONENT_BIAS = 127_int32

    integer(kind=int32), parameter :: DOUBLE_MANTISSA_BITS = 52_int32
    integer(kind=int64), parameter :: DOUBLE_MANTISSA_MASK = shiftl(1_int64, DOUBLE_MANTISSA_BITS) - 1_int64
    integer(kind=int32), parameter :: DOUBLE_EXPONENT_BITS = 11_int32
    integer(kind=int32), parameter :: DOUBLE_EXPONENT_MASK = shiftl(1_int32, DOUBLE_EXPONENT_BITS) - 1_int32
    integer(kind=int32), parameter :: DOUBLE_EXPONENT_BIAS = shiftl(1_int32, DOUBLE_EXPONENT_BITS - 1_int32) - 1_int32

contains

    pure function index_for_expn(e) result(r)
        integer(kind=int32), intent(in) :: e
        integer(kind=int32) :: r

        r = (e + 15)/16

    end function index_for_expn

    pure function pow10_bits_for_index(idx) result(r)
        integer(kind=int32), intent(in) :: idx
        integer(kind=int32) :: r
        integer(kind=int32), parameter :: POW10_ADDITIONAL_BITS = 120

        r = 16*idx + POW10_ADDITIONAL_BITS

    end function pow10_bits_for_index

    pure function length_for_index(idx) result(r)
        integer(kind=int32), intent(in) :: idx
        integer(kind=int32) :: r

        r = (log10Pow2(16*idx) + 1 + 16 + 8)/9

    end function length_for_index

    pure function log10Pow2(e) result(r)
        integer(kind=int32), intent(in) :: e
        integer(kind=int32) :: r

        r = shiftr(e*78913, 18)

    end function log10Pow2

    function mulShift_mod1e9(m, mul, j) result(r)
        integer(kind=int64), intent(in) :: m
        integer(kind=int64), dimension(:), intent(in) :: mul
        integer(kind=int32), intent(in) :: j
        integer(kind=int32) :: r

        integer(kind=int64) :: high0
        integer(kind=int64) :: low0
        integer(kind=int64) :: high1
        integer(kind=int64) :: low1
        integer(kind=int64) :: high2
        integer(kind=int64) :: low2
        !integer(kind=int64) :: s0low
        integer(kind=int64) :: s0high
        integer(kind=int32) :: c1
        integer(kind=int32) :: c2
        integer(kind=int64) :: s1low
        integer(kind=int64) :: s1high
        integer(kind=int64) :: r0
        integer(kind=int64) :: r1
        integer(kind=int64) :: r2

        low0 = umul128(m, mul(1), high0)
        low1 = umul128(m, mul(2), high1)
        low2 = umul128(m, mul(3), high2)
        s0high = low1 + high0
        c1 = merge(1, 0, blt(s0high, low1))
        s1low = low2 + high1 + c1
        c2 = merge(1, 0, blt(s1low, low2))
        s1high = high2 + c2

        if (j < 160) then
            r0 = mod1e9(s1high)
            r1 = mod1e9(ior(shiftl(r0, 32), shiftr(s1low, 32)))
            r2 = ior(shiftl(r1, 32), iand(s1low, 4294967295_int64))
            r = mod1e9(shiftr(r2, j - 128))
        else
            r0 = mod1e9(s1high)
            r1 = ior(shiftl(r0, 32), shiftr(s1low, 32))
            r = mod1e9(shiftr(r1, j - 160))
        end if

    end function mulShift_mod1e9

    function umul128(a, b, productHi) result(productLo)
        integer(kind=int64), intent(in) :: a
        integer(kind=int64), intent(in) :: b
        integer(kind=int64), intent(out) :: productHi
        integer(kind=int64) :: productLo

        integer(kind=int64) :: aLo, aHi, bLo, bHi
        integer(kind=int64) :: b00, b01, b10, b11
        integer(kind=int64) :: b00Lo, b00Hi
        integer(kind=int64) :: mid1
        integer(kind=int64) :: mid1Lo, mid1Hi
        integer(kind=int64) :: mid2
        integer(kind=int64) :: mid2Lo, mid2Hi
        integer(kind=int64) :: pHi, pLo

        aLo = iand(a, 4294967295_int64) ! 4294967295 is 0xFFFFFFFF
        aHi = iand(shiftr(a, 32), 4294967295_int64)
        bLo = iand(b, 4294967295_int64)
        bHi = iand(shiftr(b, 32), 4294967295_int64)

        b00 = aLo*bLo
        b01 = aLo*bHi
        b10 = aHi*bLo
        b11 = aHi*bHi

        b00Lo = iand(b00, 4294967295_int64)
        b00Hi = iand(shiftr(b00, 32), 4294967295_int64)

        mid1 = b10 + b00Hi
        mid1Lo = iand(mid1, 4294967295_int64)
        mid1Hi = iand(shiftr(mid1, 32), 4294967295_int64)

        mid2 = b01 + mid1Lo
        mid2Lo = iand(mid2, 4294967295_int64)
        mid2Hi = iand(shiftr(mid2, 32), 4294967295_int64)

        pHi = b11 + mid1Hi + mid2Hi
        pLo = ior(shiftl(mid2Lo, 32), b00Lo)

        productHi = pHi
        productLo = pLo

    end function umul128

    pure function mod1e9(x) result(r)
        integer(kind=int64), intent(in) :: x
        integer(kind=int32) :: r

        r = int(x - 1000000000_int64*(x/1000000000_int64), int32)

    end function mod1e9

    subroutine append_nine_digits(digits_, result)
        integer(kind=int32), intent(in) :: digits_
        character(len=*), intent(inout) :: result

        integer(kind=int32) :: digits
        integer :: i
        integer(kind=int32) :: c, c0, c1

        digits = digits_

        if (digits == 0) then
            result = repeat('0', 9)
            return
        end if

        do i = 1, 5, 4
            c = mod(digits, 10000)
            digits = digits/10000
            c0 = shiftl(mod(c, 100), 1)
            c1 = shiftl(c/100, 1)
            result(9 - i:9 - i + 1) = DIGIT_TABLE(c0 + 1:c0 + 2)
            result(7 - i:7 - i + 1) = DIGIT_TABLE(c1 + 1:c1 + 2)
        end do

        result(1:1) = char(48 + digits)

    end subroutine append_nine_digits

    subroutine append_c_digits(count, digits_, result)
        integer(kind=int32), intent(in) :: count
        integer(kind=int32), intent(in) :: digits_
        character(len=*), intent(inout) :: result

        integer(kind=int32) :: digits
        integer(kind=int32) :: i
        integer(kind=int32) :: c

        digits = digits_
        do i = 0, count - 2, 2
            c = shiftl(mod(digits, 100), 1)
            digits = digits/100
            result(count - i - 1:count - i) = DIGIT_TABLE(c + 1:c + 2)
        end do
        if (i < count) result(count - i:count - i) = char(48 + mod(digits, 10))

    end subroutine append_c_digits

    subroutine append_d_digits(olength, digits_, result)
        integer(kind=int32), intent(in) :: olength
        integer(kind=int32), intent(in) :: digits_
        character(len=*), intent(inout) :: result

        integer(kind=int32) :: digits
        integer(kind=int32) :: i
        integer(kind=int32) :: c, c0, c1

        i = 0
        digits = digits_
        do while (digits >= 10000)
            c = mod(digits, 10000)
            digits = digits/10000
            c0 = shiftl(mod(c, 100), 1)
            c1 = shiftl(c/100, 1)
            result(olength - i:olength - i + 1) = DIGIT_TABLE(c0 + 1:c0 + 2)
            result(olength - i - 2:olength - i - 1) = DIGIT_TABLE(c1 + 1:c1 + 2)
            i = i + 4
        end do
        if (digits >= 100) then
            c = shiftl(mod(digits, 100), 1)
            digits = digits/100
            result(olength - i:olength - i + 1) = DIGIT_TABLE(c + 1:c + 2)
        end if
        if (digits >= 10) then
            c = shiftl(digits, 1)
            result(3:3) = DIGIT_TABLE(c + 2:c + 2)
            result(2:2) = '.'
            result(1:1) = DIGIT_TABLE(c + 1:c + 1)
        else
            result(2:2) = '.'
            result(1:1) = char(48 + digits)
        end if

    end subroutine append_d_digits

    subroutine append_n_digits(olength, digits_, result)
        integer(kind=int32), intent(in) :: olength
        integer(kind=int32), intent(in) :: digits_
        character(len=*), intent(inout) :: result

        integer(kind=int32) :: digits
        integer(kind=int32) :: i
        integer(kind=int32) :: c, c0, c1

        i = 0
        digits = digits_
        do while (digits >= 10000)
            c = mod(digits, 10000)
            digits = digits/10000
            c0 = shiftl(mod(c, 100), 1)
            c1 = shiftl(c/100, 1)
            result(olength - i - 1:olength - i) = DIGIT_TABLE(c0 + 1:c0 + 2)
            result(olength - i - 3:olength - i - 2) = DIGIT_TABLE(c1 + 1:c1 + 2)
            i = i + 4
        end do
        if (digits >= 100) then
            c = shiftl(mod(digits, 100), 1)
            digits = digits/100
            result(olength - i - 1:olength - i) = DIGIT_TABLE(c + 1:c + 2)
            i = i + 2
        end if
        if (digits >= 10) then
            c = shiftl(digits, 1)
            result(olength - i - 1:olength - i) = DIGIT_TABLE(c + 1:c + 2)
        else
            result(1:1) = char(48 + digits)
        end if

    end subroutine append_n_digits

    pure function decimal_length9(v) result(r)
        integer(kind=int32), intent(in) :: v
        integer(kind=int32) :: r

        if (v >= 100000000) then
            r = 9; return
        else if (v >= 10000000) then
            r = 8; return
        else if (v >= 1000000) then
            r = 7; return
        else if (v >= 100000) then
            r = 6; return
        else if (v >= 10000) then
            r = 5; return
        else if (v >= 1000) then
            r = 4; return
        else if (v >= 100) then
            r = 3; return
        else if (v >= 10) then
            r = 2; return
        else
            r = 1; return
        end if

    end function decimal_length9

    pure function multiple_of_power_of2(value, p) result(r)
        integer(kind=int64), intent(in) :: value
        integer(kind=int32), intent(in) :: p
        logical(kind=int32) :: r

        r = iand(value, shiftl(1_int64, p) - 1) == 0

    end function multiple_of_power_of2

    pure function pow5Factor(v) result(count)
        integer(kind=int64), intent(in) :: v
        integer(kind=int32) :: count
        integer(kind=int64) :: v_

        v_ = v
        if (mod(v_, 5) /= 0) then
            count = 0; return
        end if
        if (mod(v_, 25) /= 0) then
            count = 1; return
        end if
        if (mod(v_, 125) /= 0) then
            count = 2; return
        end if
        if (mod(v_, 625) /= 0) then
            count = 3; return
        end if

        count = 4
        v_ = v_/625

        do while (v_ > 0)
            if (mod(v_, 5) /= 0) return
            v_ = v_/5
            count = count + 1
        end do

        error stop "Illegal Argument"

    end function pow5Factor

    pure function multiple_of_power_of5(value, p) result(r)
        integer(kind=int64), intent(in) :: value
        integer(kind=int32), intent(in) :: p
        logical(kind=int32) :: r

        r = pow5Factor(value) >= p

    end function multiple_of_power_of5

    !! now we always round to even
    pure function accept_upper_bound(even) result(r)
        logical(kind=int32), intent(in) :: even
        logical(kind=int32) :: r

        r = even

    end function accept_upper_bound

    pure function accept_lower_bound(even) result(r)
        logical(kind=int32), intent(in) :: even
        logical(kind=int32) :: r

        r = even

    end function accept_lower_bound


end module ryu_utils
