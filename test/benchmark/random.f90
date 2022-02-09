module random
    use iso_fortran_env, only: int32, int64
    implicit none
    private
    public :: random_init, random_int32, random_int64

    integer(kind=int32), parameter :: N = 624_int32
    integer(kind=int32), parameter :: M = 397_int32
    integer(kind=int32), parameter :: R = 31_int32
    integer(kind=int32), parameter :: A = 2567483615_int32 ! 0x9908B0DF
    integer(kind=int32), parameter :: F = 1812433253_int32
    integer(kind=int32), parameter :: U = 11_int32
    integer(kind=int32), parameter :: S = 7_int32
    integer(kind=int32), parameter :: B = 2636928640_int32 ! 0x9D2C5680
    integer(kind=int32), parameter :: T = 15_int32
    integer(kind=int32), parameter :: C = 4022730752_int32 ! 0xEFC60000
    integer(kind=int32), parameter :: L = 18_int32
    integer(kind=int32), parameter :: MASK_LOWER = int(shiftl(1_int64, R) - 1_int64, int32)
    integer(kind=int32), parameter :: MASK_UPPER = int(shiftl(1_int64, R), int32)

    integer(kind=int32), dimension(0:N - 1) :: mt
    integer(kind=int32) :: index

contains

    subroutine random_init(seed)
        integer(kind=int32), intent(in) :: seed
        integer(kind=int32) :: i

        mt(0) = seed
        do i = 1, N - 1
            mt(i) = F*ieor(mt(i - 1), shiftr(mt(i - 1), 30)) + i
        end do
        index = N

    end subroutine random_init

    subroutine twist()
        integer(kind=int32) :: i, x, xA

        do i = 0, N - 1
            x = iand(mt(i), MASK_UPPER) + iand(mt(mod(i + 1, N)), MASK_LOWER)
            xA = shiftr(x, 1)
            if (iand(x, 1_int32) /= 0) xA = ieor(xA, A)
            mt(i) = ieor(mt(mod(i + M, N)), xA)
        end do

        index = 0

    end subroutine twist

    function random_int32() result(y)
        integer(kind=int32) :: y
        integer(kind=int32) :: i

        i = index
        if (index >= N) then
            call twist()
            i = index
        end if

        y = mt(i)
        index = i + 1

        y = ieor(y, shiftr(mt(i), U))
        y = ieor(y, iand(shiftl(y, S), B))
        y = ieor(y, iand(shiftl(y, T), C))
        y = ieor(y, shiftr(y, L))

    end function random_int32

    function random_int64() result(y)
        integer(kind=int64) :: y
        integer(kind=int64) :: high, low

        high = transfer([0_int32, random_int32()], 1_int64)
        low = transfer([random_int32(), 0_int32], 1_int64)
        y = ior(high, low)

    end function random_int64

end module random
