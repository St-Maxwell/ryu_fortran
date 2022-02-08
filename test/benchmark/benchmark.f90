module benchmark
    use ryu
    use mt19937
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none

contains

    subroutine bench_f2shortest(num_samples, num_iter)
        integer, intent(in) :: num_samples
        integer, intent(in) :: num_iter
        character(len=1000) :: buffer
        real(kind=real32) :: f
        integer(kind=int32) :: fi
        real(kind=real64) :: t1, t2
        real(kind=real64), dimension(:), allocatable :: delta1
        real(kind=real64), dimension(:), allocatable :: delta2
        real(kind=real64) :: mean1, mean2
        real(kind=real64) :: sigma1, sigma2
        integer :: i, j

        write(*,"('Benchmark for f2shortest')")

        allocate(delta1(num_samples))
        allocate(delta2(num_samples))

        do i = 1, num_samples
            fi = int(grnd(), int32)
            f = transfer(fi, 1._real32)
            
            call cpu_time(t1)
            do j = 1, num_iter
                buffer = f2shortest(f)
            end do
            call cpu_time(t2)
            delta1(i) = (t2 - t1) * 1000000 / num_iter ! convert to us

            call cpu_time(t1)
            do j = 1, num_iter
                write(buffer,"(g0)") f
            end do
            call cpu_time(t2)
            delta2(i) = (t2 - t1) * 1000000 / num_iter ! convert to us
        end do

        mean1 = sum(delta1) / num_samples
        mean2 = sum(delta2) / num_samples
        sigma1 = sqrt(sum(delta1*delta1)/num_samples - mean1*mean1)
        sigma2 = sqrt(sum(delta2*delta2)/num_samples - mean2*mean2)

        write(*,"('f2shortest Time (us): ',F9.7,'   Std Dev: ',F7.4)") mean1, sigma1
        write(*,"('internal IO Time (us): ',F9.7,'   Std Dev: ',F7.4)") mean2, sigma2

    end subroutine bench_f2shortest

    subroutine bench_d2shortest(num_samples, num_iter)
        integer, intent(in) :: num_samples
        integer, intent(in) :: num_iter
        character(len=1000) :: buffer
        real(kind=real64) :: f
        integer(kind=int64) :: fi
        real(kind=real64) :: t1, t2
        real(kind=real64), dimension(:), allocatable :: delta1
        real(kind=real64), dimension(:), allocatable :: delta2
        real(kind=real64) :: mean1, mean2
        real(kind=real64) :: sigma1, sigma2
        integer :: i, j

        write(*,"('Benchmark for d2shortest')")

        allocate(delta1(num_samples))
        allocate(delta2(num_samples))

        do i = 1, num_samples
            fi = grnd()
            f = transfer(fi, 1._real64)
            
            call cpu_time(t1)
            do j = 1, num_iter
                buffer = d2shortest(f)
            end do
            call cpu_time(t2)
            delta1(i) = (t2 - t1) * 1000000 / num_iter ! convert to us

            call cpu_time(t1)
            do j = 1, num_iter
                write(buffer,"(g0)") f
            end do
            call cpu_time(t2)
            delta2(i) = (t2 - t1) * 1000000 / num_iter ! convert to us
        end do

        mean1 = sum(delta1) / num_samples
        mean2 = sum(delta2) / num_samples
        sigma1 = sqrt(sum(delta1*delta1)/num_samples - mean1*mean1)
        sigma2 = sqrt(sum(delta2*delta2)/num_samples - mean2*mean2)

        write(*,"('d2shortest Time (us): ',F9.7,'   Std Dev: ',F7.4)") mean1, sigma1
        write(*,"('internal IO Time (us): ',F9.7,'   Std Dev: ',F7.4)") mean2, sigma2

    end subroutine bench_d2shortest

    subroutine bench_d2exp(num_samples, num_iter, precision)
        integer, intent(in) :: num_samples
        integer, intent(in) :: num_iter
        integer, intent(in) :: precision
        character(len=1000) :: buffer
        real(kind=real64) :: f
        integer(kind=int64) :: fi
        real(kind=real64) :: t1, t2
        real(kind=real64), dimension(:), allocatable :: delta1
        real(kind=real64), dimension(:), allocatable :: delta2
        real(kind=real64) :: mean1, mean2
        real(kind=real64) :: sigma1, sigma2
        integer :: i, j

        write(*,"('Benchmark for d2exp')")

        allocate(delta1(num_samples))
        allocate(delta2(num_samples))

        do i = 1, num_samples
            fi = grnd()
            f = transfer(fi, 1._real64)
            
            call cpu_time(t1)
            do j = 1, num_iter
                buffer = d2exp(f, precision)
            end do
            call cpu_time(t2)
            delta1(i) = (t2 - t1) * 1000000 / num_iter ! convert to us

            call cpu_time(t1)
            do j = 1, num_iter
                write(buffer,"(ES20.10)") f
            end do
            call cpu_time(t2)
            delta2(i) = (t2 - t1) * 1000000 / num_iter ! convert to us
        end do

        mean1 = sum(delta1) / num_samples
        mean2 = sum(delta2) / num_samples
        sigma1 = sqrt(sum(delta1*delta1)/num_samples - mean1*mean1)
        sigma2 = sqrt(sum(delta2*delta2)/num_samples - mean2*mean2)

        write(*,"('d2exp Time (us): ',F9.7,'   Std Dev: ',F7.4)") mean1, sigma1
        write(*,"('internal IO Time (us): ',F9.7,'   Std Dev: ',F7.4)") mean2, sigma2

    end subroutine bench_d2exp

    subroutine bench_d2fixed(num_samples, num_iter, precision)
        integer, intent(in) :: num_samples
        integer, intent(in) :: num_iter
        integer, intent(in) :: precision
        character(len=1000) :: buffer
        real(kind=real64) :: f
        integer(kind=int64) :: fi
        real(kind=real64) :: t1, t2
        real(kind=real64), dimension(:), allocatable :: delta1
        real(kind=real64), dimension(:), allocatable :: delta2
        real(kind=real64) :: mean1, mean2
        real(kind=real64) :: sigma1, sigma2
        integer :: i, j

        write(*,"('Benchmark for d2fixed')")

        allocate(delta1(num_samples))
        allocate(delta2(num_samples))

        do i = 1, num_samples
            fi = grnd()
            f = transfer(fi, 1._real64)
            
            call cpu_time(t1)
            do j = 1, num_iter
                buffer = d2fixed(f, precision)
            end do
            call cpu_time(t2)
            delta1(i) = (t2 - t1) * 1000000 / num_iter ! convert to us

            call cpu_time(t1)
            do j = 1, num_iter
                write(buffer,"(F1000.10)") f
            end do
            call cpu_time(t2)
            delta2(i) = (t2 - t1) * 1000000 / num_iter ! convert to us
        end do

        mean1 = sum(delta1) / num_samples
        mean2 = sum(delta2) / num_samples
        sigma1 = sqrt(sum(delta1*delta1)/num_samples - mean1*mean1)
        sigma2 = sqrt(sum(delta2*delta2)/num_samples - mean2*mean2)

        write(*,"('d2fixed Time (us): ',F9.7,'   Std Dev: ',F7.4)") mean1, sigma1
        write(*,"('internal IO Time (us): ',F9.7,'   Std Dev: ',F7.4)") mean2, sigma2

    end subroutine bench_d2fixed

end module benchmark

program main
    use benchmark
    use mt19937
    implicit none
    integer :: num_sample
    integer :: num_iter
    integer :: precision
    integer :: u

    ! prevent from compiler optimization
    open(newunit=u, file="./test/benchmark/bench_option.txt", action="read")
    read(u,*) num_sample
    read(u,*) num_iter
    read(u,*) precision
    close(u)

    call sgrnd(12345)

    call bench_f2shortest(num_sample, num_iter)
    call bench_d2shortest(num_sample, num_iter)
    call bench_d2exp(num_sample, num_iter, precision)
    call bench_d2fixed(num_sample, num_iter, precision)

end program main