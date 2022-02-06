program main
    use ryu, only: f2shortest, d2shortest, d2fixed, d2exp
    use iso_fortran_env, only: real32, real64
    implicit none

    write (*, "(A)") f2shortest(3.14159_real32)
    write (*, "(A)") d2shortest(2.718281828_real64)
    write (*, "(A)") d2fixed(1.2345678987654321_real64, 10)
    write (*, "(A)") d2exp(299792458._real64, 5)

end program main
