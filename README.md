# ryu_fortran
A Fortran implementation of [Ryu](https://dl.acm.org/doi/10.1145/3296979.3192369) algorithm which converts floating point numbers to decimal strings. It is more effective than internal file approach.

This implementation is based on the [Scala](https://github.com/scala-native/scala-native/tree/master/nativelib/src/main/scala/scala/scalanative/runtime/ieee754tostring/ryu) version of Ryu.


## Install

### Fortran package manager

Add fpm dependency declaration in the `fpm.toml` file of your project.
```toml
[dependencies]
ryu_fortran = { git = "https://github.com/St-Maxwell/ryu_fortran" }
```

## Usage

```fortran
use ryu, only: f2shortest, d2shortest, d2fixed, d2exp
use iso_fortran_env, only: real32, real64

write (*, "(A)") f2shortest(3.14159_real32)
write (*, "(A)") d2shortest(2.718281828_real64)
write (*, "(A)") d2fixed(1.2345678987654321_real64, 10)
write (*, "(A)") d2exp(299792458._real64, 5)

! 3.14159
! 2.718281828
! 1.2345678988
! 2.99792E+08
```

## Test
Ryu algorithm is meant to generate the shortest decimal representaion of a floating point number, and is able to preserve the information after conversion. That is, if we convert the produced string back to a floating point number, we should obtain the same binary representation comparing to the original number.

`f2shortest` has been fully tested using the test cases from [ulfjack/ryu](https://github.com/ulfjack/ryu/blob/master/ryu/tests/f2s_test.cc).

To perform the tests, run
```bash
fpm test --flag "-fno-range-check"
```

If you use gfortran to build this project, the compiler option '`-fno-range-check`' is needed. Because in `lookup_table.f90` there is an `int64` literal constant '`-6917529027641081856`' which is the minimum value of signed 64-bits integer type, and for some reason (maybe a gfortran's bug) it will cause compilation error.
