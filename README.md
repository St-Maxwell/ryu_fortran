# ryu_fortran
A Fortran implementation of [Ryu](https://dl.acm.org/doi/10.1145/3296979.3192369) algorithm which converts floating pointer numbers to decimal strings. It is more effective than internal file approach.

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
use ryu_real32, only: float_to_string
use ryu_real64, only: double_to_string
use iso_fortran_env: only: real32, real64

write(*,"(A)") float_to_string(3.14159_real32)
write(*,"(A)") double_to_string(2.718281828_real64)

! 3.14159
! 2.718281828
```

## Test
Ryu algorithm is meant to generate the shortest decimal representaion of a floating point number, and is able to preserve the information after conversion. That is, if we convert the produced string back to a floating number, we should obtain the same binary representation comparing to the original number.

We have tested a number of examples which showed the correctness of the implementation. The test samples are from [floaxie](https://github.com/aclex/floaxie/tree/master/test) repository.

To perform the tests, run
```bash
fpm test test-roundtrip
```


