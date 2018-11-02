Zofu (Zofu is Object-oriented Fortran Unit-testing) is a framework for carrying out unit testing of Fortran code modules. As the name suggests, it makes use of the object-oriented features of Fortran 2003.

# Writing test modules using Zofu

Write each test module making use of Zofu unit test object (unit_test_type) and its assertion methods

use zofu

Test subroutine interface

Subroutine naming and descriptions

Setup and teardown subroutines

Assertions supported: logical, integer, real, double precision, complex, character; and rank 1 and 2 arrays of them

Assertion syntax call test%assert(foo, bar) + optional assert name (and override tolerance for floating point assertions)

# Driver programs

Use zofu-driver to create the Fortran source code for a driver program to call all test subroutines in the test module: zofu-driver module driver [--mpi]

One driver program for each module. Each one returns a non-zero error code if any tests failed. Integration with e.g. meson test. Also produces YAML summary of cases / assertions to stdout.

Link test driver program with Zofu library (libzofu.so)

# Parallel unit tests using MPI

use zofu_mpi

Use unit_test_mpi_type. No need to use mpif90. Will build with MPI support if MPI is detected. Use --mpi argument to zofu-driver. Include mpi_init() in setup() and mpi_finalize() in teardown(). and How to run meson test with MPI.

# Building Zofu

Building and installing : via [Meson](https://mesonbuild.com/); how to specify prefix, build type etc.

# Testing Zofu

also via meson test. Running with MPI.

# Licensing

LGPL