**Zofu** (Zofu is Object-oriented Fortran Unit-testing) is a framework for carrying out unit testing of Fortran code modules. As the name suggests, it makes use of the object-oriented features of Fortran 2003.

# Unit testing with Zofu

Unit testing with Zofu is module-based, so unit tests are gathered into Fortran **modules**. Typically a unit test module will contain all the unit tests for particular module in the code itself. Each unit test source file should contain only one unit test module.

Each unit test module may contain a number of test subroutines, or **cases**. Usually a case will test one aspect of a code module, e.g. a particular function or subroutine. Within each case, a number of **assertions** are made, i.e. tests of whether a specified condition is satisfied or not. A simple example would be testing the output of a function against expected values, for a range of given inputs.

# The Zofu unit test type

Zofu provides a `unit_test_type` derived type for making assertions within the unit test cases. Objects of this type also keep track of how many cases have been run in the test, how many have passed and failed, together with how many assertions have been made, and how many assertions have passed and failed.

# Writing test modules using Zofu

### Using Zofu and declaring a test

To use Zofu the test module must include a `use zofu` statement. A unit test object may then be declared, e.g.:

```fortran
use zofu
type(unit_test_type) :: test
```

### Test case subroutines

This test object is then passed in to each test case subroutine, e.g.:

```fortran
subroutine test_foo(test)
  ! Tests a foo
  class(unit_test_type), intent(in out) :: test
  ! test code goes here...
end subroutine test_foo
```

Each test case subroutine must have a name beginning with `test_`. This convention allows the test module to contain other subroutines which are not tests, but may be called by the test case subroutines.

If a test case subroutine contains a comment with a description of the test case as its first non-blank line, this description will be used in the test output (e.g. if an assertion in the case fails). If no description comment is given, the subroutine name is used for the description.

### Making assertions

Within a test case subroutine, assertions may be made using the `assert()` method of the `unit_test_type` object. This method takes either a single `logical` argument, or two arguments which the assertion tests for equality. These two arguments may be of type `logical`, `integer`, `real`, `double precision`, `complex`, or `character`. They may also be arrays of these types, of rank one or two.

For example, the statement:

```fortran
  call test%assert(OK)
```

asserts that the logical variable `OK` (declared in the test case subroutine) is true. The statement:

```fortran
  call test%assert(x, y)
```

asserts that the two variables `x` and `y` are equal. These variables could be of any of the types listed above.

For `real`, `double precision` or `complex` variables, equality is defined up to a given tolerance. Each unit test object has a built-in default tolerance given by its `tolerance` property. This is a relative tolerance (not absolute) and set by default at 1e-6. For a particular assertion the default tolerance can be overridden by adding a `tol` argument to the `assert()` method, e.g.:

```fortran
  call test%assert(x, y, tol = 1.e-9)
```

For character variables, leading and trailing blanks are ignored, but case is respected.

The `assert()` method can also take a `name` argument. This is a character string with a description of the assertion, which is used for output of failed assertions. For example:

```fortran
  call test%assert(x, y, 'test x = y')
```

### Setup and teardown routines

Each module may optionally contain special subroutines called `setup` and `teardown`. The `setup` subroutine is called before any tests are run, and the `teardown` routine is called after all tests are finished.

These can be used to initialize and finalize global variables or other settings for the test. Unlike the test case subroutines, they do not take the unit test object (or any other variables) as an argument.

# Driver programs

Once a unit test module has been written, the `zofu-driver` utility can be used to create the Fortran source code for a driver program which calls all test case subroutines in the test module. The driver program source also contains the appropriate `use` statements, declares a unit test object, and calls any `setup` and `teardown` routines at the start and end of the test.

The `zofu-driver` utility can be called from the command line as follows:

```
zofu-driver module driver [--mpi]
```

Here `module` is the filename of the unit test module, and `driver` is the filename of the driver source code to be written. (The optional `--mpi` argument is used for parallel unit tests.)

The driver program can then be compiled and linked to the unit test module and to the Zofu library. When run, the driver program will write details of any failed assertions to the standard output, together with a summary of all cases and assertions at the end of the test. The output is in [YAML](http://yaml.org/) format, so it can be redirected to a file and parsed with scripts if required.

The driver program will also return a non-zero error code if the test failed (i.e if any assertions failed).

# Running multiple tests

If a number of modules are to be tested, `zofu-driver` can build a separate driver program for each module. The tests can be run using a utility such as [meson test](https://mesonbuild.com/Unit-tests.html), which will run all the test driver programs and produce summary output for the whole suite of tests.

Some unit testing systems create a single driver program which runs all tests in multiple modules. While this is perhaps simpler, it has the disadvantage that if one test crashes, the entire suite of tests stops and no other tests can be run. By contrast, if there is a separate driver program for each test module, the suite of tests can continue to run in the event of one module crashing. This approach also allows individual modules to be tested without recompiling the test driver program.

# Parallel unit tests using MPI

Zofu can test modules that are parallelized using MPI. In this case, there is a modified derived type for the unit test, `unit_test_mpi_type` (which extends `unit_test_type`). This is in a separate Zofu module, `zofu_mpi`, so the test module must include the statement:

```fortran
use zofu_mpi
```

and the test object is declared as follows:

```fortran
type(unit_test_mpi_type) :: test
```

Similarly, the test case subroutines must be of the form:

```fortran
subroutine test_foo(test)

  class(unit_test_mpi_type), intent(in out) :: test
```

The same `zofu-driver` utility may be used to write MPI test driver programs, by using the `--mpi` switch on the command line, e.g.:

```
zofu-driver module driver --mpi
```

The setup and teardown routines in each test module should include commands for initializing and finalizing MPI, e.g. `mpi_init()` in the `setup()` routine, and `mpi_finalize()` in the `teardown()` routine.

# Building Zofu

A script (`meson.build`) is included for building Zofu using the [Meson](https://mesonbuild.com/) build system. This in turn uses the [Ninja](https://ninja-build.org/) tool to run the build. Meson and Ninja can be installed using your package manager or via [pip](https://packaging.python.org/tutorials/installing-packages/). Zofu can be configured by running:

```
meson build
```

This will create and configure a build subdirectory called `build`. By default, a debug build is performed. If you want an optimized release build, you can specify the build type at configuration time, e.g.:

```
meson build --buildtype=release
```

Zofu can then be built using:

```
ninja -C build
```

Zofu will be built with MPI support (including the `zofu_mpi` module) if Meson detects that MPI is installed on your machine. There is no need to use a wrapper compiler (e.g. mpif90) to build it.

# Installing Zofu

Zofu can be installed as follows:

```
ninja -C build install
```

By default this will install Zofu to a standard location, e.g. `/usr/local/lib` on Linux (for which you will generally need administrator privileges) . If you want to install it somewhere else, you can specify a 'prefix' using the `--prefix` and `--libdir` options at configure time, e.g.:

```
meson build --prefix=/home/bob/ --libdir=lib
```

would configure Zofu to install into the `/home/bob/lib` directory.

The Fortran module files (e.g. *.mod for the gfortran compiler) will also be installed (to make them available when building your test code), by default to the `include` subdirectory after the prefix. If you want to install the module files somewhere else, you can specify the `includedir` option at configure time, e.g.:

```
meson build --prefix=/home/bob/ --libdir=lib --includedir=finclude/zofu
```

Meson will also write a [pkg-config](https://www.freedesktop.org/wiki/Software/pkg-config/) file to make it easier for other software (e.g. your test driver programs) to link to Zofu. The pkg-config file is installed to the `pkgconfig` subdirectory under the directory where the Zofu library is installed.

# Testing Zofu

Zofu has its own unit tests. These can be run from a command line in the `build` directory using the command:

```
meson test
```

# Running tests with Meson

If you are using Meson to build your code, you can add the tests to your build and have Meson run them. Because the test driver source code files do not exist at configure time, but only after they have been created using `zofu-driver`, they can be declared in your `meson.build` file using the `configure_file()` function, e.g.:

```python
test_name = 'foo'

test_src = file(join_paths(meson.current_source_dir(),
             'test', 'src', test_name + '.F90'))
driver_src_name = test_name + '_driver.F90'

test_driver_src = configure_file(
                    output: driver_src_name,
                    command: ['zofu-driver', test_src, driver_src_name])

unit_test = executable(test_name,
              [test_driver_src, test_src],
              dependencies: zofu)
test(test_name, unit_test)

```

After building your code, all the tests can then be run from the build directory as follows:

```
meson test
```

Meson will run the tests and output a summary of how many tests passed, based on the return code from each test driver program. You can also run individual tests by adding the test name as defined in the build script:

```
meson test foo
```

If your code and tests are parallelized using MPI, you can run the tests in parallel by "wrapping" them with the `mpiexec` command, e.g.:

```
meson test --wrap='mpiexec -np 4'
```

would run all the tests on four processes. For MPI testing you should also declare your tests with the `is_parallel` option set to false, e.g.:

```python
test(test_name, unit_test, is_parallel: false)
```

otherwise Meson will by default attempt to run different tests on different processes at the same time.

# Licensing

Zofu is open-source software, licensed under the GNU Lesser General Public License (LGPL).
