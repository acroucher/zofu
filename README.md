![Unit tests](https://github.com/acroucher/zofu/workflows/Unit%20tests/badge.svg)

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

Within a test case subroutine, assertions may be made using the `assert()` method of the `unit_test_type` object. This method takes either a single `logical` argument, or two arguments which the assertion tests for equality. These two arguments may be of type `logical`, `integer` (4- or 8-byte), `real`, `double precision`, `complex`, or `character`. They may also be arrays of these types, of rank one or two.

For example, the statement:

```fortran
  call test%assert(OK)
```

asserts that the logical variable `OK` (declared in the test case subroutine) is true. The statement:

```fortran
  call test%assert(x, y)
```

asserts that the two variables `x` and `y` are equal. These variables could be of any of the types listed above.

For floating point (`real`, `double precision` or `complex`) variables, equality is defined up to a given tolerance. Each unit test object has a built-in default tolerance given by its `tolerance` property. This is a relative tolerance (not absolute) and set by default at 1e-6. For a particular assertion the default tolerance can be overridden by adding a `tol` argument to the `assert()` method, e.g.:

```fortran
  call test%assert(x, y, tol = 1.e-9)
```

For character variables, leading and trailing blanks are ignored, but case is respected.

The `assert()` method can also take an optional `name` argument. This is a character string with a description of the assertion, which is used for output of failed assertions. For example:

```fortran
  call test%assert(x, y, name = 'test x = y')
```

### Setup and teardown routines

Each module may optionally contain special subroutines called `setup` and `teardown`. The `setup` subroutine is called before any tests are run, and the `teardown` routine is called after all tests are finished.

These can be used to initialize and finalize global variables or other settings for the test. Unlike the test case subroutines, they do not take the unit test object (or any other variables) as an argument.

### Test setup routine

Each module may also contain another optional special subroutine called `setup_test`, to be called after the test is initialized but before any test cases are run. It can be used for modifying properties of the test, e.g. the default tolerance for floating point equality tests. The test object must be passed in to this subroutine, in the same way as it is passed in to the test case subroutines. In the example below:

```fortran
  subroutine setup_test(test)

    class(unit_test_type), intent(in out) :: test

    test%tolerance = 1.e-8

  end subroutine setup_test
```

the default floating point equality tolerance is set to 1e-8.

# Driver programs

Once a unit test module has been written, the `zofu-driver` utility can be used to create the Fortran source code for a driver program which calls all test case subroutines in the test module. The driver program source also contains the appropriate `use` statements, declares a unit test object, and calls any `setup`, `setup_test` or `teardown` routines at the start and end of the test.

The `zofu-driver` utility can be called from the command line as follows:

```
zofu-driver module driver [--mpi]
```

Here `module` is the filename of the unit test module, and `driver` is the filename of the driver source code to be written. (The optional `--mpi` argument is used for parallel unit tests.)

The driver program can then be compiled and linked to the unit test module and to the Zofu library. When run, the program will return a non-zero error code if the test failed (i.e if any assertions failed).

# Output from Zofu

As it runs, a Zofu test program will also write details of any failed assertions to the standard output, together with a summary of all cases and assertions at the end of the test. The output is in [YAML](http://yaml.org/) format, so it can be redirected to a file and parsed with scripts if required.

Here is the YAML output from an example (failed) test:

```yaml
failed assertions:
- {"case": "foo", "assertion": "fred", "reason": "value", "values": [1, -2]}
- {"case": "bar", "assertion": "mary", "reason": "value", "values": [7., -7.01], "index": 3, "count": 1}
- {"case": "bar", "assertion": "eric", "reason": "shape", "values": [3, 4]}
cases: {"count": 8, "passed": 7, "failed": 2}
assertions: {"count": 12, "passed": 9, "failed": 3}
passed: false
```

The YAML output from a failed assertion (in the "failed assertions" array) is itself a dictionary which may contain the following keys:

- "case": the number or name of the case
- "assertion": the name of the assertion
- "reason": "value" if the values being compared were not equal, or "shape" for array values which were not the same shape
- "values": an array of the two values being compared: for array values, this shows scalar values for the first array indices where the values were not equal
- "index": for rank-1 arrays, the integer index of the values shown in the "values" field (for rank-2 arrays, a 2-element integer array of indices)
- "count": for array values, the total number of elements which were not equal
- "rank": for tests parallelized with MPI, the processor rank of the failed assertion

(Note that the "failed assertions" array will be empty if the test passed.)

At the end of the test two further dictionaries are output, "cases" and "assertions", which summarise the total counts of cases and assertions, as well as how many passed and failed. Finally, the "passed" Boolean value records whether the test passed.

# Running multiple tests

If a number of modules are to be tested, `zofu-driver` can build a separate driver program for each module. The tests can be run using a utility such as [meson test](https://mesonbuild.com/Unit-tests.html) or [CTest](https://gitlab.kitware.com/cmake/community/-/wikis/doc/ctest/Testing-With-CTest), which will run all the test driver programs and produce summary output for the whole suite of tests.

Some unit testing systems create a single driver program which runs all tests in multiple modules. While this is perhaps simpler, it has the disadvantage that if one test crashes, the entire suite of tests stops and no other tests can be run. By contrast, if there is a separate driver program for each test module, the suite of tests can continue to run in the event of one module crashing. This approach also allows individual modules to be tested without recompiling the test driver program.

# Parallel unit tests using MPI

Zofu can test modules that are parallelized using MPI. In this case, there is a modified derived type for the unit test, `unit_test_mpi_type` (which extends `unit_test_type`). This is in a separate Zofu module, `zofu_mpi`, so the test driver program must include the statement:

```fortran
use zofu_mpi
```

and the test object is declared as follows:

```fortran
type(unit_test_mpi_type) :: test
```

If you use the `zofu-driver` utility to generate your test driver program then using the `--mpi` switch will take care of this for you:

```
zofu-driver module driver --mpi
```

Note that the test module should still `use zofu`, and the test case subroutine interface is the same as for serial unit tests:

```fortran
subroutine test_parallel_foo(test)

  class(unit_test_type), intent(in out) :: test
```

This works because `unit_test_mpi_type` extends `unit_test_type`, so is still of that class. (In fact it is necessary to keep the test case subroutine interface the same for both serial and parallel unit test cases, so declaring the test as `class(unit_test_mpi_type)` in your parallel test case will not work.)

The setup and teardown routines in each test module should include commands for initializing and finalizing MPI, e.g. `mpi_init()` in the `setup()` routine, and `mpi_finalize()` in the `teardown()` routine.

# Building and installing Zofu

Zofu includes scripts for building and installing using either [Meson](https://mesonbuild.com/) or [CMake](https://cmake.org/).

## Using Meson

### Building with Meson

A script (`meson.build`) is included for building Zofu using the [Meson](https://mesonbuild.com/) build system. This in turn uses the [Ninja](https://ninja-build.org/) tool to run the build. Meson and Ninja can be installed using your package manager or via [pip](https://packaging.python.org/tutorials/installing-packages/). Zofu can be configured by running:

```
meson build
```

in the Zofu root directory. This will create and configure a build subdirectory called `build` (you can substitute a different name if you prefer). By default, a debug build is performed. If you want an optimized release build, you can specify the build type at configuration time, e.g.:

```
meson build --buildtype=release
```

Zofu can then be built using:

```
ninja -C build
```

Zofu will be built with MPI support (including the `zofu_mpi` module) if Meson detects that MPI is installed on your machine. There is usually no need to use a wrapper compiler (e.g. mpif90) to build it.

However, if you are building on a system with an unusual compiler and/or MPI library setup (e.g. some types of compute cluster), Meson may not be able to detect MPI. In this case you can use a wrapper compiler. You can specify that you want to use a wrapper compiler by setting the `-Dmpi_wrapper_compiler` build option to `true`, and specify the wrapper compiler using the `FC` environment variable. For example:

```
FC=ftn meson build -Dmpi_wrapper_compiler=true
```

will configure the Zofu build to use an MPI wrapper compiler called `ftn`.

### Installing with Meson

Zofu can be installed as follows:

```
ninja -C build install
```

By default this will install the Zofu shared library to a standard location, e.g. `/usr/local/lib` on Linux (for which you will generally need administrator privileges) . If you want to install it somewhere else, you can specify a 'prefix' using the `--prefix` and `--libdir` options at configure time, e.g.:

```
meson build --prefix=/home/bob/ --libdir=lib
```

would configure Zofu to install into the `/home/bob/lib` directory. The `zofu-driver` utility will similary be installed by default to the `bin` subdirectory after the prefix; the subdirectory name can be changed using the `--bindir` option.

The Fortran module files (e.g. *.mod for the gfortran compiler) will also be installed (to make them available when building your test code), by default to the `include` subdirectory after the prefix. If you want to install the module files somewhere else, you can specify the `--includedir` option at configure time, e.g.:

```
meson build --prefix=/home/bob/ --libdir=lib --includedir=finclude/zofu
```

Meson will also write a [pkg-config](https://www.freedesktop.org/wiki/Software/pkg-config/) file to make it easier for other software (e.g. your test driver programs) to link to Zofu. The pkg-config file is installed to the `pkgconfig` subdirectory under the directory where the Zofu library is installed.

## Using CMake

### Building with CMake

A script (`CMakeLists.txt`) is included for building Zofu using [CMake](https://cmake.org/). CMake can be installed using your package manager. CMake will by default use the `make` tool to run the build, but can also use Ninja instead if you configure it to do so.

The Zofu CMake build can be configured by running:

```
mkdir build
cd build
cmake ..
```

in the Zofu root directory. Zofu can then be built by executing `make` in the build directory.

### Installing with CMake

Zofu can be installed by executing:

```
make package
make install
```
in the build directory.

### Customizing a CMake build

To specify a release type (`Release` or `Debug` for example), 
pass `-D CMAKE_BUILD_TYPE=Debug` or `-D CMAKE_BUILD_TYPE=Release`
to `cmake`. If not specified, `Release` is assumed.

The installation path for can be specified for each component of Zofu - the
`zofu-driver` executable, the library, Fortran `/.mod` files, and HTML
documentation. The following parameters are optional:

* `ZOFU_BINARY_INSTALL_DIR` sets the relative path to `zofu-driver` 
  under the root install directory. If not specified, it typically
  defaults to `./bin`.
* `ZOFU_LIBRARY_INSTALL_DIR` sets the relative path to the library
  (`.a`, `.dll`, `.dylib`). If not specified, it typically defaults to
  `./lib`; see https://cmake.org/cmake/help/latest/module/GNUInstallDirs.html
  for details.
* `ZOFU_FORTRAN_MODULE_INSTALL_DIR` sets the relative path to the Fortran
`.mod` files; the default is `./finstall/zofu`
* `ZOFU_DOCUMENTATION_INSTALL_DIR` sets the relative path to the HTML
documentation generated by FORD; the default is `./doc/html`

These variables are passed to CMake using the `-D` option. For example:

~~~sh
cd build
cmake -D CMAKE_BUILD_TYPE=Debug \
    -D ZOFU_BINARY_INSTALL_DIR:PATH=debug/bin  \
    -D ZOFU_LIBRARY_INSTALL_DIR:PATH=debug/lib \
    -D ZOFU_FORTRAN_MODULE_INSTALL_DIR:PATH=debug/finclude \
    -D ZOFU_DOCUMENTATION_INSTALL_DIR:PATH=debug/html \
    ..
make
make test
make package
~~~

Similarly for Windows:

~~~bat
cd build
cmake.exe -G Ninja -D CMAKE_BUILD_TYPE=Debug ^
    -D ZOFU_BINARY_INSTALL_DIR:PATH=debug/bin  ^
    -D ZOFU_LIBRARY_INSTALL_DIR:PATH=debug/lib ^
    -D ZOFU_FORTRAN_MODULE_INSTALL_DIR:PATH=debug/finclude ^
    -D ZOFU_DOCUMENTATION_INSTALL_DIR:PATH=debug/html ^
    ..
ninja
ninja test
ninja package
~~~

More information can be found in `contrib/cmake/README.md`

# Testing Zofu

Zofu has its own unit tests. If you are using Meson, these can be run from a command line in the `build` directory using the command:

```
meson test
```

If you are using CMake with `make`, the tests can be run using the command:

```
make test
```

# Running tests with Meson

If you are using Meson to build your code, you can add the tests to your build and have Meson run them. Because the test driver source code files do not exist at configure time, but only after they have been created using `zofu-driver`, they can be declared in your `meson.build` file using the `configure_file()` function.

The Meson build script below builds a shared library from a source file `adder.F90`, uses `zofu-driver` to create the driver source for a test module `adder_tests.F90` and creates a test driver program from it.

```python
project('adder', ['fortran', 'c'])

zofu = dependency('zofu', required: true)

src_dir = join_paths(meson.current_source_dir(), 'src')

adder = shared_library('adder', join_paths(src_dir, 'adder.F90'))

test_src_dir = join_paths(meson.current_source_dir(), 'test')

test_name = 'adder_tests'
test_src = join_paths(meson.current_source_dir(),
                      'test', test_name + '.F90')
driver_src_name = test_name + '_driver.F90'

test_driver_src = configure_file(
  output: driver_src_name,
  command: ['zofu-driver', test_src, driver_src_name])

test_exe = executable('adder_tests',
                      [test_driver_src, test_src],
                      link_with: adder,
                      dependencies: zofu)
test('adder_tests', test_exe)
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

# Documentation

This readme page is the main user documentation. In addition, it is possible to build HTML pages detailing the Zofu library API using the [FORD](https://github.com/Fortran-FOSS-Programmers/ford) documentation tool. Typing `ford doc.md` at the command line in the main Zofu directory will generate a `doc/` subdirectory containing a main HTML page `index.html`, which may be viewed in a web browser.

# Licensing

Zofu is open-source software, licensed under the GNU Lesser General Public License (LGPL).
