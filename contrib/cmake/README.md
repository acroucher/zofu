# Zofu Installation and Configuration Notes

## Installing Zofu on Windows with Meson and Ninja

Example conditions:

- Windows 8.1 environment
- User name of `brak`
- Home directory of `C:\Users\brak`
- zofu installation (target) directory of `C:\Users\brak\dev`
- Working directory is arbitrary; assume `C:\Users\brak\Documents\git-projects`
- GCC 9 including `gfortran.exe` is installed under `c:\TDM-GCC-64`
- Python 3.8 installed under `C:\Python38`
- `ninja.exe` installed under `C:\ProgramData\chocolatey\bin\ninja.EXE`

~~~bat
cd C:\Users\brak\Documents\git-projects
C:\Python38\Scripts\pip3.8.exe install --user meson
C:\Users\brak\AppData\Roaming\Python\Python38\Scripts\meson.exe build --prefix=c:/Users/brak/dev --libdir=lib --includedir=finclude/zofu
git clone https://github.com/acroucher/zofu.git
cd zofu
C:\ProgramData\chocolatey\bin\ninja.EXE -C build
C:\ProgramData\chocolatey\bin\ninja.EXE -C build install
~~~

Due to issues with the meson/ninja build process, the Fortran `.mod` files must be manually copied to the installation directory:

~~~bat
cp build\libzofu.dll.p\*.mod  C:\Users\brak\dev\finclude\zofu
~~~

The resulting installation tree should look like:

~~~
C:\Users\brak\dev
|-- bin
|   |-- libzofu.dll
|   `-- zofu-driver.exe
|-- finclude
|   `-- zofu
|       |-- zofu.mod
|       |-- zofu_kinds.mod
|       |-- zofu_scan.mod
|       `-- zofu_str_utils.mod
`-- lib
    |-- libzofu.dll.a
    `-- pkgconfig
        `-- zofu.pc
~~~

The current working directory should be `C:\Users\brak\Documents\git-projects\zofu` and it should contain the subdirectory `test`

To manually test zofu:

~~~cmd
cd test
C:\TDM-GCC-64\bin\gfortran.exe -o test_integer_asserts check.F90 test_integer_asserts.F90 -Lc:\Users\brak\dev\lib -Ic:\Users\brak\dev\finclude\zofu -l zofu
cp C:\Users\brak\dev\bin\libzofu.dll .
.\test_integer_asserts.exe
~~~

The results should resemble:

~~~yaml
failed assertions:
- {"case": 2, "reason": "value", "values": [1, -2]}
- {"case": 4, "reason": "value", "values": [1, -2]}
- {"case": 6, "reason": "value", "values": [7, -7], "index": 3, "count": 1}
- {"case": 8, "reason": "value", "values": [7, -7], "index": 3, "count": 1}
- {"case": 9, "reason": "shape", "values": [3, 4]}
- {"case": 11, "reason": "value", "values": [3, 2], "index": [3, 1], "count": 1}
- {"case": 13, "reason": "value", "values": [3, 2], "index": [3, 1], "count": 1}
- {"case": 14, "reason": "shape", "values": [[3, 2], [2, 3]]}
cases: {"count": 14, "passed": 6, "failed": 8}
assertions: {"count": 14, "passed": 6, "failed": 8}
passed: false
~~~

## Installating Zofu on Linux with Meson and Ninja

Installation on Linux is essentially the same process as for Windows except the paths and library names differ.

Example conditions:

- Fairly recent Linux system (Fedora, Ubuntu, etc.)
- User name of `brak`
- Home directory of `/home/brak`
- zofu installation (target) directory of `/home/brak/dev`
- Working directory is arbitrary; assume `/home/brak/git-projects`
- Recent versions of `ninja`, `python`, and GCC including `gfortran` are installed and available on `PATH` (*e.g.* under `/usr/bin`, `/usr/local/bin`)

~~~sh
cd /home/brak/git-projects
pip3 install --user meson
[/home/brak/.local/bin?]/meson build --prefix=/home/brak/dev --libdir=lib --includedir=finclude/zofu
git clone https://github.com/acroucher/zofu.git
cd zofu
ninja -C build
ninja -C build install
~~~

Due to issues with the meson/ninja build process, the Fortran `.mod` files must be manually copied to the installation directory:

~~~sh
cp build/libzofu.so.p/*.mod  /home/brak/dev/finclude/zofu
~~~

The resulting installation tree should look like:

~~~
/home/brak/dev
|-- bin
|   `-- zofu-driver.exe
|-- finclude
|   `-- zofu
|       |-- zofu.mod
|       |-- zofu_kinds.mod
|       |-- zofu_scan.mod
|       `-- zofu_str_utils.mod
`-- lib
    |-- libzofu.so
    `-- pkgconfig
        `-- zofu.pc
~~~

The current working directory should be `/home/brak/git-projects/zofu` and it should contain the subdirectory `test`

To manually test zofu:

~~~sh
cd test
gfortran -o test_integer_asserts check.F90 test_integer_asserts.F90 -L/home/brak/dev/lib -I/home/brak/dev/finclude/zofu -l zofu
LD_LIBRARY_PATH=/home/brak/dev/lib ./test_integer_asserts
~~~

The results should resemble:

~~~yaml
failed assertions:
- {"case": 2, "reason": "value", "values": [1, -2]}
- {"case": 4, "reason": "value", "values": [1, -2]}
- {"case": 6, "reason": "value", "values": [7, -7], "index": 3, "count": 1}
- {"case": 8, "reason": "value", "values": [7, -7], "index": 3, "count": 1}
- {"case": 9, "reason": "shape", "values": [3, 4]}
- {"case": 11, "reason": "value", "values": [3, 2], "index": [3, 1], "count": 1}
- {"case": 13, "reason": "value", "values": [3, 2], "index": [3, 1], "count": 1}
- {"case": 14, "reason": "shape", "values": [[3, 2], [2, 3]]}
cases: {"count": 14, "passed": 6, "failed": 8}
assertions: {"count": 14, "passed": 6, "failed": 8}
passed: false
~~~

## Alternate Build and Installation Using CMake

The typical CMake build methods for Linux and Windows are similar. 
This process assumes CMake and a low-level generator such as `make`
or `ninja` are installed. If FORD is installed, documentation will
automatically be created during the build process.

### Windows

Assuming `cmake` is installed under `C:\Program Files\CMake\bin` and
`ninja` is installed as `C:\ProgramData\chocolatey\bin\ninja.EXE`.

~~~bat
cd C:\Users\brak\Documents\git-projects
git clone https://github.com/acroucher/zofu.git
cd zofu
mkdir build
cd build
C:\Program Files\CMake\bin\cmake.exe -G Ninja ..
C:\ProgramData\chocolatey\bin\ninja.EXE
C:\ProgramData\chocolatey\bin\ninja.EXE test
C:\ProgramData\chocolatey\bin\ninja.EXE package
C:\ProgramData\chocolatey\bin\ninja.EXE install
~~~

### Linux

Assuming `cmake` and `make` or `ninja` are installed and are accessable on `$PATH`:

~~~sh
cd /home/brak/git-projects
git clone https://github.com/acroucher/zofu.git
cd zofu
mkdir build
cd build
cmake ..
make
make test
make package
make install
~~~

The preceeding example expects that `cmake` chooses `UNIX Makefiles`
as the default generator. If `ninja` will be used as the generator,
replace the last five commands with

~~~sh
cmake.exe -G Ninja ..
ninja
ninja test
ninja package
ninja install
~~~

### Customizing Release Type

Pass `-D CMAKE_BUILD_TYPE=Debug` or `-D CMAKE_BUILD_TYPE=Release`
to `cmake` to specify the release type of the build. If not specified,
`Release` is assumed.

### Customizing Installation Paths

Four parameters are suppled for configuring the installation path of
each component of Zofu - the `zofu-driver` executable, the library,
Fortran `/.mod` files, and HTML documentation.

`ZOFU_BINARY_INSTALL_DIR` sets the relative path to `zofu-driver`
under the root install directory. If not specified, it typically
defaults to `./bin`.

`ZOFU_LIBRARY_INSTALL_DIR` sets the relative path to the library
(`.a`, `.dll`, `.dylib`). If not specified, it typically defaults to
`./lib`; see https://cmake.org/cmake/help/latest/module/GNUInstallDirs.html
for details.

`ZOFU_FORTRAN_MODULE_INSTALL_DIR` sets the relative path to the Fortran
`.mod` files; the default is `./finstall/zofu`

`ZOFU_DOCUMENTATION_INSTALL_DIR` sets the relative path to the HTML
documentation generated by FORD; the default is `./doc/html`

These variables are passed to CMake using the `-D` option. For example:

~~~sh
cmake -D CMAKE_BUILD_TYPE=Debug \
    -D ZOFU_BINARY_INSTALL_DIR:PATH=debug/bin  \
    -D ZOFU_LIBRARY_INSTALL_DIR:PATH=debug/lib \
    -D ZOFU_FORTRAN_MODULE_INSTALL_DIR:PATH=debug/finclude \
    -D ZOFU_DOCUMENTATION_INSTALL_DIR:PATH=debug/html \
    ..
~~~

then run `ninja` or `make` as appropriate.

## CMake Integration

Two CMake recipes have been included to assist with using zofu:
`FindZOFU.cmake` and `ZOFUhelper.cmake`. The former is used to detect
zofu using CMake's `find_package()` facility, the latter simplifies
compiling and running unit tests.

Place these files in a directory where CMake's `include()` command can
find them and add the following prior to the section of `CMakeLists.txt`
where tests are defined:

~~~cmake
find_package(ZOFU)
if(ZOFU_FOUND)
    include(ZOFUhelper)
endif()
~~~

See also `contrib/zofu/cmake/CMakeLists_zofu_framents.txt`

For CMake to find zofu, three parameters must be passed to CMake, each
pointing to a directory:

- `ZOFU_BINARY_PATH`, path containing `zofu-driver`. Windows also contains `libzofu.dll`
- `ZOFU_LIBRARY_PATH`, path containing `libzofu.so`, `libzofu.dll.a`, *etc.*
- `ZOFU_MODULE_PATH`, path containing `zofu.mod`, `zofu_kinds.mod`, `zofu_scan.mod`, and `zofu_str_utils.mod`

See the installation notes given previously for OS-specific values of these paths.

Using the Windows installation example:

~~~bat
cd my_cmake_project_root_dir
mkdir build
cd build
cmake.exe -D ZOFU_BINARY_PATH:PATH=/Users/brak/dev/bin       ^
    -D ZOFU_LIBRARY_PATH:PATH=/Users/brak/dev/lib            ^
    -D ZOFU_LIBRARY_PATH:PATH=/Users/brak/dev/finclude/zofu  ^
    ..
~~~

(note: `^` is the DOS batch file line continuation character,
equivalent to `\` in a unix shell)

If everything is properly configured, CMake will confirm it can find
zofu with a message like `Zofu Fortran testing library is available`

To configure unit tests with CMake and zofu, you will need

- Source code for unit tests
- A working directory which exists and is prepopulated with necessary
  files, *etc.* for running the unit tests

`ZOFUhelper.cmake` defines the CMake function `add_zofu_unit_test()`
which defines unit test compilation targets and adds tests to the
CTest test plan.

Example:

~~~cmake
if(ZOFU_FOUND)
    set(TEST_SOURCES
        "${TEST_SRC_BASE}/zofu_demo/check.F90"
        "${TEST_SRC_BASE}/zofu_demo/test_real_asserts.F90"
    )
    add_zofu_unit_test(
        TARGET ut_c1_zofudemo
        SOURCES "${TEST_SOURCES}"
        DEPENDENCIES pretest_setup
        WILL_FAIL
        RUN_DIRECTORY ${SOFIRE2_1C_TEST_DIR}
    )
endif()
~~~

The test `ut_c1_zofudemo` is defined. The executable named
`ut_c1_zofudemo` is built from the source files `check.F90`
and `test_real_asserts.F90` located in the directory
`${TEST_SRC_BASE}/zofu_demo`. The directory `${SOFIRE2_1C_TEST_DIR}`
should be created before the test is run and populated with any files
or resources needed by the unit test executable. In this example,
the test directory setup work is performed by the custom target
`pretest_setup` which is declared elsewhere in `CMakeLists.txt`
(not included). The test is set to fail if it runs for more than
90 seconds.

This demonstration test is based on test code from the zofu source
distribution and is expected to fail. `WILL_FAIL` is set so CMake/CTest
knows the test is expected to fail and will not fail the test plan if
failure is detected. For more information, detailed documentation is
provided in `ZOFUhelper.cmake`

See also `contrib/cmake/demo/CMakeLists_zofu_framents.txt`.
