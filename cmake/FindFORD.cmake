# Set FORD_FOUND if FORD documentation tool can be found; see
# https://github.com/Fortran-FOSS-Programmers/ford
if(IS_DIRECTORY "${FORD_PATH}")
    find_program(FORD_BINARY
        NAMES ford
        PATHS "${FORD_PATH}"
    )
else()
    find_program(FORD_BINARY NAMES ford)
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(FORD DEFAULT_MSG FORD_BINARY)