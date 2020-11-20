# cmake_minimum_required(VERSION 3.6) #maybe ok?
cmake_minimum_required(VERSION 3.6)

# Append local CMake module directory
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# Set package metadata
set(CPACK_PACKAGE_NAME "zofu")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "1")
set(CPACK_PACKAGE_VERSION_PATCH "1")
set(CPACK_PACKAGE_VERSION "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Zofu is a unit testing framework for modern Fortran")
set(CPACK_PACKAGE_VENDOR "Acorvid Technical Services Corporation")
set(CPACK_PACKAGE_CONTACT "Bob Apthorpe <bob.apthorpe@gmail.com>")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
set(CPACK_PACKAGE_HOMEPAGE_URL "https://github.com/apthorpe/zofu")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "${CPACK_PACKAGE_NAME}_${CPACK_PACKAGE_VERSION}")

# Save compilation commands to compile_commands.json
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Set project name and language
project(zofu
    LANGUAGES Fortran
    VERSION "${CPACK_PACKAGE_VERSION}")

###############################################################################
## Options ####################################################################
###############################################################################

# Enable MPI support; disabled by default
option(ENABLE_MPI "Enable MPI" OFF)

option(BUILD_DOCS "build Zofu documentation" OFF)

# Set additional search path for FORD
set(FORD_PATH "" CACHE PATH "Directory path of FORD documentation tool")

# Set relative path to executables under installation root directory
# Default is ""; if not set, defaults to CMAKE_INSTALL_BINDIR for the
# platform. See https://cmake.org/cmake/help/latest/module/GNUInstallDirs.html
set(ZOFU_BINARY_INSTALL_DIR ""
    CACHE PATH "Relative path to Zofu executables for installation")

# Set relative path to libraries under installation root directory
# Default is ""; if not set, defaults to CMAKE_INSTALL_LIBDIR for the
# platform. See https://cmake.org/cmake/help/latest/module/GNUInstallDirs.html
set(ZOFU_LIBRARY_INSTALL_DIR ""
    CACHE PATH "Relative path to Zofu libraries for installation")

# Set relative path to Fortran `.mod` files under installation root directory
# Default is "finstall/zofu"
set(ZOFU_FORTRAN_MODULE_INSTALL_DIR "finstall/zofu"
    CACHE PATH "Relative path to Zofu Fortran `.mod` files for installation")

# Set relative path to HTML documentation under installation root directory
# Default is "doc/html"
set(ZOFU_DOCUMENTATION_INSTALL_DIR "doc/html"
    CACHE PATH "Relative path to Zofu documentation for installation")

###############################################################################
## Dependencies and CMake Modules  ############################################
###############################################################################

# Defined cmake_parse_arguments(). Needed only for CMake 3.4 and earlier
include(CMakeParseArguments)

# Set default installation paths; should be invoked after setting project
# language(s)
include(GNUInstallDirs)
if(NOT ZOFU_BINARY_INSTALL_DIR)
    set(ZOFU_BINARY_INSTALL_DIR "${CMAKE_INSTALL_BINDIR}")
endif()
if(NOT ZOFU_LIBRARY_INSTALL_DIR)
    set(ZOFU_LIBRARY_INSTALL_DIR "${CMAKE_INSTALL_LIBDIR}")
endif()

# CTest setup
# Needed for valgrind (usually only enable_testing() is needed)
include(CTest)

# Query MPI support
if(ENABLE_MPI)
    find_package(MPI COMPONENTS Fortran OPTIONAL_COMPONENTS)
endif()

# Add convenience function for creating and running acceptance tests
include(ZofuAcceptanceHelper)

# Fortran Documenter
if(BUILD_DOCS)
    find_package(FORD)
endif()

###############################################################################
## Build ######################################################################
###############################################################################

# Interrogates Fortran compiler to see which options are supported
# (vs configuring by looking at compiler name or ID)
include(DefineFortranOptions)

list(APPEND FCOPTS
    ${FCOPT_NO_OPTIMIZATION}
    ${FCOPT_WALL}
    ${FCOPT_FCHECKALL}
    ${FCOPT_DEBUG}
    ${FCOPT_BACKTRACE}
)

if(${FC_ALLOWS_NO_MAYBE_UNINITIALIZED})
    list(APPEND FCOPTS ${FCOPT_NO_MAYBE_UNINITIALIZED})
endif()

# Set recent language standard if available
if(${FC_ALLOWS_STD_F2018})
    list(APPEND FCOPTS ${FCOPT_STD_F2018})
elseif(${FC_ALLOWS_STD_F2008})
    list(APPEND FCOPTS ${FCOPT_STD_F2008})
endif()

message(STATUS "General Fortran compiler options set to ${FCOPTS}")

### Zofu library

set(ZOFU_LIB_NAME zofu)

set(ZOFU_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/src")

# Define sources for zofu library (no MPI)
set(ZOFU_SERIAL_LIB_SOURCES
    "${ZOFU_SOURCE_DIR}/zofu.F90"
    "${ZOFU_SOURCE_DIR}/zofu_kinds.F90"
    "${ZOFU_SOURCE_DIR}/zofu_scan.F90"
    "${ZOFU_SOURCE_DIR}/zofu_str_utils.F90"
)

# Define sources for MPI-enabled zofu library (no MPI)
set(ZOFU_MPI_LIB_SOURCES "${ZOFU_SOURCE_DIR}/zofu_mpi.F90")

set(ZOFU_LIB_SOURCES ${ZOFU_SERIAL_LIB_SOURCES})
if(MPI_Fortran_FOUND)
    list(APPEND ZOFU_LIB_SOURCES ${ZOFU_MPI_LIB_SOURCES})
endif()

set(ZOFU_FORTRAN_MODULE_DIR "${CMAKE_CURRENT_BINARY_DIR}/${ZOFU_LIB_NAME}_include")
file(MAKE_DIRECTORY "${ZOFU_FORTRAN_MODULE_DIR}")
set(ZOFU_MODULE_FILE "${ZOFU_FORTRAN_MODULE_DIR}/zofu.mod")
add_library(${ZOFU_LIB_NAME} ${ZOFU_LIB_SOURCES})
set_target_properties(
    ${ZOFU_LIB_NAME}
    PROPERTIES
    Fortran_MODULE_DIRECTORY ${ZOFU_FORTRAN_MODULE_DIR}
)

if(MPI_Fortran_FOUND)
    target_compile_options(${ZOFU_LIB_NAME} PUBLIC ${FCOPTS} ${MPI_Fortran_COMPILE_OPTIONS})
    set_target_properties(
        ${ZOFU_LIB_NAME}
        PROPERTIES
        INCLUDE_DIRECTORIES ${MPI_Fortran_INCLUDE_DIRS}
    )
else()
    target_compile_options(${ZOFU_LIB_NAME} PUBLIC ${FCOPTS})
endif()

install(DIRECTORY "${ZOFU_FORTRAN_MODULE_DIR}/"
    DESTINATION "${ZOFU_FORTRAN_MODULE_INSTALL_DIR}")

### Zofu-driver executable

# Define sources for zofu-driver executable
set(ZOFUDRIVER_SOURCES "${ZOFU_SOURCE_DIR}/zofu_driver.F90")

# Define zofu-driver executable artifact
set(ZOFUDRIVER_FORTRAN_MODULE_DIR "${CMAKE_CURRENT_BINARY_DIR}/zofu-driver_include")
file(MAKE_DIRECTORY "${ZOFUDRIVER_FORTRAN_MODULE_DIR}")
add_executable(ZOFUDRIVER ${ZOFUDRIVER_SOURCES})
set_target_properties(
    ZOFUDRIVER
    PROPERTIES
    OUTPUT_NAME zofu-driver
    DEPENDS ${ZOFU_LIB_NAME}
    Fortran_MODULE_DIRECTORY ${ZOFUDRIVER_FORTRAN_MODULE_DIR}
    INCLUDE_DIRECTORIES ${ZOFU_FORTRAN_MODULE_DIR}
    LINK_LIBRARIES ${ZOFU_LIB_NAME}
)

if(MPI_Fortran_FOUND)
    target_compile_options(ZOFUDRIVER PUBLIC ${FCOPTS} ${MPI_Fortran_COMPILE_OPTIONS})
else()
    target_compile_options(ZOFUDRIVER PUBLIC ${FCOPTS})
endif()

install(
    TARGETS ZOFUDRIVER ${ZOFU_LIB_NAME}
    RUNTIME DESTINATION ${ZOFU_BINARY_INSTALL_DIR}
    LIBRARY DESTINATION ${ZOFU_LIBRARY_INSTALL_DIR}
    ARCHIVE DESTINATION ${ZOFU_LIBRARY_INSTALL_DIR}
)

###############################################################################
## Testing ####################################################################
###############################################################################

if(BUILD_TESTING)
# Directory containing unit test source files
set(TEST_SRC_BASE "${CMAKE_CURRENT_SOURCE_DIR}/test")

# Simple unit tests

set(SIMPLE_TEST
    test_complex_asserts
    test_double_asserts
    test_integer_asserts
    test_logical_asserts
    test_real_asserts
    test_string_asserts
    test_str_utils
)

foreach(TEST_NAME IN LISTS SIMPLE_TEST)
    set(TEST_SOURCES ${TEST_SRC_BASE}/check.F90 ${TEST_SRC_BASE}/${TEST_NAME}.F90)
    add_zofu_acceptance_test(
       TARGET ${TEST_NAME}
       SOURCES ${TEST_SOURCES}
    )
endforeach()

# test_zofu_scan - needs environment variable set

set(TEST_NAME test_zofu_scan)
set(TEST_SOURCES ${TEST_SRC_BASE}/test_zofu_scan.F90)
# set(TEST_ENV "ZOFU_TEST_DATA_PATH=${TEST_SRC_BASE}/data/")
add_zofu_acceptance_test(
    TARGET ${TEST_NAME}
    SOURCES ${TEST_SOURCES}
)
# Note: Set environment variable ZOFU_TEST_DATA_PATH
# to ${CMAKE_CURRENT_SOURCE_DIR}/test/data/ <-- TRAILING SLASH
set_tests_properties(${TEST_NAME}
    PROPERTIES
    ENVIRONMENT ZOFU_TEST_DATA_PATH=${TEST_SRC_BASE}/data/
)

# test_mpi - needs MPI compilation options and .mod files

if(MPI_Fortran_FOUND)
    # test_mpi
    set(TEST_NAME test_mpi)
    set(TEST_SOURCES
        ${TEST_SRC_BASE}/check_mpi.F90
        ${TEST_SRC_BASE}/test_mpi.F90
    )
    add_zofu_acceptance_test(
        TARGET ${TEST_NAME}
        SOURCES ${TEST_SOURCES}
    )
    set_target_properties(
        ${TEST_NAME}
        PROPERTIES
        INCLUDE_DIRECTORIES ${MPI_Fortran_INCLUDE_DIRS}
    )
    target_compile_options(${TEST_NAME} PUBLIC ${MPI_Fortran_COMPILE_OPTIONS})
endif()

endif(BUILD_TESTING)

###############################################################################
## Documentation ##############################################################
###############################################################################

if(FORD_FOUND)
    set(FORD_BUILD_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/doc")
    set(FORD_CONFIG_TEMPLATE "${CMAKE_CURRENT_SOURCE_DIR}/doc.md.in")
    set(FORD_CONFIG "${CMAKE_CURRENT_BINARY_DIR}/doc.md")
    file(MAKE_DIRECTORY "${FORD_BUILD_DIRECTORY}")
    configure_file("${FORD_CONFIG_TEMPLATE}" "${FORD_CONFIG}" @ONLY)

    add_custom_target(doc
        ALL
        COMMAND ${CMAKE_COMMAND} -E echo "Generating documentation with FORD"
        COMMAND ${FORD_BINARY} ${FORD_CONFIG}
        WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
        VERBATIM
        USES_TERMINAL
    )
    # Installs contents of ./build/doc under installer path ./doc/html (note
    # trailing slash in source directory)
    install(DIRECTORY "${FORD_BUILD_DIRECTORY}/"
        DESTINATION "${ZOFU_DOCUMENTATION_INSTALL_DIR}")
endif()

###############################################################################
## Packaging ##################################################################
###############################################################################

# Documentation files
install(FILES LICENSE README.md
    DESTINATION doc)

list(APPEND CPACK_GENERATOR ZIP)

if(WIN32)
    # Set up NSIS
    find_package(NSIS)
    if(NSIS_FOUND)
        # set(CPACK_NSIS_MUI_ICON "${CMAKE_CURRENT_SOURCE_DIR}/img/ZOFU.ico")
        # set(CPACK_NSIS_MUI_UNIICON "${CMAKE_CURRENT_SOURCE_DIR}/img/ZOFU.ico")
        set(CPACK_NSIS_INSTALLED_ICON_NAME "Uninstall.exe")
        set(CPACK_NSIS_HELP_LINK "${CPACK_PACKAGE_HOMEPAGE_URL}")
        set(CPACK_NSIS_URL_INFO_ABOUT "${CPACK_PACKAGE_HOMEPAGE_URL}")
        set(CPACK_NSIS_MODIFY_PATH ON)
        set(CPACK_NSIS_ENABLE_UNINSTALL_BEFORE_INSTALL ON)

        list(APPEND CPACK_GENERATOR NSIS)
    endif()

    # NuGet
    # TODO: Find a more robust means of detecting whether NuGet is available
    find_program(NUGET_EXECUTABLE nuget)
    if(NUGET_EXECUTABLE)
        message(STATUS "NuGet found at ${NUGET_EXECUTABLE}; needs configuration")

        # list(APPEND CPACK_GENERATOR NuGet)
    endif()

    # Set up WIX
    # TODO: Make this user-configurable or otherwise deal with version and architecture in WIX path
    set(WIX_ROOT_DIR "/Program Files (x86)/WiX Toolset v3.11")
    set(WIX_FIND_REQUIRED OFF)
    set(WIX_FIND_QUIETLY ON)
    find_package(WIX)
    # message("WIX found? ${WIX_FOUND}")
    if(WIX_FOUND)
        message(STATUS "WIX was found: WIX_FOUND = ${WIX_FOUND}")
        # The variable holding the WIX root directory used by CPack is different from the
        # variable populated by find_package(WIX) i.e. cmake/FindWix.cmake
        # Manually tell CPack where find_package() found WIX...
        set(CPACK_WIX_ROOT "${WIX_ROOT_DIR}")
        # Manually convert LICENSE to RTF format because WIX/CPack is stupid
        set(WIX_LICENSE_FILE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE.rtf")
        set(CPACK_WIX_LICENSE_RTF "${WIX_LICENSE_FILE}")
        install(FILES "${WIX_LICENSE_FILE}" DESTINATION doc)
        # set(CPACK_WIX_PRODUCT_ICON "${CMAKE_CURRENT_SOURCE_DIR}/img/ZOFU.ico")
        # Note: This should not change for the life of the product
        # Generated from guidgen.exe
        # set(CPACK_WIX_UPGRADE_GUID "")

        if(EXISTS "${WIX_CANDLE}")
            list(APPEND CPACK_GENERATOR WIX)
        else()
            message(STATUS "Why is WIX_FOUND = ${WIX_FOUND} if WIX_CANDLE (${WIX_CANDLE}) is not found?")
        endif()
    else()
        message(STATUS "WIX was not found: WIX_FOUND = ${WIX_FOUND}")
    endif()
else()
    list(APPEND CPACK_GENERATOR TGZ TBZ2)
    if(APPLE)
        # Set up DRAGNDROP
        # Add DragNDrop properties
        set(CPACK_DMG_VOLUME_NAME "${CPACK_PACKAGE_NAME} v${CPACK_PACKAGE_VERSION}")
#        set(CPACK_DMG_FORMAT "UDZO")
        set(CPACK_DMG_FORMAT "UDBZ")
#*       CPACK_DMG_DS_STORE
#*       CPACK_DMG_DS_STORE_SETUP_SCRIPT
#*       CPACK_DMG_BACKGROUND_IMAGE
#        CPACK_DMG_DISABLE_APPLICATIONS_SYMLINK
#        CPACK_DMG_SLA_DIR
#        CPACK_DMG_SLA_LANGUAGES
#        CPACK_DMG_<component>_FILE_NAME
#        CPACK_COMMAND_HDIUTIL
#        CPACK_COMMAND_SETFILE
#        CPACK_COMMAND_REZ
        list(APPEND CPACK_GENERATOR DragNDrop)
    else()
        # Set up DEB
        # TODO: Find a more robust means of detecting whether debian packaging
        # should be enabled
        # Note that readelf is not strictly necessary but platform is assumed
        # Debian-ish if it's present
        find_program(READELF_EXECUTABLE readelf)
        if(READELF_EXECUTABLE)
            set(CPACK_DEBIAN_PACKAGE_DESCRIPTION
"${CPACK_PACKAGE_DESCRIPTION_SUMMARY}
Zofu (Zofu is Object-oriented Fortran Unit-testing) is a framework for
carrying out unit testing of Fortran code modules. As the name
suggests, it makes use of the object-oriented features of Fortran 2003.")
            # set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "any")
            # Auto-detect dependencies
            set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)
            # Build multiple packages
            set(CPACK_DEB_COMPONENT_INSTALL ON)
            set(CPACK_DEBIAN_ENABLE_COMPONENT_DEPENDS ON)
            # set(CPACK_DEBIAN_PACKAGE_DEBUG ON)
            # Turn off for executable-only; only needed for packaging libraries
            set(CPACK_DEBIAN_PACKAGE_GENERATE_SHLIBS ON)

            list(APPEND CPACK_GENERATOR DEB)
        endif()

        # Set up RPM
        # TODO: Find a more robust means of detecting whether RPM
        # generator is available
        find_program(RPMBUILD_EXECUTABLE rpmbuild)
        if(RPMBUILD_EXECUTABLE)
            # Needs additional work (maybe?)
            set(CPACK_RPM_PACKAGE_LICENSE "LGPL3")
            set(CPACK_RPM_FILE_NAME RPM-DEFAULT)
            list(APPEND CPACK_GENERATOR RPM)
        endif()
    endif()
endif()

# This must be last
include(CPack)
