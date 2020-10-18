# Synopsis:
#
# add_zofu_acceptance_test() is a convenience function to build
# acceptance test binaries for Zofu and schedule them in CTest.
#
# Usage:
#
# add_zofu_acceptance_test(
#    TARGET testname
#    SOURCES list_of_source_files
#    [WILL_FAIL]
#    [DEBUG]
# )
#
# Before calling this function, ZOFU_LIB_NAME and
# ZOFU_FORTRAN_MODULE_DIR should be set in the calling environment
#
# Arguments:
#
# - TARGET specifies the test target and executable name. Single value,
#   required.
# - SOURCES indicates a list of source files required to build the test
#   application. Multi-value, required.
# - WILL_FAIL - if present as an argument, WILL_FAIL will be set to
#   TRUE for the test (i.e. test is expected to fail). No value.
#   Optional; default is WILL_FAIL FALSE (test expected to pass)
# - DEBUG - if present, this function will print out the parsed argument
#   values for troubleshooting. No value. Optional.
#
function(add_zofu_acceptance_test)
    # Define the supported set of keywords
    set(prefix ARG)
    set(noValues WILL_FAIL DEBUG)
    set(singleValues TARGET ENVIRONMENT)
    set(multiValues SOURCES)

    cmake_parse_arguments(
        ${prefix}
        "${noValues}" "${singleValues}" "${multiValues}"
        ${ARGN}
    )

    if(${ARG_DEBUG})
        # Log details for each supported keyword
        message(STATUS "Option summary:")
        foreach(arg IN LISTS noValues)
            if(${prefix}_${arg})
                message(" ${arg} enabled")
            else()
                message(" ${arg} disabled")
            endif()
        endforeach()

        foreach(arg IN LISTS singleValues multiValues)
            # Single argument values will print as a string
            # Multiple argument values will print as a list
            message(" ${arg} = ${${prefix}_${arg}}")
        endforeach()
    endif()

    set(TEST_FORTRAN_MODULE_DIR "${CMAKE_CURRENT_BINARY_DIR}/${ARG_TARGET}_include")
    file(MAKE_DIRECTORY "${TEST_FORTRAN_MODULE_DIR}")
    add_executable(${ARG_TARGET} ${ARG_SOURCES})
    set_target_properties(
        ${ARG_TARGET}
        PROPERTIES
        OUTPUT_NAME ${ARG_TARGET}
        DEPENDS ${ZOFU_LIB_NAME}
        Fortran_MODULE_DIRECTORY ${TEST_FORTRAN_MODULE_DIR}
        INCLUDE_DIRECTORIES ${ZOFU_FORTRAN_MODULE_DIR}
        LINK_LIBRARIES ${ZOFU_LIB_NAME}
    )

    add_test(NAME ${ARG_TARGET}
        COMMAND $<TARGET_FILE:${ARG_TARGET}>
        CONFIGURATIONS Debug Release ""
    )

    if(${ARG_WILL_FAIL})
        set_tests_properties(${ARG_TARGET} PROPERTIES
            WILL_FAIL TRUE)
    endif()

endfunction()