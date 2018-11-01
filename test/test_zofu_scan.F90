program test_zofu_scan

  !! Test Zofu unit test module scanning.

  use zofu
  use zofu_scan

  implicit none
  type(unit_test_type) :: test

  call test%init()

  call test%run(test_subroutine_roles)
  call test%run(test_modules)
  
  call test%summary()
  if (test%failed) stop 1

contains

!------------------------------------------------------------------------

  subroutine subroutine_role_test(test, name, role)
    ! Test a subroutine role

    class(unit_test_type), intent(in out) :: test
    character(len = *), intent(in) :: name
    integer, intent(in) :: role
    ! Locals:
    integer :: sub_role

    sub_role = subroutine_role(name)
    call test%assert(sub_role, role, name)

  end subroutine subroutine_role_test

!------------------------------------------------------------------------

  subroutine test_subroutine_roles(test)
    ! Subroutine role assignments

    class(unit_test_type), intent(in out) :: test

    call subroutine_role_test(test, "foo()", SUB_ROLE_UNKNOWN)
    call subroutine_role_test(test, "test_abc", SUB_ROLE_TEST)
    call subroutine_role_test(test, "test_abc()", SUB_ROLE_TEST)
    call subroutine_role_test(test, "setup", SUB_ROLE_SETUP)
    call subroutine_role_test(test, "TEARDOWN", SUB_ROLE_TEARDOWN)
    call subroutine_role_test(test, "test_setup", SUB_ROLE_TEST)
    call subroutine_role_test(test, "test_teardown", SUB_ROLE_TEST)
    call subroutine_role_test(test, "foo_test", SUB_ROLE_UNKNOWN)
    call subroutine_role_test(test, "setup_test", SUB_ROLE_UNKNOWN)
    call subroutine_role_test(test, "setup_bar", SUB_ROLE_UNKNOWN)

  end subroutine test_subroutine_roles

!------------------------------------------------------------------------

  subroutine module_test(test, filename, module_name, num_subroutines, &
       setup, teardown, err)
    ! Test a module

    class(unit_test_type), intent(in out) :: test
    character(len = *), intent(in) :: filename
    character(len = *), intent(in) :: module_name
    integer, intent(in) :: num_subroutines
    logical, intent(in) :: setup, teardown
    logical, intent(in) :: err
    ! Locals:
    type(test_module_type) :: test_module
    integer :: ierr

    ierr = test_module%init(filename)
    call test%assert(ierr /= 0, err, filename // " error")
    call test%assert(test_module%name, module_name, filename // " name")
    call test%assert(test_module%test_subroutines%count, num_subroutines, &
         filename // " subroutine count")
    call test%assert(test_module%setup, setup, filename // " setup")
    call test%assert(test_module%teardown, teardown, filename // " teardown")
    
  end subroutine module_test

!------------------------------------------------------------------------

  subroutine test_modules(test)
    ! Test modules

    class(unit_test_type), intent(in out) :: test

    call module_test(test, "adder_test.F90", "adder_test", 5, &
         .false., .false., .false.)
    call module_test(test, "missing.F90", "", 0, &
         .false., .false., .true.)
    
  end subroutine test_modules
    
!------------------------------------------------------------------------

end program test_zofu_scan
