program test_str_utils

  ! Test Zofu string utilities.

  use str_utils

  implicit none
  logical :: OK, test
  character(:), allocatable :: a, b

  OK = .true.

  a = "foo"
  b = a
  test = (str_equal(a, b) .eqv. .true.)
  OK = (OK .and. test)

  b = "bar"
  test = (str_equal(a, b) .eqv. .false.)
  OK = (OK .and. test)

  a = "FOO"
  b = "foo"
  test = (str_lower(a) == b)
  OK = (OK .and. test)

  a = "Quick brown fox"
  test = (str_startswith(a, "Quick") .eqv. .true.)
  OK = (OK .and. test)

  test = (str_startswith(a, "fox") .eqv. .false.)
  OK = (OK .and. test)

  test = (str_endswith(a, "fox") .eqv. .true.)
  OK = (OK .and. test)
  
  if (.not. OK) stop 1
  
end program test_str_utils
