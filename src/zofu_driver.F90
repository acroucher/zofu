program zofu_driver
  !! Program for writing driver program for module of Zofu unit tests.

  use zofu_scan
  use str_utils

  implicit none
  integer :: num_args, ierr
  character(:), allocatable :: module_filename, driver_filename, mpi_str
  type(test_module_type) :: test_module
  logical :: use_mpi

  num_args = command_argument_count()

  if (num_args < 2) then

     write(*, '(a)') "Usage:"
     write(*, '(a)') "  zofu-driver module driver [--mpi]"
     write(*, '(a)') "  module: filename of test module"
     write(*, '(a/)') "  driver: filename for output test driver program"
     write(*, '(a)') "Options:"
     write(*, '(a)') "  --mpi: if test module is parallelized using MPI"
     write(*, '(a)') "  -h, --help: show help options"

  else

     module_filename = get_argument(1)
     driver_filename = get_argument(2)

     if (num_args == 3) then
        mpi_str = get_argument(3)
        use_mpi = str_equal(mpi_str, "--mpi")
     else
        use_mpi = .false.
     end if

     ierr = test_module%init(module_filename, use_mpi)
     if (ierr == 0) then
        call test_module%write_driver(driver_filename)
        call test_module%destroy()
     else
        stop "Error opening module file " // module_filename // "."
     end if

  end if

contains

  function get_argument(number) result(arg)
    !! Reads command line argument with specified number into an
    !! allocatable character variable.

    integer, intent(in) :: number
    character(:), allocatable :: arg
    ! Locals:
    integer :: arg_length

    call get_command_argument(number, length = arg_length)
    allocate(character(arg_length):: arg)
    call get_command_argument(number, value = arg)

  end function get_argument

end program zofu_driver
