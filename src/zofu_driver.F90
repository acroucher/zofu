program zofu_driver
  !! Program for writing driver program for module of Zofu unit tests.

  use zofu_scan
  use str_utils

  implicit none
  integer :: num_args, ierr
  integer, parameter :: max_argument_length = 256
  character(len = max_argument_length) :: module_filename, driver_filename, mpi_str
  type(test_module_type) :: test_module
  logical :: use_mpi

  num_args = command_argument_count()

  if (num_args < 2) then

     write(*, '(a)') "Usage:"
     write(*, '(a/)') "  zofu-driver module driver [--mpi]"
     write(*, '(a)') "  module: filename of test module"
     write(*, '(a/)') "  driver: filename for output test driver program"
     write(*, '(a)') "Options:"
     write(*, '(a)') "  --mpi: if test module is parallelized using MPI"
     write(*, '(a)') "  -h, --help: show help options"

  else

     call get_command_argument(1, value = module_filename)
     call get_command_argument(2, value = driver_filename)

     if (num_args == 3) then

        call get_command_argument(3, value = mpi_str)
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

end program zofu_driver
