!   Copyright 2018 University of Auckland.

!   This file is part of Zofu.

!   Zofu is free software: you can redistribute it and/or modify
!   it under the terms of the GNU Lesser General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.

!   Zofu is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU Lesser General Public License for more details.

!   You should have received a copy of the GNU Lesser General Public License
!   along with Zofu.  If not, see <http://www.gnu.org/licenses/>.

module zofu_scan
  !! Module for scanning unit test module and writing a driver program for it.

  use zofu_str_utils

  implicit none
  private

  integer, parameter, public :: SUB_ROLE_UNKNOWN = 0, SUB_ROLE_TEST = 1, &
       SUB_ROLE_SETUP = 2, SUB_ROLE_TEARDOWN = 3
  integer, parameter :: max_parsed_line_length = 256 !! maximum parsed line length

  type, public :: test_subroutine_type
     !! Unit test subroutine
     private
     character(:), allocatable, public :: name !! Subroutine name
     character(:), allocatable, public :: description !! Subroutine description
     integer, public :: role !! Subroutine role (setup, teardown, test etc.)
     type(test_subroutine_type), pointer :: next !! Pointer to next subroutine in list
   contains
     private
     procedure, public :: init => test_subroutine_init
  end type test_subroutine_type

  type :: subroutine_list_type
     !! Linked list of subroutines
     private
     integer, public :: count !! Number of subroutines
     type(test_subroutine_type), pointer, public :: head !! First subroutine
     type(test_subroutine_type), pointer, public :: tail !! Last subroutine
   contains
     private
     procedure :: init => subroutine_list_init
     procedure :: destroy => subroutine_list_destroy
     procedure :: append => subroutine_list_append
  end type subroutine_list_type

  type, public :: test_module_type
     !! Module of unit tests
     private
     character(:), allocatable, public :: filename !! Module filename
     character(:), allocatable, public :: name !! Module name
     logical, public :: mpi !! If tests are parallelized using MPI
     type(subroutine_list_type), public :: test_subroutines !! List of test subroutines
     logical, public :: setup, teardown !! Whether module has setup / teardown routines
     integer :: unit !! File unit
   contains
     private
     procedure, public :: init => test_module_init
     procedure, public :: destroy => test_module_destroy
     procedure, public :: write_driver => test_module_write_driver
     procedure :: parse => test_module_parse
     procedure :: parse_name => test_module_parse_name
     procedure :: parse_subroutines => test_module_parse_subroutines
     procedure :: add_subroutine => test_module_add_subroutine
  end type test_module_type

  public :: subroutine_role

contains

!------------------------------------------------------------------------

  integer function subroutine_role(name) result(role)
    !! Returns subroutine role, based on its name.

    character(len = *), intent(in) :: name
    ! Locals:
    character(:), allocatable :: lowername

    lowername = trim(adjustl(str_lower(name)))

    if (lowername == 'setup') then
       role = SUB_ROLE_SETUP
    else if (lowername == 'teardown') then
       role = SUB_ROLE_TEARDOWN
    else if (str_startswith(lowername, 'test_')) then
       role = SUB_ROLE_TEST
    else
       role = SUB_ROLE_UNKNOWN
    end if

  end function subroutine_role

!------------------------------------------------------------------------
! Test subroutine methods:
!------------------------------------------------------------------------

  subroutine test_subroutine_init(self, name, description)
    !! Initializes test subroutine.

    class(test_subroutine_type), intent(in out) :: self
    character(len = *), intent(in) :: name
    character(len = *), intent(in) :: description

    self%name = name
    self%description = description
    self%next => null()

  end subroutine test_subroutine_init

!------------------------------------------------------------------------
! Subroutine list methods:
!------------------------------------------------------------------------

  subroutine subroutine_list_init(self)
    !! Initialises subroutine list.

    class(subroutine_list_type), intent(in out) :: self

    self%count = 0
    self%head => null()
    self%tail => null()

  end subroutine subroutine_list_init

!------------------------------------------------------------------------

  subroutine subroutine_list_destroy(self)
    !! Destroys subroutine list.

    class(subroutine_list_type), intent(in out) :: self
    ! Locals:
    type(test_subroutine_type), pointer :: new_head

    do while (associated(self%head))
       new_head => self%head%next
       deallocate(self%head)
       self%head => new_head
    end do

  end subroutine subroutine_list_destroy
  
!------------------------------------------------------------------------

  subroutine subroutine_list_append(self, name, description)
    !! Appends subroutine to list.

    class(subroutine_list_type), intent(in out) :: self
    character(len = *), intent(in) :: name, description

    if (associated(self%tail)) then
       allocate(self%tail%next)
       self%tail => self%tail%next
    else
       allocate(self%tail)
       self%head => self%tail
    end if

    call self%tail%init(name, description)
    self%count = self%count + 1

  end subroutine subroutine_list_append
  
!------------------------------------------------------------------------
! Test module methods:
!------------------------------------------------------------------------

  integer function test_module_init(self, filename, mpi) result(ierr)
    !! Initialises test module, reading from the specified filename.
    !! The optional mpi parameter specifies whether the test module is
    !! parallelized using MPI (default false).

    class(test_module_type), intent(in out) :: self
    character(len = *), intent(in) :: filename
    logical, intent(in), optional :: mpi
    ! Locals
    logical :: use_mpi

    if (present(mpi)) then
       use_mpi = mpi
    else
       use_mpi = .false.
    end if

    self%filename = filename
    self%mpi = use_mpi

    self%setup = .false.
    self%teardown = .false.
    call self%test_subroutines%init()

    ierr = self%parse()

  end function test_module_init

!------------------------------------------------------------------------

  subroutine test_module_destroy(self)
    !! Destroys test module.

    class(test_module_type), intent(in out) :: self

    call self%test_subroutines%destroy()

  end subroutine test_module_destroy

!------------------------------------------------------------------------

  subroutine test_module_parse_name(self)
    !! Parses for name of test module.

    class(test_module_type), intent(in out) :: self
    ! Locals:
    character(len = max_parsed_line_length) :: line, lowerline
    integer :: imodule
    logical :: found
    character(len = 6), parameter :: keyword = "module"

    self%name = ""
    found = .false.
    do while (.not. found)
       read(self%unit, '(a)') line
       lowerline = str_lower(trim(adjustl(line)))
       imodule = index(lowerline, keyword)
       if (imodule == 1) then
          self%name = lowerline(imodule + len(keyword):)
          self%name = trim(adjustl(self%name))
          found = .true.
       end if
    end do

  end subroutine test_module_parse_name

!------------------------------------------------------------------------

  subroutine test_module_add_subroutine(self, name, description)
    !! Adds a subroutine to the test module.

    class(test_module_type), intent(in out) :: self
    character(len = *), intent(in) :: name, description
    ! Locals:
    integer :: role

    role = subroutine_role(name)

    select case (role)
    case (SUB_ROLE_SETUP)
       self%setup = .true.
    case (SUB_ROLE_TEARDOWN)
       self%teardown = .true.
    case (SUB_ROLE_TEST)
       call self%test_subroutines%append(name, description)
    end select
    
  end subroutine test_module_add_subroutine

!------------------------------------------------------------------------

  subroutine test_module_parse_subroutines(self)
    !! Parses test module for subroutines.

    class(test_module_type), intent(in out) :: self
    ! Locals:
    character(len = max_parsed_line_length) :: line, lowerline
    integer :: ios, isub
    character(len = 10), parameter :: keyword = "subroutine"
    character(:), allocatable :: subname, description

    subname = ''
    ios = 0
    do while (ios == 0)
       read(self%unit, '(a)', iostat = ios) line
       if (ios == 0) then
          lowerline = str_lower(trim(adjustl(line)))
          isub = index(lowerline, keyword)
          if (isub == 1) then
             ! Found subroutine:
             subname = lowerline(isub + len(keyword):)
             call strip_brackets(subname)
             subname = trim(adjustl(subname))
             call get_description(subname, description, ios)
             call self%add_subroutine(subname, description)
          end if
       end if
    end do

  contains

    subroutine strip_brackets(name)
      !! Strips any trailing brackets from subroutine name.

      character(:), allocatable, intent(in out) :: name
      ! Locals:
      integer :: ibrac

      ibrac = index(name, '(')
      if (ibrac > 0) then
         name = name(:ibrac - 1)
      end if

    end subroutine strip_brackets

    subroutine get_description(name, description, ios)
      !! Looks for subroutine description comment. If not found, the
      !! description is set equal to the subroutine name.

      character(len = *), intent(in) :: name
      character(:), allocatable, intent(out) :: description
      integer, intent(in out) :: ios
      ! Locals:
      character(len = max_parsed_line_length) :: line
      integer :: icomment
      
      description = ''
      line = ''
      do while (len_trim(line) == 0)
         read(self%unit, '(a)', iostat = ios) line
         if (ios /= 0) exit
      end do
      icomment = index(line, '!')
      if (icomment > 0) then
         description = line(icomment + 1:)
         description = trim(adjustl(description))
      else
         description = name
      end if

    end subroutine get_description

  end subroutine test_module_parse_subroutines

!------------------------------------------------------------------------

  integer function test_module_parse(self) result(ierr)
    !! Parses subroutines in a test module. Returns non-zero error
    !! code if an error occurred while opening the file.

    class(test_module_type), intent(in out) :: self

    open(newunit = self%unit, file = self%filename, &
         status = 'old', iostat = ierr)

    if (ierr == 0) then
       call self%parse_name()
       call self%parse_subroutines()
       close(self%unit)
    end if

  end function test_module_parse

!------------------------------------------------------------------------

  subroutine test_module_write_driver(self, filename)
    !! Writes Zofu driver program for a test module to the specified
    !! filename.

    class(test_module_type), intent(in out) :: self
    character(len = *), intent(in) :: filename !! Filename for driver program
    ! Locals:
    integer :: unit
    character(:), allocatable :: test_type
    type(test_subroutine_type), pointer :: sub

    open(newunit = unit, file = filename, status = 'replace')

    write(unit, '(a/)') "program tests"
    write(unit, '(a)') "  ! Driver program for Zofu unit tests in:"
    write(unit, '(a, a)') "  ! ", trim(adjustl(self%filename))
    write(unit, '(a/)') "  ! Generated by Zofu."

    write(unit, '(a)') "  use zofu"
    if (self%mpi) then
       write(unit, '(a)') "  use zofu_mpi"
    end if
    write(unit, '(a, a/)') "  use ", self%name

    write(unit, '(a/)') "  implicit none"
    if (self%mpi) then
       test_type = "unit_test_mpi_type"
    else
       test_type = "unit_test_type"
    end if
    write(unit, '(a, a, a/)') "  type(", test_type, ") :: test"

    write(unit, '(a/)') "  call test%init()"
    if (self%setup) then
       write(unit, '(a/)') "  call setup()"
    end if

    sub => self%test_subroutines%head
    if (associated(sub)) then
       do 
          write(unit, '(5(a))') "  call test%run(", &
               sub%name, ", '", sub%description, "')"
          sub => sub%next
          if (.not. associated(sub)) exit
       end do
    end if
    write(unit, *)

    write(unit, '(a)') "  call test%summary()"
    if (self%teardown) then
       write(unit, '(a)') "  call teardown()"
    end if
    write(unit, *)

    write(unit, '(a/)') "  if (test%failed) stop 1"
    write(unit, '(a)') "end program tests"
    
    close(unit)

  end subroutine test_module_write_driver

!------------------------------------------------------------------------

end module zofu_scan
