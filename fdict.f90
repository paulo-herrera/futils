!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MIT License
! 
! Copyright (c) 2024 Paulo A. Herrera
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
! 
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!=====================================================================================
! FDICT
! A simple implementation of a dictionary-like container with string keys and
! real values.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module fdict
	use, intrinsic :: ieee_arithmetic
	implicit none
	private
	public :: key_len_dict, add_dict, print_dict, delete_dict, &
	          get_value_dict, in_dict, get_keys_dict, get_values_dict, dict, &
	          operator (.in.), operator (.get.) 

	
	integer, parameter :: key_len_dict = 128
	logical, parameter :: warning_on = .true.
	integer, parameter :: val_prec = SELECTED_REAL_KIND(15)
	
	type record
		character(len=key_len_dict) :: key
		real(val_prec)         :: value
		type(record), pointer  :: next => null()
	end type

	type dict
		type(record), pointer :: head => null()
		type(record), pointer :: tail => null()
		integer               :: len = 0
	end type
		
	interface add_dict
		module procedure add_dict_real
		module procedure add_dict_double
	end interface
	
	interface print_dict
		module procedure print_dict_default
		module procedure print_dict_fmt
	end interface

	interface operator (.in.)
		module procedure in_dict
	end interface 
	
	interface operator (.get.)
		module procedure get_value_dict
	end interface

	
contains
!=======================================================================

	function get_keys_dict(d) result (k)
		implicit none
		type(dict), intent(in) :: d
		character(len=key_len_dict), allocatable :: k(:)
		type(record), pointer  :: current
		integer :: n,i
		
		n = d%len
		allocate(k(n))
		if (.not. associated(d%head)) then
			if (warning_on) then
				print*, "WARNING(delete_dict) -- Empty dictionary"
			end if
			return 
		else
			current => d%head
		end if
		
		i = 1
		do while (associated(current))
			k(i) = trim(current%key)
			i = i + 1
			current => current%next
		end do
		
	end function get_keys_dict
!-----------------------------------------------------------------------	
	
	function get_values_dict(d) result (k)
		implicit none
		type(dict), intent(in) :: d
		real(val_prec), allocatable :: k(:)
		type(record), pointer  :: current
		integer :: n,i
		
		n = d%len
		allocate(k(n))
		if (.not. associated(d%head)) then
			if (warning_on) then
				print*, "WARNING(delete_dict) -- Empty dictionary"
			end if
			return 
		else
			current => d%head
		end if
		
		i = 1
		do while (associated(current))
			k(i) = current%value
			i = i + 1
			current => current%next
		end do
		
	end function get_values_dict
!-----------------------------------------------------------------------	

	subroutine add_dict_real(key, v, d)
		implicit none
		character(len=*), intent(in) :: key
		real, intent(in)             :: v
		type(dict), intent(inout)    :: d
	
		if (.not. associated(d%head)) then
			allocate(d%head)
			d%head%key = key
			d%head%value = real(v, val_prec)
			nullify(d%head%next)
			d%tail => d%head 
		else
			allocate(d%tail%next)
			d%tail%next%key = key
			d%tail%next%value = real(v, val_prec)
			d%tail => d%tail%next
		end if 
		d%len = d%len + 1
		
	end subroutine add_dict_real
!-----------------------------------------------------------------------	
	
	subroutine add_dict_double(key, v, d)
		implicit none
		character(len=*), intent(in) :: key
		double precision, intent(in)             :: v
		type(dict), intent(inout)    :: d
	
		if (.not. associated(d%head)) then
			allocate(d%head)
			d%head%key = key
			d%head%value = real(v, val_prec)
			nullify(d%head%next)
			d%tail => d%head 
		else
			allocate(d%tail%next)
			d%tail%next%key = key
			d%tail%next%value = real(v, val_prec)
			d%tail => d%tail%next
		end if 
		d%len = d%len + 1
		
	end subroutine add_dict_double
!-----------------------------------------------------------------------	
	
	function in_dict(key, d)
		implicit none
		character(len=*), intent(in) :: key
		type(dict), intent(in)       :: d
		logical                      :: in_dict
		type(record), pointer        :: current
		
		in_dict = .false.
	
		if (len(key) > key_len_dict) then
			print*, "WARNING(in_dict) -- key is longer than length of keys in dictionary"
			print*, "WARNING(in_dict) -- returning FALSE"
			in_dict = .false.
		end if 
		
		if (.not. associated(d%head)) then
			if (warning_on) then
				print*, "WARNING(in_dict) -- Empty dictionary"
			end if
			in_dict = .false.
		else
			current => d%head
		end if
		
		do while (.not.(in_dict) .and. associated(current))
			if (trim(current%key) == trim(key)) then 
				in_dict = .true.
			end if
			current => current%next
		end do
		
	end function in_dict
!-----------------------------------------------------------------------	
	
	function get_value_dict(key, d) result(v)
		implicit none
		character(len=*), intent(in) :: key
		type(dict), intent(in)       :: d
		real(val_prec)               :: v
		logical                      :: has_key
		type(record), pointer        :: current
		
		v = 0.0_val_prec
		has_key = in_dict(key, d)
		
		if (has_key) then
			current => d%head
		    do while (associated(current))
				if ( trim(current%key) == trim(key)) then 
					v = current%value
					exit
				end if
				current => current%next
			end do
		else
			v = ieee_value(0.0_val_prec, ieee_quiet_nan )
		end if
		
	end function get_value_dict
!-----------------------------------------------------------------------	
	
	subroutine delete_dict(d)
		implicit none
		type(dict), intent(inout) :: d
		type(record), pointer  :: current
		type(record), pointer  :: next
		
		if (.not. associated(d%head)) then
			if (warning_on) then
				print*, "WARNING(delete_dict) -- Empty dictionary"
			end if
			return 
		else
			current => d%head
			nullify(d%head, d%tail)
			d%len = 0
		end if
		
		do while (associated(current))
			next => current%next
			nullify(current%next)
			deallocate(current)
			current => next
		end do
		
	end subroutine delete_dict
!-----------------------------------------------------------------------	
	
	subroutine print_dict_default(d)
		implicit none
		type(dict), intent(in) :: d
		type(record), pointer  :: current
		
		if (.not. associated(d%head)) then
			if (warning_on) then
				print*, "WARNING(print_dict) -- Empty dictionary"
			end if
			return 
		else
			current => d%head
		end if
		
		do while (associated(current))
			print '(A64, F15.8)', current%key, current%value
			current => current%next
		end do
	
	end subroutine print_dict_default
!-----------------------------------------------------------------------	

	subroutine print_dict_fmt(d, format)
		implicit none
		type(dict), intent(in) :: d
		type(record), pointer  :: current
		character(len=*), intent(in) :: format
		
		if (.not. associated(d%head)) then
			if (warning_on) then
				print*, "WARNING(print_dict) -- Empty dictionary"
			end if
			return 
		else
			current => d%head
		end if
		
		do while (associated(current))
			print format, current%key, current%value
			current => current%next
		end do
	
	end subroutine print_dict_fmt
!-----------------------------------------------------------------------
end module
