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
! A simple implemenation of a parser to read properties-like files, i.e. files with lines
! like the following one
! property_name : property_value ! coments
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module fprop
	use fdict, only : dict, add_dict
	implicit none
	private
	public :: read_fprop
	
	logical, parameter :: warning_on = .true.
	logical, parameter :: verbose = .false.
	integer, parameter :: len_record = 256
	character, parameter :: sep_char_fprop = ":"
	character, parameter :: comment_char_fprop = "!"

contains
!=======================================================================
	function open_file(src) result(iunit)
		character(len=*), intent(in) :: src
		integer :: iunit
		integer :: ierr
		
		if (verbose) then
			print *, "(open_file_fprop) Reading file from: ", src 
		end if
		
		!newunit: Fortran 2008 option
		open(newunit=iunit, file=src, status="OLD", form="FORMATTED", action="READ", iostat=ierr)
		if (ierr /= 0) then
			print '(A,A)', "ERROR -- While opening file: ", src 
		end if
	
	end function open_file
!-----------------------------------------------------------------------	

	subroutine close_file(src, iunit, ierr)
		character(len=*), intent(in) :: src
		integer, intent(in) :: iunit
		integer, intent(inout) :: ierr
		
		if (ierr < 0) then
			print *, "ERROR -- closing file early: ", trim(src)
		end if 
		
		if (verbose) then
			print *, "(close_file) -- closing file: ", trim(src)
		end if
		
		close (unit = iunit, iostat = ierr)
		
		if (ierr < 0) then
			print *, "ERROR -- while closing file: ", trim(src)
		end if 
		
	end subroutine close_file
!-----------------------------------------------------------------------		
	
	subroutine split(line, nline, ierr, var_name, var_value)
		character(len=*), intent(in) :: line
		integer, intent(in)          :: nline
		integer, intent(out)         :: ierr
		character(len=*), intent(out) :: var_name
		character(len=*), intent(out) :: var_value
		character(len=len_record) :: buf
		integer :: icomment, isep
		
		!print *, "line: ", trim(line)
		icomment = scan(line, comment_char_fprop, .false.)
		!print *, icomment
		if (icomment > 0) then
			buf = line(1:icomment-1)
		else
			buf = line
		end if
		!print *, "buf: ", trim(buf)
		
		isep = scan(buf, sep_char_fprop, .false.)
		if (isep == 0) then
			print *, "ERROR -- at line ", nline," expected a separator character: <<", sep_char_fprop, ">>" 
		    print *, "   LINE: ", trim(buf)
		    ierr = -1
		    return
		end if
		
		var_name = buf(1:isep-1)
		var_value = buf(isep+1:)
		if (verbose) then
			print *, "  (split)  var_name: ", trim(var_name)
			print *, "  (split)  var_value: ", trim(var_value)
		end if
		ierr = 1
		return
	end subroutine split
!-----------------------------------------------------------------------	

	function read_fprop(src) result (db)
		character(len=*), intent(in) :: src
		type(dict)                   :: db
		integer                      :: iunit, ierr, nline, nchars
		character(len=len_record)    :: buf
		character(len=len_record)    :: var_name, var_value
		real*8 :: variable
		
		iunit = open_file(src)
		
		nline = 0
		do while (.true.)
			read (unit=iunit, fmt='(A)', advance="yes", iostat=ierr) buf
						
			if (ierr /= 0) then
				exit
			end if
			
			nline = nline + 1 
			if (verbose) then
				print '(A,I4,A2, A64)', "(read_fprop)[L", nline,"] ", trim(buf)
			end if 
			
			nchars = len(trim(buf))
			!print *, "nchars: ", nchars
			if (nchars == 0) then
				if (verbose)then
					print *, "(read_fprop) Empty line at line # ", nline
				end if
				cycle
			end if
			
			buf = trim(buf)
			if (buf(1:1) == comment_char_fprop) then
				!print *, "Got a comment,,, ", trim(buf)
				cycle
			end if 
			
			call split(buf, nline, ierr, var_name, var_value)
			if (ierr < 0) then
				call close_file(src, iunit, ierr)
			end if
			
			read(var_value, *) variable
			if (verbose) then
				print '(A,A,A,ES12.3,A)', "(read_fprop) String[", trim(var_value), "]:Float[", variable, "]"  
			end if
			call add_dict(var_name, variable, db)
		end do
		
		ierr = 1
		call close_file(src, iunit, ierr)
	
	end function read_fprop

end module fprop
