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
! A set of preprocessor macros to include assert, warning, etc, messages.
! ASSERT, WARNING and other messages can be turnoff at compilation with -DASSERT_OFF, -DWARNING_OFF, etc.
! Look at the provided Makefile for details.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!Logical errors that should always end program execution.
#define ERROR(COND, MSG) if (.not. COND) then;	print '(A,A,A,I5,A,A,A,A)', '*ERROR*  ', __FILE__, "[Line", __LINE__, "]  <<", "COND", ">>  ", MSG; call abort; end if

!Test of expected conditions. It may impact performance, hence it is useful to be able to turn them off.
#ifndef ASSERT_OFF
#define ASSERT(COND, MSG) if (.not. COND) then;	print '(A,A,A,I5,A,A,A,A)', '*ASSERT*  ', __FILE__, "[Line", __LINE__, "]  <<", "COND", ">>  ", MSG; call abort; end if
#else
#define ASSERT(COND,MSG) 
#endif

!Warnings, e.g. unexpected input.
#ifndef WARNING_OFF
!#define WARNING(COND, MSG) if (.not. COND) then; print '(A,A,A,I5,A,A,A,A)', '*WARNING*  ', __FILE__, "[Line", __LINE__, "]  <<", "COND", ">>  ", MSG; end if
#define WARNING(COND, MSG) if (.not. COND) then; print '(A,A,A,I5,A,A)', '*WARNING*  ', __FILE__, "[Line", __LINE__, "]  ", MSG; end if
#else
#define WARNING(COND,MSG) 
#endif

!Useful to check intermediate steps during program execution.
#ifndef VERBOSE_OFF
#define VERBOSE(COND, MSG) if (COND) then; print '(A,A,A,I5,A,A)', '*VERBOSE*  ', __FILE__, "[Line", __LINE__, "]  ", MSG; end if
#else
#define VERBOSE(COND,MSG) 
#endif

!Useful during development.
#ifndef DEBUG_OFF
#define DEBUG(MSG) print '(A,A,A,I5,A,A)', '*DEBUG*  ', __FILE__, '[Line', __LINE__, ']  ', MSG
#else
#define DEBUG(MSG) 
#endif
