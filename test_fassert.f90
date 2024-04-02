#include "fassert.fpp"
program test_assert
implicit none
integer, parameter :: a = 1, b=2

print *, "Line number: ", __LINE__
print *, "Source file: ", __FILE__

DEBUG("here at this line")

VERBOSE(.true., "important message")

WARNING(a==b, "Both variables should be equal")

ASSERT(a==b, "This is an assert")

ERROR(a==b, "Both variables should be equal")

print *, "**** ALL DONE ****"
end program
