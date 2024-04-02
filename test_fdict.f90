program test1
use fdict
implicit none


type(dict) :: d
logical    :: has_record = .false.
real*8     :: var
character(len=key_len_dict), allocatable :: keys(:)
real*8, allocatable :: values(:)
integer    :: n, i

	call print_dict(d)
	call add_dict("item1", 0.1, d)
	call add_dict("item2", 0.2, d)
	call add_dict("item3", 0.3d0, d)

	call print_dict(d)
	call print_dict(d, '(A16,F8.3)')
	
	has_record = in_dict("item1", d)
	print '(A,L)', "*item1* is in dict: ", has_record
	has_record = in_dict("item", d)
	print '(A,L)', "*item* is in dict: ", has_record
	has_record = in_dict("item11", d)
	print '(A,L)', "*item11* is in dict: ", has_record
	
	print '(A, L)', "*item1* .in. d: ", ("item1" .in. d)
	print '(A, L)', "*item* .in. d: ", ("item" .in. d)
	
	var = get_value_dict("item1", d)
	print '(A, F15.8)', "value of *item1*: ", var
	var = get_value_dict("item11", d)
	print '(A, F15.8)', "value of *item11*: ", var
	
	print '(A, F15.8)', "value of (*item1* .get. d): ", ("item1" .get. d)
	print '(A, F15.8)', "value of (*item11* .get. d): ", ("item11" .get. d)
	
	
	keys = get_keys_dict(d)
	print '(A, I3)', "len(keys): ", size(keys)
	do i = 1,size(keys)
		print '(A)', trim(keys(i))
	end do
	
	
	values = get_values_dict(d)
	print '(A, I3)', "len(values): ", size(values)
	do i = 1,size(values)
		print '(F15.8)', values(i)
	end do
	
	call delete_dict(d)
	call print_dict(d)
	
	print*, "**** TEST_FDICT: ALL DONE ****"

end program
