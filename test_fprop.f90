program test_fprop
use fprop
use fdict, only: dict, print_dict
implicit none

type(dict) :: db

	db = read_fprop("test_fprop1.data")
	print *, "Read database: "
	call print_dict(db)
	
	! should fail, wrong format
	db = read_fprop("test_fprop2.data")
	
	! should fail, missing file
	db = read_fprop("test_fprop3.data")

	print *, "**** TEST_FPROP: ALL DONE ****"
end program 
