! ---------------------------------------------------------------------
! This simple Hello World program will have the master processor (with ID 0)
! print a statement to console stating that it is the master. All child
! processors (that do not have the ID 0) will print a hello world statement
! to the console and also state their unique ID.
! ---------------------------------------------------------------------

program HELLOWORLD
use mpi
IMPLICIT NONE

! ---------------------------------------------------------------------
! We declare a few variables here
! ---------------------------------------------------------------------

integer :: ierr,num_procs,my_id
integer status(MPI_STATUS_SIZE)

! ---------------------------------------------------------------------
! Initialize MPI and use MPI_COMM_RANK to determine individual processor
! ID's, which will be stored in my_id for each processor.
! MPI_COMM_SIZE will determine how many processors are available for use
! and store that integer in num_procs. The -np flag on the mpirun command
! will allow you to specify how many processors to use.
! ---------------------------------------------------------------------

CALL MPI_INIT(ierr)
CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_id, ierr)
CALL MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)

! ---------------------------------------------------------------------
! Master processor (with ID 0) will write a Hello World statement to the
! console claiming that it is the master.
! ---------------------------------------------------------------------

	if (my_id==0) then

       	write(*,*) 'Hello world from the master processor!'


! ---------------------------------------------------------------------
! Slave processors (processors that do not have the ID 0) will then print
! a Hello World statement to the console and also display what their unique
! ID is.
! ---------------------------------------------------------------------

	else

		write(*,*) 'Hello world from processor: ', my_id

	end if

! ---------------------------------------------------------------------
! We call MPI_FINALIZE to end the MPI program.
! The program HELLOWORLD is also ended.
! ---------------------------------------------------------------------

CALL MPI_FINALIZE(ierr)

stop

end Program HELLOWORLD
