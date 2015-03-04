! ---------------------------------------------------------------------
! This more advanced Hello World program will have each processor print
! a Hello World statement to the console, as well as have each child processor
! run a Fortran subroutine that generates two uniform random numbers and
! pass them back to the master processor. The master processor will then
! write the data to a text file. This entire process will be done five times.
! ---------------------------------------------------------------------

program HELLOWORLD
use mpi
IMPLICIT NONE

! ---------------------------------------------------------------------
! Declare all variable types below
! ---------------------------------------------------------------------

integer :: ierr,num_procs,my_id,i,j
integer status(MPI_STATUS_SIZE)
real(8) :: random1, random2
real,dimension(2) :: mpi_values

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
! We open a data stream to the file "hello_world_random_numbers_output.dat"
! and allow the filled to be added to with the ACCESS='APPEND' statement.
! ---------------------------------------------------------------------

OPEN(17, file="hello_world_random_numbers_output.dat", ACCESS = 'APPEND')

do i = 1,5

! ---------------------------------------------------------------------
! Master - determines that its ID is 0 and executes the following code
! ---------------------------------------------------------------------

	if (my_id == 0) then

! ---------------------------------------------------------------------
! The master processor will write a Hello World statement to the console
! announcing that it is in fact the master processor
! ---------------------------------------------------------------------

	write(*,*) 'Hello world from the master processor!'

! ---------------------------------------------------------------------
! The following do loop will have the master processor be ready to receive
! data from all the child processors (num_procs-1)
! ---------------------------------------------------------------------

    	do j = 1,(num_procs-1)

! ---------------------------------------------------------------------
! The MPI_RECV statement. The master will "listen" in a sense for incoming
! data transmissions from the child processors. It will then write the data
! it receives to file.
! ---------------------------------------------------------------------

        	CALL MPI_RECV(mpi_values,2,MPI_REAL,MPI_ANY_SOURCE,i,MPI_COMM_WORLD,status,ierr)
       		write(17,*) i,mpi_values(1),mpi_values(2)

    	end do

! ---------------------------------------------------------------------
! Slaves - all processors that do not have the ID 0. They begin with writing
! to the console their Hello World statement along with their unique ID.
! Next, we call the Hello Fortran subroutine which will share the my_id value,
! and allow sharing of the random1 and random2 real numbers that we generate.
! ---------------------------------------------------------------------

	else

    write(*,*) 'Hello world from processor: ', my_id

    CALL Hello(my_id,random1,random2)

! ---------------------------------------------------------------------
! We fill an array with the two random numbers that we generated in the
! subroutine. Remember, each processor that is not the master will be doing
! this simultaneously, and the variables random1, random2, and my_id will
! each have their own instance on each processor. This means that random1
! that is generated on processor 8 will not share the same memory location
! as random1 that is generated on processor 3.
! ---------------------------------------------------------------------

    mpi_values(1) = random1
    mpi_values(2) = random2

! ---------------------------------------------------------------------
! The MPI_SEND statement. We send the data from each processor to the master.
! ---------------------------------------------------------------------

    CALL MPI_SEND(mpi_values,2,MPI_REAL,0,i,MPI_COMM_WORLD,ierr)

	end if

end do

! ---------------------------------------------------------------------
! End of loop, and we call MPI_FINALIZE to end the MPI program.
! The program HELLOWORLD is also ended.
! ---------------------------------------------------------------------

CALL MPI_FINALIZE(ierr)

stop

end Program HELLOWORLD

! ---------------------------------------------------------------------
! Subroutine - each non-master processor will run an instance of this
! code. We define the shared variables random1, random2, and my_id (these
! are shared in the sense that they can be passed from the subroutine to
! the primary executing code, within the SAME processor). We generate 
! the idum value (a seed value) which will be different for each processor.
! We use this seed value to call the rand1 function, which generates two
! random numbers for us. Since random1 and random2 are shared variables in
! our subroutine, the processor executing this instance of the code will
! be able to use that data and package it into an array before calling the
! MPI_SEND subroutine.
! ---------------------------------------------------------------------

subroutine Hello(my_id,random1,random2)

real(8) :: random1, random2
integer :: idum,my_id

idum = -1-my_id

random1 = ran1(idum)
random2 = ran1(idum)

end subroutine Hello

! ---------------------------------------------------------------------
! Random number generator function
! ---------------------------------------------------------------------

  FUNCTION ran1(idum)
  INTEGER idum,IA,IM,IQ,IR,NTAB,NDIV
     REAL ran1,AM,EPS,RNMX
     PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
     INTEGER j,k,iv(NTAB),iy
     SAVE iv,iy
     DATA iv /NTAB*0/, iy /0/
     if (idum.le.0.or.iy.eq.0) then 
        idum=max(-idum,1) 
        do j=NTAB+8,1,-1 
           k=idum/IQ
           idum=IA*(idum-k*IQ)-IR*k
           if (idum.lt.0) idum=idum+IM
           if (j.le.NTAB) iv(j)=idum
        enddo 
        iy=iv(1)
     endif 
     k=idum/IQ 
     idum=IA*(idum-k*IQ)-IR*k 
     if (idum.lt.0) idum=idum+IM 
     j=1+iy/NDIV 
     iy=iv(j) 
     iv(j)=idum 
     ran1=min(AM*iy,RNMX) 
     return
     END

! ---------------------------------------------------------------------
! End
! ---------------------------------------------------------------------
