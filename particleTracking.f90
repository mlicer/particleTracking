program particleTracking
! Code performs Lagrangian particle tracking method (PTM) and weathering 
! computation for oil-type of particles. It uses ADRIPOM or NADRIPOM  
! circulation results. See setupPTM.py to see how to run the model.
! 
! Model input: 
! *) ADRIPOM NetCDF's
! *) init.nml namelist (created by setupPTM.py preprocessing script)
! Model output a NetCDF, determined in the following line of 
! the writeOutput.f90 subroutine:
! 
! call handle_err( nf_create('ptm_output_'//setup%startdate//'.nc', NF_CLOBBER, outfile_id) )
!
! Prerequisites:
! *) Python
! *) NumPy
! *) F90 netCDF
! *) openMP library
!
! Modify Makefile for F90 compiler, Includes, and Libraries.
! More info: matjaz.licer@nib.si
! September 2015

	use ptmModule
	use omp_lib
	implicit none
	
	integer :: p,timestep

	! timing:
	real :: e_time
	integer :: count,clock_start,clock_stop,clock_rate

	! open MP book-keeping:
	proc_num = omp_get_num_procs ( )
	thread_num = omp_get_max_threads ( )

	write ( *, '(a)' ) ' '
	write ( *, '(a,i8)' ) ' OPENMP: The number of processors available = ', proc_num
	write ( *, '(a,i8)' ) ' OPENMP: The number of threads available    = ', thread_num

	call initialize    
	
	call readOceanModelInput

	call readAtmosphere
	
	! MAIN TIME LOOP: --------------------------------------------------
	do timestep = 1,setup%numHoursOfTracking


		if (timestep.le.setup%releaseDuration) then
			call createParticles(timestep)
		endif

		
		print *, "PTM computation for ",&
		particles%numOfExistingParticles," particles: step ",&
		timestep,"/",setup%numHoursOfTracking		
		
		! MAIN PARTICLE LOOP: ------------------------------------------
		call system_clock(count_rate=clock_rate) !Find the time rate
		call system_clock(count=clock_start)     !Start Timer
		!$omp parallel &
		!$omp shared (setup, particles, current, ncgrid) &
		!$omp private (p)
		!$omp do
		do p = 1,particles%numOfExistingParticles

			if (setup%tracking3D) then
				call advectionDiffusion3D(timestep,p)	
			else		
				call advectionDiffusion2D(timestep,p)			
			endif	

			call fate(timestep,p) 				
							
		enddo !p = 1,setup%numberOfParticles		
		!$omp end do
		!$omp end parallel
		
		call system_clock(count=clock_stop)      ! Stop Timer
		e_time = real(clock_stop-clock_start)/real(clock_rate)

! 		print *, "NUMBER OF openMP THREADS:",thread_num
! 		print *, "EXECUTION TIME FOR PARTICLE LOOP:",e_time


		call computeConcentrations(timestep)
		
	enddo !t = 2,setup%numHoursOfTracking

			
	if (setup%tracking3D) then
		call writeOutput3D
	else
		call writeOutput2D
	endif

end program particleTracking
