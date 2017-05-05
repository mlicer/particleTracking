subroutine computeConcentrations(t)
! This routine computes concentrations [kg/m2] of passive tracer, advected
! and diffused by the code particleTracking.f90.
! More info: matjaz.licer@mbss.org

	use ptmModule
	use omp_lib
	implicit none

	
	integer :: t, p, j, i
	real :: cellArea
	real ::  dx, dy, d2r, r2d
	real :: meanlat

	! timing:
	real :: e_time
	integer :: count,clock_start,clock_stop,clock_rate



	d2r = pi/180.;
	r2d = 180./pi ;



	meanlat = sum(ncgrid%lats)/(ncgrid%nLon*ncgrid%nLat)
	
	! do NOT move this to ptmModule.f90! In allocate arrays, ncgrid is not allocated yet! 
	if (.not.allocated(slick%pollutantConcentration))&
	allocate(slick%pollutantConcentration( ncgrid%nLon, ncgrid%nLat, ncgrid%nLevels, setup%numHoursOfTracking ))
	 
	slick%pollutantConcentration(:,:,setup%level,t) = 0.0   

	cellArea = (Rearth * ncgrid%resolution * d2r)**2 * cos(d2r * meanlat)

! openmp loop iterator is private by default
	!$omp parallel &
	!$omp   shared ( setup, particles) &
	!$omp   private ( i, j, ncgrid )
	!$omp do
	do p = 1,totalNumberOfParticles
		if ( (particles%lats(t,p) .gt. 0.0) .and. (particles%lons(t,p) .gt. 0.0) ) then

			call get_closest_point(particles%lats(t,p),particles%lons(t,p),j,i)

			slick%pollutantConcentration(i,j,setup%level,t) = &
			slick%pollutantConcentration(i,j,setup%level,t) + particles%mass(t,p)	
		endif
	enddo !p = 1,setup%numberOfParticles
	!$omp end do
	!$omp end parallel

	
	where(slick%pollutantConcentration(:,:,setup%level,t) > 0.0 .and. &
	ncgrid%lsm(:,:,setup%level) > 0 )
		slick%pollutantConcentration(:,:,setup%level,t) = &
		slick%pollutantConcentration(:,:,setup%level,t) / cellArea
	elsewhere
		slick%pollutantConcentration(:,:,setup%level,t) = 0.0
	end where

	
end subroutine computeConcentrations
