 ! -------------------------create particles ---------------------------

  subroutine createParticles(t)
	
	use ptmModule
	implicit none

	!    type(grid) :: ncgrid
	integer :: i, j, k, imax, t, point, particleIndex
	real :: meanDistanceBetweenReleasedParticles, polygonLength, distance
	real :: releasePointDensity, d2r, r2d
	integer :: numberOfPolygonSegments, numberOfParticlesInPreviousSegments
	integer, allocatable :: numberOfParticlesPerSegment(:)
	real, allocatable :: random_lon_vector(:),random_lat_vector(:),random_sign_vector(:)
	real, allocatable :: random_vector(:)
	integer, allocatable :: seed(:)
	real, allocatable :: polygonSegmentLength(:)
	real, parameter :: initialSpread=0.1
	d2r = pi/180.;
	r2d = 180./pi ;
	
		if (t.lt.setup%releaseDuration) then
			imax = setup%numCreatedParticlesPerStep
		else
			imax = (setup%numberOfParticles* setup%numberOfReleasePoints - particles%numOfExistingParticles)/ setup%numberOfReleasePoints
		endif
		print *,'1',imax
	
		
	if(.not.allocated(random_lon_vector))allocate(random_lon_vector(imax))
	if(.not.allocated(random_lat_vector))allocate(random_lat_vector(imax))
	if(.not.allocated(random_sign_vector))allocate(random_sign_vector(imax))
	if(.not.allocated(random_vector))allocate(random_vector(imax))
	if(.not.allocated(polygonSegmentLength)) &
	allocate(polygonSegmentLength(setup%numberOfReleasePoints-1))	
	if(.not.allocated(numberOfParticlesPerSegment)) &
	allocate(numberOfParticlesPerSegment(setup%numberOfReleasePoints-1))		
	
	
	call generate_random_vector(imax,random_lon_vector)
	call generate_random_vector(imax,random_lat_vector)
	call generate_random_vector(imax,random_sign_vector)
	call generate_random_vector(imax,random_vector)

	call random_number(random_sign_vector)
	where(random_sign_vector>0.5)
		random_lon_vector=-random_lon_vector
	end where

	call random_number(random_sign_vector)
	where(random_sign_vector>0.5)
		random_lat_vector=-random_lat_vector
	end where	


! ----------------------------------------------------------------------	
! ------------------SINGLE POINT----------------------------------------	
! ----------------------------------------------------------------------	
	if (setup%releaseType=='singlePoint') then

		do i=1,imax	
			particleIndex = (t-1) * setup%numCreatedParticlesPerStep+i
			
			particles%lats(t,particleIndex)=&
			setup%releasePointsLats(1) + initialSpread*ncgrid%resolution*random_lat_vector(i)

			particles%lons(t,particleIndex)=&
			setup%releasePointsLons(1) + initialSpread*ncgrid%resolution*random_lon_vector(i)

			particles%levels(t,particleIndex)=&
			setup%level

			particles%hourOfBirth(particleIndex) = t

			particles%mass(t,particleIndex) = &
			setup%totalMass/setup%numberOfParticles

			! set maximum evaporation rate for each particle:
			if (random_vector(i)<setup%completeEvapPercentage) then
				particles%totalEvaporationFlag(particleIndex)=.true.
			else
				particles%totalEvaporationFlag(particleIndex)=.false.
			endif

		enddo

		particles%numOfExistingParticles = &
		particles%numOfExistingParticles + imax

! ----------------------------------------------------------------------	
! -----------------MULTIPLE POINTS--------------------------------------	
! ----------------------------------------------------------------------			
	elseif (setup%releaseType=='multiplePoints') then

	do point = 1,setup%numberOfReleasePoints

		! now indexing must take into account more release points:	
			do i=1,imax
			
				particleIndex = particles%numOfExistingParticles+i				
				
				particles%lats(t,particleIndex)=&
				setup%releasePointsLats(point) + initialSpread*ncgrid%resolution*random_lat_vector(i)

				particles%lons(t,particleIndex)=&
				setup%releasePointsLons(point) + initialSpread*ncgrid%resolution*random_lon_vector(i)

				particles%levels(t,particleIndex)=&
				setup%level

				particles%hourOfBirth(particleIndex) = t

				particles%mass(t,particleIndex) = &
				setup%totalMass/setup%numberOfParticles

				! set maximum evaporation rate for each particle:
				if (random_vector(i)<setup%completeEvapPercentage) then
					particles%totalEvaporationFlag(particleIndex)=.true.
				else
					particles%totalEvaporationFlag(particleIndex)=.false.
				endif

			enddo

			particles%numOfExistingParticles = &
			particles%numOfExistingParticles + imax	
		
	 enddo

! ----------------------------------------------------------------------	
! ----------------POLYGON-----------------------------------------------	
! ----------------------------------------------------------------------	
	elseif (setup%releaseType=='polygon') then
	
		numberOfPolygonSegments = setup%numberOfReleasePoints-1
		! compute total length of polygon sides:
		polygonLength = 0.0
		if (numberOfPolygonSegments.lt.1) then
			print *,"createParticles.f90 ERROR: polygon releaseType requires at least 2 release points (= 1 segment)!"
			stop
		else
			do i = 1,numberOfPolygonSegments
				! compute distance between neighbouring points:

								
				call points_dist_sphere( releasePointsLats(i)*d2r, releasePointsLons(i)*d2r, &
									releasePointsLats(i+1)*d2r, releasePointsLons(i+1)*d2r, Rearth, distance )
									
				! add to total polygon length:
				polygonSegmentLength(i) = distance
				polygonLength = polygonLength +  distance
			enddo
		endif
	
		! determine number of particles per segment:
		if (numberOfPolygonSegments.eq.1) then
		
			numberOfParticlesPerSegment(1) = setup%numberOfParticles
			particles%numOfExistingParticles = setup%numberOfParticles
			
		else
			! mean initial distance [in metres] between released particles:
			meanDistanceBetweenReleasedParticles = polygonLength / setup%numberOfParticles
					
			do i = 1,numberOfPolygonSegments
			
				if (i.lt.numberOfPolygonSegments) then
					numberOfParticlesPerSegment(i) = nint(polygonSegmentLength(i)/meanDistanceBetweenReleasedParticles)
				else
					numberOfParticlesPerSegment(i) = setup%numberOfParticles - particles%numOfExistingParticles
				endif
				
				particles%numOfExistingParticles = &
				particles%numOfExistingParticles +  numberOfParticlesPerSegment(i)			
						
			enddo	
				
		endif


		! create particles along the sides of the polygon:
		do i = 1,numberOfPolygonSegments		
			
			do j = 1,numberOfParticlesPerSegment(i) 
				if (i.gt.1) then
					numberOfParticlesInPreviousSegments = 0
					do k=1,i-1
						numberOfParticlesInPreviousSegments = &
						numberOfParticlesInPreviousSegments + numberOfParticlesPerSegment(k)
					enddo
					particleIndex = numberOfParticlesInPreviousSegments + j
				else
					numberOfParticlesInPreviousSegments = 0
					particleIndex = j
				endif


								
				! lats:
				releasePointDensity = (setup%releasePointsLats(i+1) - setup%releasePointsLats(i)) / numberOfParticlesPerSegment(i)
				particles%lats(t,particleIndex)= setup%releasePointsLats(i) + releasePointDensity * j 

				! lons:
				releasePointDensity = (setup%releasePointsLons(i+1) - setup%releasePointsLons(i)) / numberOfParticlesPerSegment(i)
				particles%lons(t,particleIndex)= setup%releasePointsLons(i) + releasePointDensity * j
				
				! levels:
				particles%levels(t,particleIndex)=&
				setup%level

				! hours of birth:
				particles%hourOfBirth(particleIndex) = t

				! mass:
				particles%mass(t,particleIndex) = &
				setup%totalMass/setup%numberOfParticles
				
			enddo
		enddo	

		! set maximum evaporation flag for each particle:
		deallocate(random_vector)
		allocate(random_vector(particles%numOfExistingParticles))

		particles%totalEvaporationFlag=.false.
		where (random_vector < setup%completeEvapPercentage )
			particles%totalEvaporationFlag=.true.
		endwhere

		deallocate(random_vector)	
							
	else
		print *,"CREATE_PARTICLES ERROR: invalid release type."
		stop
	endif
	
  end subroutine createParticles
