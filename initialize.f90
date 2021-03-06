subroutine initialize
	use ptmModule
	implicit none
	integer :: i
	
	open(1,file='init.nml',status='old') 
	read(1,NML=initialParameters)
	
	
	setup%model=model
	setup%modelRuntimeInHours=modelRuntimeInHours
	setup%dt=dt
	setup%numHoursOfTracking=numHoursOfTracking
	setup%releaseDuration=releaseDuration
	setup%performBacktracking=performBacktracking
	setup%numberOfParticles=numberOfParticles
	setup%numCreatedParticlesPerStep=numCreatedParticlesPerStep
	setup%totalMass=totalMass
	setup%oilIndex=oilIndex
	setup%completeEvapPercentage=completeEvapPercentage
	setup%evapThreshold=evapThreshold	
	setup%useOceanModelDiffusivity=useOceanModelDiffusivity
	setup%Dh=Dh
	setup%numOfExistingFiles=numOfExistingFiles
	setup%level=level
	setup%tracking3D=tracking3D	
	setup%sign_t=sign_t
	setup%startdate=startdate
	setup%enddate=enddate
	setup%releaseType=releaseType
	setup%numberOfReleasePoints=numberOfReleasePoints


	
	if(.not.allocated(existingFiles)) allocate(existingFiles(setup%numOfExistingFiles))	
	if(.not.allocated(existingAtmFiles)) allocate(existingAtmFiles(setup%numOfExistingFiles))	
	if(.not.allocated(hourReadStart)) allocate(hourReadStart(setup%numOfExistingFiles))	
	if(.not.allocated(hourReadEnd)) allocate(hourReadEnd(setup%numOfExistingFiles))	
	if(.not.allocated(releasePointsLats)) allocate(releasePointsLats(setup%numberOfReleasePoints))		
	if(.not.allocated(releasePointsLons)) allocate(releasePointsLons(setup%numberOfReleasePoints))		
	if(.not.allocated(setup%existingFiles)) allocate(setup%existingFiles(setup%numOfExistingFiles))
	if(.not.allocated(setup%existingAtmFiles)) allocate(setup%existingAtmFiles(setup%numOfExistingFiles))
	if(.not.allocated(setup%hourReadStart)) allocate(setup%hourReadStart(setup%numOfExistingFiles))
	if(.not.allocated(setup%hourReadEnd)) allocate(setup%hourReadEnd(setup%numOfExistingFiles))	
	if(.not.allocated(setup%releasePointsLats)) allocate(setup%releasePointsLats(setup%numberOfReleasePoints))		
	if(.not.allocated(setup%releasePointsLons)) allocate(setup%releasePointsLons(setup%numberOfReleasePoints))			

	read(1,NML=files)
	do i = 1,setup%numOfExistingFiles
		setup%existingFiles(i)=existingFiles(i)
	enddo

	read(1,NML=atmfiles)
	do i = 1,setup%numOfExistingFiles
		print *,i,existingAtmFiles(i),shape(existingAtmFiles)
		setup%existingAtmFiles(i)=existingAtmFiles(i)
	enddo	

	read(1,NML=startTimes)
	do i = 1,setup%numOfExistingFiles
		setup%hourReadStart(i)=hourReadStart(i)
	enddo
	
	read(1,NML=endTimes)
	do i = 1,setup%numOfExistingFiles
		setup%hourReadEnd(i)=hourReadEnd(i)
	enddo

	read(1,NML=releaseLats)
	print *,releasePointsLats
	do i = 1,setup%numberOfReleasePoints
		print *,i
		setup%releasePointsLats(i)=releasePointsLats(i)
	enddo

	read(1,NML=releaseLons)
	do i = 1,setup%numberOfReleasePoints
		setup%releasePointsLons(i)=releasePointsLons(i)
	enddo		

	if (setup%releaseType.eq.'polygon')then
		totalNumberOfParticles = setup%numberOfParticles
	else
		totalNumberOfParticles = setup%numberOfParticles * setup%numberOfReleasePoints
	endif

	call allocateArrays

end subroutine initialize
