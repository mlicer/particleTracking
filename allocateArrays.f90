subroutine allocateArrays
	use ptmModule
	implicit none
	allocate(particles%dates( setup%numHoursOfTracking ))
	allocate(particles%levels( setup%numHoursOfTracking, totalNumberOfParticles))
	allocate(particles%lats( setup%numHoursOfTracking, totalNumberOfParticles ))
	allocate(particles%lons( setup%numHoursOfTracking, totalNumberOfParticles ))
	allocate(particles%mass( setup%numHoursOfTracking, totalNumberOfParticles ))
	allocate(particles%totalEvaporationFlag( totalNumberOfParticles ))    
	allocate(particles%evapPercentage( setup%numHoursOfTracking, totalNumberOfParticles ))    
	allocate(particles%hourOfBirth( totalNumberOfParticles ))        
	particles%numOfExistingParticles = 0
end subroutine allocateArrays
