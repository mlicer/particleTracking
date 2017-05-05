! --------------------------------- read ocean T S RHO etc ------------

subroutine readOcean2D
	use ptmModule
	use netcdf
	use utilities
	implicit none

	character*255 :: filename
	real, allocatable :: tmp(:,:,:,:)

	!      integer  :: nlon, nlat, nLevels
	integer  :: file_id,t_id,s_id, rho_id, hdiff_id, vdiff_id     ! variable dimensions
	integer  :: i,j,t, hour
	real :: missValnum
	integer  :: status, hour_t, hour_s,hour_rho, hour_hdiff, hour_vdiff
	character*10 ::missVal



	allocate(tmp(ncgrid%nLon,ncgrid%nLat,ncgrid%nLevels,setup%modelRuntimeInHours))

	! 3D version:
	!    allocate(sea%T(ncgrid%nLon,ncgrid%nLat,ncgrid%nLevels,setup%numHoursOfTracking))
	!    allocate(sea%S(ncgrid%nLon,ncgrid%nLat,ncgrid%nLevels,setup%numHoursOfTracking))
	!    allocate(sea%RHO(ncgrid%nLon,ncgrid%nLat,ncgrid%nLevels,setup%numHoursOfTracking))
	!    allocate(sea%HorizontalDiffusivity(ncgrid%nLon,ncgrid%nLat,ncgrid%nLevels,setup%numHoursOfTracking))    
	!    allocate(sea%VerticalDiffusivity(ncgrid%nLon,ncgrid%nLat,ncgrid%nLevels,setup%numHoursOfTracking))

	allocate(sea%T(ncgrid%nLon,ncgrid%nLat,1,setup%numHoursOfTracking))
	allocate(sea%S(ncgrid%nLon,ncgrid%nLat,1,setup%numHoursOfTracking))
	allocate(sea%RHO(ncgrid%nLon,ncgrid%nLat,1,setup%numHoursOfTracking)) 
	
	if ( setup%useOceanModelDiffusivity ) then  
		allocate(sea%HorizontalDiffusivity(ncgrid%nLon,ncgrid%nLat,1,setup%numHoursOfTracking))    
		allocate(sea%VerticalDiffusivity(ncgrid%nLon,ncgrid%nLat,1,setup%numHoursOfTracking))            
	endif
	
	! set correct indexes for reading variables
	hour_t = 0
	hour_hdiff = 0
	hour_vdiff = 0          
	!    hour_s = 0
	!    hour_rho = 0

	do i = 1,setup%numOfExistingFiles

	 filename=setup%existingFiles(i)

	 print *,"READING OCEAN STATE FROM: ",trim(adjustl(filename))

	 call handle_err(NF90_OPEN(trim(adjustl(filename)), NF90_NOWRITE, file_id))
	 call handle_err(NF90_INQ_VARID(file_id, 't', t_id))
	! reading temperature and diffusivities:
	 call handle_err(NF90_GET_VAR(file_id, t_id, tmp))
	 call handle_err(NF90_GET_ATT(file_id, t_id, "missing_value", missValnum))	 


	 !call rstr2num(missVal,missValnum)

	 if ( setup%useOceanModelDiffusivity ) then
		 ! reading horizontal diffusivity ID
		 ! Note that in our POM horizontal kinematic viscosity = horizontal diffusivity since TPRNI = 1. 
		 call handle_err(NF90_INQ_VARID(file_id, 'horizontal_kinematic_viscosity', hdiff_id))
		 ! reading vertical diffusivity ID
		 call handle_err(NF90_INQ_VARID(file_id, 'vertical_diffusivity', vdiff_id))         
		!    call handle_err(NF90_INQ_VARID(file_id, 'salinity', s_id))
	endif
	

	if (.not. setup%performBacktracking) then
		do t = setup%hourReadStart(i),setup%hourReadEnd(i)
		    hour_t=hour_t+1
		    sea%T(:,:,1,hour_t)=tmp(:,:,setup%level,t)  
		enddo 
	else
		do t = setup%hourReadEnd(i),setup%hourReadStart(i),-1
		    hour_t=hour_t+1
		    print *, "HOUR OF DAY, ARRAY INDEX:", t, hour_t
		    sea%T(:,:,1,hour_t)=tmp(:,:,setup%level,t)            
		enddo
	endif
	
	if ( setup%useOceanModelDiffusivity ) then
		! reading horizontal diffusivity
		call handle_err(NF90_GET_VAR(file_id, hdiff_id, tmp))

		if (.not. setup%performBacktracking) then
			do t = setup%hourReadStart(i),setup%hourReadEnd(i)
				hour_hdiff=hour_hdiff+1
				sea%HorizontalDiffusivity(:,:,1,hour_hdiff)=tmp(:,:,setup%level,t)            
			enddo 
		else
			do t = setup%hourReadEnd(i),setup%hourReadStart(i),-1
				hour_hdiff=hour_hdiff+1
				sea%HorizontalDiffusivity(:,:,1,hour_hdiff)=tmp(:,:,setup%level,t)            
			enddo
		endif

		! reading vertical diffusivity
		call handle_err(NF90_GET_VAR(file_id, vdiff_id, tmp))
		if (.not. setup%performBacktracking) then
			do t = setup%hourReadStart(i),setup%hourReadEnd(i)
				hour_vdiff=hour_vdiff+1
				sea%VerticalDiffusivity(:,:,1,hour_hdiff)=tmp(:,:,setup%level,t)            
			enddo 
		else
			do t = setup%hourReadEnd(i),setup%hourReadStart(i),-1
				hour_vdiff=hour_vdiff+1
				sea%VerticalDiffusivity(:,:,1,hour_vdiff)=tmp(:,:,setup%level,t)            
			enddo
		endif        
	endif
	 ! close file 
	 call handle_err(NF90_CLOSE(file_id))

	enddo



end subroutine readOcean2D
