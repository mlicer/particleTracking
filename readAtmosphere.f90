! --------------------------------- read ocean T S RHO etc ------------

subroutine readAtmosphere
	use ptmModule
	use netcdf
	use utilities
	implicit none

	character*255 :: filename
	real, allocatable :: tmpu(:,:,:),tmpv(:,:,:)

	!      integer  :: nlon, nlat, nLevels
	integer  :: file_id,u_id,v_id ! variable dimensions
	integer  :: i,j,t, hour
	real :: missValnum
	integer  :: status, hour_t
	character*10 ::missVal


	allocate(air%U(ncgrid%nLon,ncgrid%nLat,setup%numHoursOfTracking))
	allocate(air%V(ncgrid%nLon,ncgrid%nLat,setup%numHoursOfTracking))
	
	allocate(tmpu(ncgrid%nLon,ncgrid%nLat,setup%modelRuntimeInHours))
	allocate(tmpv(ncgrid%nLon,ncgrid%nLat,setup%modelRuntimeInHours))

	! set correct indexes for reading variables
	hour_t = 0

	do i = 1,setup%numOfExistingFiles

	 filename=setup%existingAtmFiles(i)

	 print *,"READING ATMOSPHERE STATE FROM: ",trim(adjustl(filename))

	 call handle_err(NF90_OPEN(trim(adjustl(filename)), NF90_NOWRITE, file_id))
	 call handle_err(NF90_INQ_VARID(file_id, 'uair', u_id))
	 call handle_err(NF90_INQ_VARID(file_id, 'vair', v_id))
	 

	
	! reading winds:
	 call handle_err(NF90_GET_VAR(file_id, u_id, tmpu))
	 call handle_err(NF90_GET_VAR(file_id, v_id, tmpv))
	 call handle_err(NF90_GET_ATT(file_id, u_id, "missing_value", missVal))

	 
	 call rstr2num(missVal,missValnum)

	if (.not. setup%performBacktracking) then
		do t = setup%hourReadStart(i),setup%hourReadEnd(i)
		    hour_t=hour_t+1
		    air%U(:,:,hour_t)=tmpu(:,:,t)  
		    air%V(:,:,hour_t)=tmpv(:,:,t)  
		enddo 
	else
		do t = setup%hourReadEnd(i),setup%hourReadStart(i),-1
		    hour_t=hour_t+1
		    air%U(:,:,hour_t)=tmpu(:,:,t)  
		    air%V(:,:,hour_t)=tmpv(:,:,t) 
		enddo
	endif
	

	 ! close file 
	 call handle_err(NF90_CLOSE(file_id))

	enddo



end subroutine readAtmosphere
