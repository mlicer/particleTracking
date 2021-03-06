subroutine readUV3D
! --------------------------------- read NC 3D current velocity --------
	use ptmModule
	use netcdf
	use utilities
	implicit none
	include 'netcdf.inc'

	character*255 :: filename

	real, allocatable :: tmp(:,:,:,:)

	!      integer  :: nlon, nlat, nLevels
	integer  :: file_id,u_id,v_id, w_id     ! variable dimensions
	integer  :: i,j,t, hour
	real :: missValnum
	integer  :: status, hour_u, hour_v, hour_w
	character*10 ::missVal

	allocate(tmp(ncgrid%nLon,ncgrid%nLat,ncgrid%nLevels,setup%modelRuntimeInHours))

	! 3D version:
	allocate(current%u(ncgrid%nLon,ncgrid%nLat,ncgrid%nLevels,setup%numHoursOfTracking))
	allocate(current%v(ncgrid%nLon,ncgrid%nLat,ncgrid%nLevels,setup%numHoursOfTracking))
	allocate(current%w(ncgrid%nLon,ncgrid%nLat,ncgrid%nLevels,setup%numHoursOfTracking))	


	! set correct indexes for reading variables
	hour_u = 0
	hour_v = 0
	hour_w = 0
	
	do i = 1,setup%numOfExistingFiles

	filename=setup%existingFiles(i)

	print *,"READING OCEAN CURRENTS FROM: ",trim(adjustl(filename))

	call handle_err(NF90_OPEN(trim(adjustl(filename)), NF90_NOWRITE, file_id))
	call handle_err(NF90_INQ_VARID(file_id, 'u', u_id))
	call handle_err(NF90_INQ_VARID(file_id, 'v', v_id))
	call handle_err(NF90_INQ_VARID(file_id, 'w_vertical_velocity', w_id))     
	! fill u values
	call handle_err(NF90_GET_VAR(file_id, u_id, tmp))
	call handle_err(NF90_GET_ATT(file_id, u_id, "missing_value", missValnum))



	! change attribute type (don't ask...)     
	! call rstr2num(missVal,missValnum)

	if (i==1) then
	! create lsm mask of zeros:
	ncgrid%lsm(:,:,:)=0.0
	where (abs(tmp(:,:,:,i)) .lt. missValnum) 
		ncgrid%lsm(:,:,:)=1.0
	end where

	endif

	if (.not. setup%performBacktracking) then
	do t = setup%hourReadStart(i),setup%hourReadEnd(i)
		hour_u=hour_u+1
		print *, "HOUR OF DAY, TOTAL HOUR OF SIMULATION:", t, hour_u
		current%u(:,:,:,hour_u)=tmp(:,:,:,t)
	enddo

	! read v values:
	call handle_err(NF90_GET_VAR(file_id, v_id, tmp))

	do t = setup%hourReadStart(i),setup%hourReadEnd(i)
		hour_v=hour_v+1
		current%v(:,:,:,hour_v)=tmp(:,:,:,t)
	enddo  


	! read w values:   
	call handle_err(NF90_GET_VAR(file_id, w_id, tmp))
	do t = setup%hourReadStart(i),setup%hourReadEnd(i)
		hour_w=hour_w+1
		current%w(:,:,:,hour_w)=tmp(:,:,:,t)            
	enddo            
	else
	do t = setup%hourReadEnd(i),setup%hourReadStart(i),-1
		hour_u=hour_u+1
		print *, "BACKTRACKING. HOUR OF DAY, TOTAL HOUR OF SIMULATION:", t, hour_u
		current%u(:,:,:,hour_u)=tmp(:,:,:,t)
	enddo
	! read v values
	call handle_err(NF90_GET_VAR(file_id, v_id, tmp))

	do t = setup%hourReadEnd(i),setup%hourReadStart(i),-1
		hour_v=hour_v+1
	    current%v(:,:,:,hour_v)=tmp(:,:,:,t)
	enddo

	! read w values
	call handle_err(NF90_GET_VAR(file_id, w_id, tmp))

	do t = setup%hourReadEnd(i),setup%hourReadStart(i),-1
		hour_w=hour_w+1
	    current%w(:,:,:,hour_w)=tmp(:,:,:,t)
	enddo        
	endif

	! close file 
	call handle_err(NF90_CLOSE(file_id))

	enddo

end subroutine readUV3D
