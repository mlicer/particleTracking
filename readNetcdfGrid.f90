! --------------------------------- read NC grid ----------------------

subroutine readNetcdfGrid(filename)
	use ptmModule
	use netcdf
	implicit none

	character*(*), intent(in) :: filename


	integer                              :: nlon, nlat, nLevels
	integer                              :: file_id, lon_id, lat_id, level_id
	integer                              :: lon_dim, lat_dim, level_dim            ! variable dimensions
	integer,dimension(NF90_MAX_VAR_DIMS) :: lon_dim_ids, lat_dim_ids, level_dim_ids    ! variable dimension ids
	integer,dimension(NF90_MAX_VAR_DIMS) :: lon_dim_len, lat_dim_len, level_dim_len    ! variable dimension lengths
	integer,dimension(3)                 :: index_count, index_start
	integer                              :: i,j
	integer                              :: status

	print *, "GRID FILENAME:",trim(adjustl(filename))
	call handle_err(NF90_OPEN(trim(adjustl(filename)), NF90_NOWRITE, file_id))
	call handle_err(NF90_INQ_VARID(file_id, 'lon', lon_id))
	call handle_err(NF90_INQ_VARID(file_id, 'lat', lat_id))
	call handle_err(NF90_INQ_VARID(file_id, 'z', level_id))
	call handle_err(NF90_INQUIRE_VARIABLE(ncid=file_id, varid=lon_id, ndims=lon_dim, dimids=lon_dim_ids))
	call handle_err(NF90_INQUIRE_VARIABLE(ncid=file_id, varid=lat_id, ndims=lat_dim, dimids=lat_dim_ids))
	call handle_err(NF90_INQUIRE_VARIABLE(ncid=file_id, varid=level_id, ndims=level_dim, dimids=level_dim_ids))

	! get variable dimension lengths
	do i=1,lon_dim
	call handle_err(NF90_INQUIRE_DIMENSION(ncid=file_id, dimid=lon_dim_ids(i), len=lon_dim_len(i)))
	enddo
	do i=1,lat_dim
	call handle_err(NF90_INQUIRE_DIMENSION(ncid=file_id, dimid=lat_dim_ids(i), len=lat_dim_len(i)))
	enddo
	do i=1,level_dim
	call handle_err(NF90_INQUIRE_DIMENSION(ncid=file_id, dimid=level_dim_ids(i), len=level_dim_len(i)))
	enddo     

	! set nlon and nlat
	ncgrid%nLon = lon_dim_len(1)
	ncgrid%nLat = lat_dim_len(1)
	ncgrid%nLevels = level_dim_len(1)


	! allocate 1d grid
	allocate(ncgrid%lons(ncgrid%nLon))
	allocate(ncgrid%lats(ncgrid%nLat))
	allocate(ncgrid%levels(ncgrid%nLevels))

	!      ! allocate 2d grid
	allocate(ncgrid%lon2(ncgrid%nLon,ncgrid%nLat))
	allocate(ncgrid%lat2(ncgrid%nLon,ncgrid%nLat)) 

	!      ! allocate 3d mask:    
	allocate(ncgrid%lsm(ncgrid%nLon,ncgrid%nLat,ncgrid%nLevels))     

	! fill grid
	call handle_err(NF90_GET_VAR(file_id, lon_id, ncgrid%lons))
	call handle_err(NF90_GET_VAR(file_id, lat_id, ncgrid%lats))
	call handle_err(NF90_GET_VAR(file_id, level_id, ncgrid%levels))

	ncgrid%resolution = ncgrid%lons(2)-ncgrid%lons(1)

	do i =1,ncgrid%nLon
	do j=1,ncgrid%nLat
		ncgrid%lon2(i,j) = ncgrid%lons(i)
		ncgrid%lat2(i,j) = ncgrid%lats(j)
	enddo
	enddo

	print *,"GRID LATITUDES:",ncgrid%lats
	print *,"GRID LONGITUDES:",ncgrid%lons
	
	! close file 
	call handle_err(NF90_CLOSE(file_id))

end subroutine readNetcdfGrid
