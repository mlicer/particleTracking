module trackingTools
! This is a key module for particleTracking.f90 code. It contains
! 1)	all derived-type declarations used in the codes
! 2)	all NetCDF IO functions needed for particle tracking
! 3)	particle allocation and creation routines
! More info: matjaz.licer@mbss.org

    implicit none
    
! define derived types:-----------------------------
	type grid
		integer          :: nLon
		integer          :: nLat
		integer          :: nLevels
		real             :: resolution     
		real, allocatable            :: lons(:)
		real, allocatable            :: lats(:)
		real, allocatable             :: levels(:)
		real, allocatable             :: lon2(:,:)
		real, allocatable             :: lat2(:,:)     
		real, allocatable             :: lsm(:,:,:)
	end type grid

	type particle
		integer, allocatable         :: dates(:)
		integer         :: numOfExistingParticles
		integer, allocatable         :: hourOfBirth(:)
		real, allocatable            :: levels(:,:)
		real, allocatable            :: mass(:,:)
		real, allocatable            :: evapPercentage(:,:)     
		real, allocatable            :: lats(:,:)
		real, allocatable            :: lons(:,:)
	end type particle

	type velocity
		real, allocatable :: u(:,:,:,:)
		real, allocatable :: v(:,:,:,:)
		real, allocatable :: w(:,:,:,:)    
	end type velocity

	type spill
		real,allocatable :: pollutantConcentration(:,:,:,:)
		character*20     :: oilType
	end type spill

	type ocean  
		real, allocatable             :: T(:,:,:,:)
		real, allocatable             :: S(:,:,:,:)
		real, allocatable             :: RHO(:,:,:,:)
		real, allocatable             :: HorizontalDiffusivity(:,:,:,:)     
		real, allocatable             :: VerticalDiffusivity(:,:,:,:)          
		type(velocity)   :: current
	end type ocean

	type atmosphere  
		real, allocatable             :: T2m(:,:,:)
		real, allocatable             :: UWND(:,:,:)
		real, allocatable             :: VWND(:,:,:)
	end type atmosphere

	type init
		integer :: modelRuntimeInHours, dt
		integer :: numHoursOfTracking
		integer :: releaseDuration
		integer :: numberOfParticles
		integer :: numCreatedParticlesPerStep
		real :: totalMass
		integer :: oilIndex
		integer :: level
		integer :: sign_t
		real    :: lat0,lon0, Dh
		character*10 :: startdate,enddate
		integer :: numOfExistingFiles
		integer,allocatable :: hourReadStart(:)
		integer,allocatable :: hourReadEnd(:)
		character*255,allocatable :: existingFiles(:)
		character*255 :: model
		logical :: performBacktracking
	end type init

	integer :: modelRuntimeInHours, dt
	integer :: numHoursOfTracking
	integer :: releaseDuration
	integer :: numberOfParticles
	integer :: numCreatedParticlesPerStep
	real :: totalMass
	integer :: oilIndex
	integer :: level
	integer :: sign_t
	real    :: lat0,lon0, Dh
	character*10 :: startdate,enddate
	integer :: numOfExistingFiles
	integer,allocatable :: hourReadStart(:)
	integer,allocatable :: hourReadEnd(:)
	character*255,allocatable :: existingFiles(:)
	character*255 :: model
	logical :: performBacktracking

! declare IO namelists: -------------------------------------------
    namelist /initialParameters/ model,modelRuntimeInHours,&
	dt,numHoursOfTracking,releaseDuration,performBacktracking,&
	numberOfParticles,numCreatedParticlesPerStep,totalMass,&
	oilIndex,Dh,numOfExistingFiles,lat0,lon0,&
	level,sign_t,startdate,enddate
	
	namelist/files/existingFiles
	
	namelist/startTimes/hourReadStart
	
	namelist/endTimes/hourReadEnd
	
! declare variables: -------------------------------------------
	type(grid) :: ncgrid
	type(particle) :: particles
	type(velocity) :: current
	type(ocean) :: sea	
    type(spill) :: slick
	type(init) :: setup
! subroutines ----------------------------------------------------------
! subroutines ----------------------------------------------------------
! subroutines ----------------------------------------------------------

contains

 ! ------------------------handle NC errors ----------------------------

subroutine handle_err(istatus)
use netcdf
implicit none
integer, intent(in) :: istatus

IF (istatus .NE. NF90_NOERR) THEN
   print *, 'NetCDF fail:'
   print *,'ERROR CODE: ', trim(nf90_strerror(istatus))
   stop
ENDIF

RETURN
end subroutine handle_err


subroutine allocateArrays
	implicit none
    if(.not.allocated(particles%dates)) allocate(particles%dates(setup%numHoursOfTracking))
    if(.not.allocated(particles%levels))allocate(particles%levels(setup%numHoursOfTracking,setup%numberOfParticles))
    if(.not.allocated(particles%lats))allocate(particles%lats(setup%numHoursOfTracking,setup%numberOfParticles))
    if(.not.allocated(particles%lons))allocate(particles%lons(setup%numHoursOfTracking,setup%numberOfParticles))
    if(.not.allocated(particles%mass))allocate(particles%mass(setup%numHoursOfTracking,setup%numberOfParticles))
    if(.not.allocated(particles%evapPercentage))allocate(particles%evapPercentage(setup%numHoursOfTracking,setup%numberOfParticles))    
    if(.not.allocated(particles%hourOfBirth))allocate(particles%hourOfBirth(setup%numberOfParticles))        
    particles%numOfExistingParticles = 0
end subroutine allocateArrays


 ! --------------------------------- read NC grid ----------------------

  subroutine readNetcdfGrid(filename)
     use netcdf
     implicit none
     include 'netcdf.inc'

     character*(*), intent(in) :: filename


     integer                              :: nlon, nlat, nLevels
     integer                              :: file_id, lon_id, lat_id, level_id
     integer                              :: lon_dim, lat_dim, level_dim            ! variable dimensions
     integer,dimension(NF90_MAX_VAR_DIMS) :: lon_dim_ids, lat_dim_ids, level_dim_ids    ! variable dimension ids
     integer,dimension(NF90_MAX_VAR_DIMS) :: lon_dim_len, lat_dim_len, level_dim_len    ! variable dimension lengths
     integer,dimension(3)                 :: index_count, index_start
     integer                              :: i,j
     integer                              :: status

     call handle_err(NF90_OPEN(trim(adjustl(filename)), NF90_NOWRITE, file_id))
     call handle_err(NF90_INQ_VARID(file_id, 'Lon', lon_id))
     call handle_err(NF90_INQ_VARID(file_id, 'Lat', lat_id))
     call handle_err(NF90_INQ_VARID(file_id, 'level', level_id))
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

     ! close file 
     call handle_err(NF90_CLOSE(file_id))

  end subroutine readNetcdfGrid

 ! --------------------------------- read NC current velocity ----------
 
  subroutine readUV
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
    !allocate(current%u(ncgrid%nLon,ncgrid%nLat,ncgrid%nLevels,setup%numHoursOfTracking))
    !allocate(current%v(ncgrid%nLon,ncgrid%nLat,ncgrid%nLevels,setup%numHoursOfTracking))
    
    allocate(current%u(ncgrid%nLon,ncgrid%nLat,1,setup%numHoursOfTracking))
    allocate(current%v(ncgrid%nLon,ncgrid%nLat,1,setup%numHoursOfTracking))
    
    ! set correct indexes for reading variables
     hour_u = 0
     hour_v = 0
     hour_w = 0
     
     do i = 1,setup%numOfExistingFiles

     filename=setup%existingFiles(i)

     print *,"READING OCEAN CURRENTS FROM: ",trim(adjustl(filename))

     call handle_err(NF90_OPEN(trim(adjustl(filename)), NF90_NOWRITE, file_id))
     call handle_err(NF90_INQ_VARID(file_id, 'u_velocity', u_id))
     call handle_err(NF90_INQ_VARID(file_id, 'v_velocity', v_id))
     call handle_err(NF90_INQ_VARID(file_id, 'w_vertical_velocity', w_id))     
     ! fill u values
     call handle_err(NF90_GET_VAR(file_id, u_id, tmp))
     call handle_err(NF90_GET_ATT(file_id, u_id, "missing_value", missVal))


     
     ! change attribute type (don't ask...)     
     call rstr2num(missVal,missValnum)

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
            !current%u(:,:,:,hour_u)=tmp(:,:,:,t)
            current%u(:,:,1,hour_u)=tmp(:,:,setup%level,t)            
        enddo

     ! read v values:
      call handle_err(NF90_GET_VAR(file_id, v_id, tmp))
     
        do t = setup%hourReadStart(i),setup%hourReadEnd(i)
            hour_v=hour_v+1
            !current%v(:,:,,hour_v)=tmp(:,:,:,t)
            current%v(:,:,1,hour_v)=tmp(:,:,setup%level,t)            
        enddo  
     

     ! read w values:   
      call handle_err(NF90_GET_VAR(file_id, w_id, tmp))
        do t = setup%hourReadStart(i),setup%hourReadEnd(i)
            hour_w=hour_w+1
            !current%w(:,:,,hour_w)=tmp(:,:,:,t)            
            current%w(:,:,1,hour_w)=tmp(:,:,setup%level,t)            
        enddo            
    else
		do t = setup%hourReadEnd(i),setup%hourReadStart(i),-1
			hour_u=hour_u+1
			print *, "BACKTRACKING. HOUR OF DAY, TOTAL HOUR OF SIMULATION:", t, hour_u
			!current%u(:,:,:,hour_u)=tmp(:,:,:,t)
			current%u(:,:,1,hour_u)=tmp(:,:,setup%level,t)            
		enddo
		! read v values
		call handle_err(NF90_GET_VAR(file_id, v_id, tmp))

		do t = setup%hourReadEnd(i),setup%hourReadStart(i),-1
			hour_v=hour_v+1
		!            current%v(:,:,:,hour_v)=tmp(:,:,:,t)
			current%v(:,:,1,hour_v)=tmp(:,:,setup%level,t)            
		enddo

		! read w values
		call handle_err(NF90_GET_VAR(file_id, w_id, tmp))

		do t = setup%hourReadEnd(i),setup%hourReadStart(i),-1
			hour_w=hour_w+1
		!            current%w(:,:,:,hour_w)=tmp(:,:,:,t)
			current%w(:,:,1,hour_w)=tmp(:,:,setup%level,t)            
		enddo        
		endif

		! close file 
call handle_err(NF90_CLOSE(file_id))

    enddo
 
  end subroutine readUV

 ! --------------------------------- read ocean T S RHO etc ------------

  subroutine readOCEAN
     use netcdf
     use utilities
     implicit none
     include 'netcdf.inc'

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
    allocate(sea%HorizontalDiffusivity(ncgrid%nLon,ncgrid%nLat,1,setup%numHoursOfTracking))    
    allocate(sea%VerticalDiffusivity(ncgrid%nLon,ncgrid%nLat,1,setup%numHoursOfTracking))            
 
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
         call handle_err(NF90_INQ_VARID(file_id, 'temperature', t_id))
         ! reading horizontal diffusivity ID
         ! Note that in our POM horizontal kinematic viscosity = horizontal diffusivity since TPRNI = 1. 
         call handle_err(NF90_INQ_VARID(file_id, 'horizontal_kinematic_viscosity', hdiff_id))
         ! reading vertical diffusivity ID
         call handle_err(NF90_INQ_VARID(file_id, 'vertical_diffusivity', vdiff_id))         
     !    call handle_err(NF90_INQ_VARID(file_id, 'salinity', s_id))
      
        ! reading temperature and diffusivities:
         call handle_err(NF90_GET_VAR(file_id, t_id, tmp))
         call handle_err(NF90_GET_ATT(file_id, t_id, "missing_value", missVal))

         
         call rstr2num(missVal,missValnum)

        if (.not. setup%performBacktracking) then
            do t = setup%hourReadStart(i),setup%hourReadEnd(i)
                hour_t=hour_t+1
                !sea%T(:,:,:,hour_t)=tmp(:,:,:,t)
                sea%T(:,:,1,hour_t)=tmp(:,:,setup%level,t)  
            enddo 
        else
            do t = setup%hourReadEnd(i),setup%hourReadStart(i),-1
                hour_t=hour_t+1
                print *, "HOUR OF DAY, ARRAY INDEX:", t, hour_t
    !            sea%T(:,:,:,hour_t)=tmp(:,:,:,t)
                sea%T(:,:,1,hour_t)=tmp(:,:,setup%level,t)            
            enddo
        endif
        
        ! reading horizontal diffusivity
        call handle_err(NF90_GET_VAR(file_id, hdiff_id, tmp))

        if (.not. setup%performBacktracking) then
            do t = setup%hourReadStart(i),setup%hourReadEnd(i)
                hour_hdiff=hour_hdiff+1
                !sea%HorizontalDiffusivity(:,:,:,hour_hdiff)=tmp(:,:,:,t)
                sea%HorizontalDiffusivity(:,:,1,hour_hdiff)=tmp(:,:,setup%level,t)            
            enddo 
        else
            do t = setup%hourReadEnd(i),setup%hourReadStart(i),-1
                hour_hdiff=hour_hdiff+1
    !            sea%HorizontalDiffusivity(:,:,:,hour_hdiff)=tmp(:,:,:,t)
                sea%HorizontalDiffusivity(:,:,1,hour_hdiff)=tmp(:,:,setup%level,t)            
            enddo
        endif
        
        ! reading vertical diffusivity
        call handle_err(NF90_GET_VAR(file_id, vdiff_id, tmp))
        if (.not. setup%performBacktracking) then
            do t = setup%hourReadStart(i),setup%hourReadEnd(i)
                hour_vdiff=hour_vdiff+1
                !sea%VerticalDiffusivity(:,:,:,hour_hdiff)=tmp(:,:,:,t)
                sea%VerticalDiffusivity(:,:,1,hour_hdiff)=tmp(:,:,setup%level,t)            
            enddo 
        else
            do t = setup%hourReadEnd(i),setup%hourReadStart(i),-1
                hour_vdiff=hour_vdiff+1
    !            sea%VerticalDiffusivity(:,:,:,hour_vdiff)=tmp(:,:,:,t)
                sea%VerticalDiffusivity(:,:,1,hour_vdiff)=tmp(:,:,setup%level,t)            
            enddo
        endif        

         ! close file 
         call handle_err(NF90_CLOSE(file_id))

    enddo
 

    
  end subroutine readOCEAN

 ! -------------------------create particles ---------------------------

  subroutine createParticles(t)
    implicit none

!    type(grid) :: ncgrid
    integer :: i, imax, t
    real, allocatable :: random_vector(:)

        
    if(.not.allocated(random_vector))allocate(random_vector(setup%numberOfParticles))


    call random_number(random_vector)

    if (t.lt.setup%releaseDuration) then
        imax = setup%numCreatedParticlesPerStep
    else
        imax = setup%numberOfParticles-(t-1)* setup%numCreatedParticlesPerStep
    endif


    do i=1,imax
        particles%lats(t,(t-1)* setup%numCreatedParticlesPerStep+i)=&
        setup%lat0 + 0.25*ncgrid%resolution*random_vector(i)
        
        particles%lons(t,(t-1)* setup%numCreatedParticlesPerStep+i)=&
        setup%lon0 + 0.25*ncgrid%resolution*random_vector(i)

        particles%levels(t,(t-1)* setup%numCreatedParticlesPerStep+i)=&
        setup%level
        
        particles%hourOfBirth((t-1)* setup%numCreatedParticlesPerStep+i) = t
        
        particles%mass(t,(t-1)* setup%numCreatedParticlesPerStep+i) = &
        setup%totalMass/setup%numberOfParticles
    enddo
    
    particles%numOfExistingParticles = &
    particles%numOfExistingParticles + imax

	
  end subroutine createParticles
end module trackingTools
