module ptmModule
! This is a key module for particleTracking.f90 code. It contains
! *)	all derived-type declarations used in the codes
! *)	particle allocation and creation routines
! More info: matjaz.licer@nib.si

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
		integer                      :: numOfExistingParticles
		integer, allocatable         :: hourOfBirth(:)
		integer, allocatable         :: levels(:,:)
		real, allocatable            :: mass(:,:)
		logical, allocatable         :: totalEvaporationFlag(:)     
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
		real, allocatable             :: U(:,:,:)
		real, allocatable             :: V(:,:,:)
	end type atmosphere

	type init
		integer :: modelRuntimeInHours, dt
		integer :: numHoursOfTracking
		integer :: releaseDuration
		integer :: numberOfParticles,numberOfReleasePoints
		integer :: totalNumberOfParticles
		integer :: numCreatedParticlesPerStep
		real :: totalMass
		integer :: oilIndex
		real    :: completeEvapPercentage
		real    :: evapThreshold		
		integer :: level
		integer :: sign_t
		real    :: Dh		
		character*10 :: startdate,enddate
		integer :: numOfExistingFiles
		integer,allocatable :: hourReadStart(:)
		integer,allocatable :: hourReadEnd(:)
		real,allocatable :: releasePointsLats(:)
		real,allocatable :: releasePointsLons(:)		
		character*255,allocatable :: existingFiles(:)
		character*255,allocatable :: existingAtmFiles(:)
		character*255 :: model,releaseType
		logical :: performBacktracking
		logical :: tracking3D
		logical :: useOceanModelDiffusivity	
	end type init

	integer :: modelRuntimeInHours, dt
	integer :: numHoursOfTracking
	integer :: releaseDuration
	integer :: numberOfParticles,numberOfReleasePoints
	integer :: numCreatedParticlesPerStep
	real :: totalMass
	integer :: oilIndex
	integer :: level
	integer :: sign_t
	real    :: Dh
	real    :: completeEvapPercentage
	real    :: evapThreshold
	character*10 :: startdate,enddate
	integer :: numOfExistingFiles
	integer,allocatable :: hourReadStart(:)
	integer,allocatable :: hourReadEnd(:)
	real,allocatable :: releasePointsLats(:)
	real,allocatable :: releasePointsLons(:)		
	character*255,allocatable :: existingFiles(:)
	character*255,allocatable :: existingAtmFiles(:)
	character*255 :: model,releaseType
	logical :: performBacktracking
	logical :: tracking3D	
	logical :: useOceanModelDiffusivity	
	
! declare IO namelists: -------------------------------------------
    namelist /initialParameters/ model,modelRuntimeInHours,&
	dt,numHoursOfTracking,releaseDuration,performBacktracking,&
	numberOfParticles,numCreatedParticlesPerStep,totalMass,&
	oilIndex,completeEvapPercentage,evapThreshold,&
	useOceanModelDiffusivity,Dh,numOfExistingFiles,level,tracking3D,&
	sign_t,startdate,enddate,releaseType,numberOfReleasePoints
	
	namelist/files/existingFiles

	namelist/atmfiles/existingAtmFiles
	
	namelist/startTimes/hourReadStart
	
	namelist/endTimes/hourReadEnd
	
	namelist/releaseLats/releasePointsLats
	
	namelist/releaseLons/releasePointsLons	
	
! declare variables: -------------------------------------------
	type(grid) :: ncgrid
	type(particle) :: particles
	type(velocity) :: current
	type(ocean) :: sea	
	type(spill) :: slick
	type(init) :: setup
	type(atmosphere) :: air
	
	real, parameter :: Rearth = 6370000., pi = 3.141592653589793
	real, parameter :: windCorrectionFactor=0.03


	! open MP declarations:
	integer ( kind = 4 ) proc_num
	integer ( kind = 4 ) thread_num
    
	integer :: totalNumberOfParticles
    
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

 ! ------------------------seed for random numbers ---------------------
subroutine init_random_seed(vecsize)

      INTEGER :: i, n,clock, vecsize 
      INTEGER, DIMENSION(:), ALLOCATABLE :: seed

			n=vecsize
      CALL RANDOM_SEED(size = n)
      ALLOCATE(seed(n))

      CALL SYSTEM_CLOCK(COUNT=clock)

      seed = clock + 37 * (/ (i - 1, i = 1, n) /)
      CALL RANDOM_SEED(PUT = seed)

      DEALLOCATE(seed)
end subroutine init_random_seed

subroutine generate_random_vector(vectorSize,randVector)
	integer :: vectorSize
	real, allocatable :: randVector(:)
	call init_random_seed(vectorSize)
	if(.not.allocated(randVector))allocate(randVector(vectorSize))
	call random_number(randVector)
end subroutine

end module ptmModule
