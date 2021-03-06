subroutine fate(t,p)
! This routine computes fate and weathering of the particles,
! advected and diffused by the particleTracking.f90 code.
! Oil evaporation is computed according to "Fingas, Oil Spill Science
! and Technology, p.241, Table 9.2, Elsevier 2011".
! More info: matjaz.licer@mbss.org 

use ptmModule
!use initModule
use omp_lib
implicit none

	real :: dx, dy, fval
	real :: d2r, r2d, u0,v0, udiff, vdiff
	real :: A,B
	integer ::p,t,tt, i_point,j_point, functionType, idx, nt
    real :: evapPercentage, lnT, sqrtT, ageInMinutes, massThreshold
    character*132 :: oilName
        

	d2r = pi/180.;
	r2d = 180./pi ;
	massThreshold = (1 - setup%evapThreshold) * (setup%totalMass / setup%numberOfParticles)

    call oilEvapTable(setup%oilIndex,A,B,functionType)

    ! get grid indices of particle current location:

	call get_closest_point(particles%lats(t,p),particles%lons(t,p),j_point,i_point)

    if (ncgrid%lsm(i_point,j_point,setup%level).eq. 1) then ! compute only for wet points:
    
        ! current age of current particle in minutes:
        ageInMinutes = (t-particles%hourOfBirth(p))*setup%dt*60.  
        
      	! compute evaporation percentage following fingas:
        evapPercentage = A + B*sea%T(i_point,j_point,setup%level,t)   

        ! apply only to particles that have not been created this timestep:		

            if ( ageInMinutes .gt. 0.0) then
				SELECT CASE (functionType)
				   CASE(1)
				      evapPercentage = evapPercentage*lnT(ageInMinutes)*0.01
				   CASE(2)
				      evapPercentage = evapPercentage*sqrtT(ageInMinutes)*0.01
				   CASE DEFAULT
				      evapPercentage = evapPercentage*lnT(ageInMinutes)*0.01
				END SELECT

				! difference in evaporation from previous timestep:
				
				!$omp critical
                if (particles%totalEvaporationFlag(p)) then
                    particles%evapPercentage(t-1,p) = evapPercentage - particles%evapPercentage(t-1,p)
                    particles%mass(t,p) = max(0.0,particles%mass(t-1,p) - particles%mass(t-1,p)*particles%evapPercentage(t-1,p))
                else
                    if (particles%mass(t-1,p) > massThreshold) then
                        particles%evapPercentage(t-1,p) = evapPercentage - particles%evapPercentage(t-1,p)
                        particles%mass(t,p) = max(0.0,particles%mass(t-1,p) - particles%mass(t-1,p)*particles%evapPercentage(t-1,p))
                    else
                        particles%mass(t,p)=particles%mass(t-1,p)
                    endif
                endif
				!$omp end critical

            endif       

    endif

    return
end subroutine fate

    real function sqrtT(x)
    implicit none
    real,intent(in) :: x

    sqrtT = x**(0.5)
    return
    end function sqrtT

    real function lnT(x)
    implicit none
    real,intent(in) :: x

    lnT = log(x)
    return
    end function lnT
    
subroutine oilEvapTable(idx,Aout,Bout,functionType)
    use ptmModule
    implicit none
    
    integer,parameter :: numOfOilTypes = 3
    real :: A(numOfOilTypes),B(numOfOilTypes)
    integer :: idx
    real, intent(out) :: Aout,Bout
    integer, intent(out) :: functionType

    
    ! Fingas, Table 9.2, p. 219
    
    ! functionType = 1: ln(T)
    ! functionType = 2: sqrt(T)
    
    select case (idx)
    
    case(1)
        slick%oilType(:) = 'ArabianLight        '
        Aout = 2.52
        Bout = 0.037
        functionType = 1
    case(2)
        slick%oilType(:) = 'ArabianMedium       '
        Aout = 1.89
        Bout = 0.045
        functionType = 1 
    case(3)      
        slick%oilType(:) = 'ArabianHeavy        '  
        Aout = 1.31
        Bout = 0.045   
        functionType = 1    
    case default
        print *, "ERROR! ILLEGAL INDEX IN FATE.F90: ",idx
        stop
    end select
     
    return
end subroutine oilEvapTable



