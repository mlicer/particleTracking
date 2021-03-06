    subroutine advectionDiffusion2D(t,p)
    ! Advection and diffusion routines for particleTracking.f90
    ! More info: matjaz.licer@mbss.org
    
    use ptmModule
    implicit none
    	real :: dx,dy,dz
	    real :: u0,v0
	    real :: udiff, vdiff
	    integer :: p,t,i_point,j_point, point
		
		dz=0.0

    ! test for initial points (land/sea):
    
    if (t==1) then
		do point = 1, setup%numberOfReleasePoints
			call get_closest_point(setup%releasePointsLats(point),&
								   setup%releasePointsLons(point),j_point,i_point)
			if ( ncgrid%lsm(i_point,j_point,setup%level) .ne. 1.0 ) then
				print *,''
				print *, "PTM ERROR: INITIAL POINT ",setup%releasePointsLats(point),&
				setup%releasePointsLons(point),&
				"SEEMS TO BE A LAND POINT!"
				stop
			endif 
		enddo 
    endif
	
		
	    particles%dates(t)= t
	    !particles%dates(setup%numHoursOfTracking)=setup%numHoursOfTracking

	    ! update particle grid index (i_point,j_point) from previous timestep:

	    call get_closest_point(particles%lats(t,p),&
	    particles%lons(t,p), j_point, i_point)

	    ! advect particles and enforce beaching if neccessary:
	    if (ncgrid%lsm(i_point,j_point,setup%level) .eq. 1.0) then
	
	    ! get advection velocities at this location/timestep:
		    u0 = current%u(i_point,j_point,setup%level,t)
		    v0 = current%v(i_point,j_point,setup%level,t)	    

		! advection of the surface layer due to atmospheric winds:
			if (particles%levels(t,p)==1) then
				u0 = u0 + windCorrectionFactor * air%U(i_point,j_point,t)
				v0 = v0 + windCorrectionFactor * air%V(i_point,j_point,t)
			endif

		    ! add turbulent diffusion:
		    call diffusion2D(i_point,j_point,udiff,vdiff,t,p)

		    u0 = u0 + udiff
		    v0 = v0 + vdiff	

        ! advect particles one timestep:
       	! note that backtracking is enforced via sign_t value!

		    dx = setup%sign_t * u0 * setup%dt * 3600.;
        	dy = setup%sign_t * v0 * setup%dt * 3600.;
        
        
 			call updateParticleLocation(dx,dy,dz,t,p)

	    else ! if particle is beaching: i,j stays the same +
		     ! no advection, only diffusion:

		     i_point = particles%lons(t,p)
		     j_point = particles%lats(t,p)
		     u0 = 0.0
		     v0 = 0.0

		    ! add turbulent diffusion:
		    call diffusion2D(i_point,j_point,udiff,vdiff,t,p)

		    u0 = u0 + udiff
		    v0 = v0 + vdiff	

        ! advect particles one timestep.
        ! note that backtracking is enforced via sign_t value!

       		dx = setup%sign_t * u0 * setup%dt * 3600.;
        	dy = setup%sign_t * v0 * setup%dt * 3600.;
        
        
			call updateParticleLocation(dx,dy,dz,t,p)
	
	    endif

    ! 	enddo
    ! enddo

    end subroutine advectionDiffusion2D

    subroutine diffusion2D(i0,j0,udiff,vdiff,t,p)
    use ptmModule
    implicit none
    real, intent(out) :: udiff, vdiff
    integer :: sgnu, sgnv, t, p, i0, j0
    real :: random

    sgnu = +1;
    sgnv = +1;
       
    call random_number(random)
    if (random>0.5) then
        sgnu = -1;
    endif

    call random_number(random)
    if (random>0.5) then
        sgnv = -1;
    endif

	! determine whether to use default fixed or model values of diffusivity:
	if (setup%useOceanModelDiffusivity) then
		! set possible missingValues (=9999.) to zero (these values occur during first step)
		if (sea%HorizontalDiffusivity(i0,j0,particles%levels(t,p),t).gt. 100.) then
			Dh = 0.0
		else
			Dh = sea%HorizontalDiffusivity(i0,j0,particles%levels(t,p),t)  
		endif
	else  ! default values
		Dh = setup%Dh
	endif

    call random_number(random)
    udiff = sgnu*sqrt(2*setup%Dh/(setup%dt*3600))*random;
    vdiff = sgnv*sqrt(2*setup%Dh/(setup%dt*3600))*random;

    end subroutine diffusion2D
