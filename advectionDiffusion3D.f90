    subroutine advectionDiffusion3D(t,p)
    ! Advection and diffusion routines for particleTracking.f90
    ! More info: matjaz.licer@mbss.org
    
    use ptmModule
    implicit none
	    real :: dx, dy, dz
	    real :: u0, v0, w0
	    real :: udiff, vdiff, wdiff
	    integer :: p,t,i_point,j_point, k_point, point


    ! test for initial point (land/sea):
    if (t==1) then
		do point = 1, setup%numberOfReleasePoints
			call get_closest_point(setup%releasePointsLats(point),&
								   setup%releasePointsLons(point),j_point,i_point)
			if ( ncgrid%lsm(i_point,j_point,setup%level) .ne. 1.0 ) then
				print *,''
				print *, "PTM ERROR: INITIAL RELEASE POINT ",setup%releasePointsLats(point),&
				setup%releasePointsLons(point),&
				"SEEMS TO BE A LAND POINT!"
				stop
			endif 
		enddo 
    endif

	if (t < setup%numHoursOfTracking) then	
	    particles%dates(t)= t
	    !particles%dates(setup%numHoursOfTracking)=setup%numHoursOfTracking

	    ! update particle grid index (i_point,j_point) from previous timestep:

	    call get_closest_point(particles%lats(t,p),&
	    particles%lons(t,p), j_point, i_point)
				
	    ! advect particles and enforce beaching if neccessary:
	    if (ncgrid%lsm(i_point,j_point,particles%levels(t,p)) .eq. 1.0) then
	
	    ! get advection velocities at this location/timestep:
		    u0 = current%u(i_point,j_point,particles%levels(t,p),t)
		    v0 = current%v(i_point,j_point,particles%levels(t,p),t)	    
		    w0 = current%w(i_point,j_point,particles%levels(t,p),t)

		! advection of the surface layer due to atmospheric winds:
			if (particles%levels(t,p)==1) then
				u0 = u0 + windCorrectionFactor * air%U(i_point,j_point,t)
				v0 = v0 + windCorrectionFactor * air%V(i_point,j_point,t)
			endif
			
		    ! add turbulent diffusion:
		    call diffusion3D(i_point,j_point,udiff,vdiff,wdiff,t,p)

		    u0 = u0 + udiff
		    v0 = v0 + vdiff
		    w0 = w0 + wdiff			    	
			

        ! advect particles one timestep:
       	! note that backtracking is enforced via sign_t value!

		    dx = setup%sign_t * u0 * setup%dt * 3600.;
        	dy = setup%sign_t * v0 * setup%dt * 3600.;
        	dz = setup%sign_t * w0 * setup%dt * 3600.;
 
			call updateParticleLocation(dx,dy,dz,t,p)
			
	    else ! if particle is beaching: i,j stays the same +
		     ! no advection, only diffusion:
			
		     i_point = particles%lons(t,p)
		     j_point = particles%lats(t,p)
			 	
		    ! add turbulent diffusion:
		    call diffusion3D(i_point,j_point,udiff,vdiff,wdiff,t,p)


		    u0 = udiff
		    v0 = vdiff	
		    w0 = wdiff	

        ! advect particles one timestep.
        ! note that backtracking is enforced via sign_t value!

       		dx = setup%sign_t * u0 * setup%dt * 3600.;
        	dy = setup%sign_t * v0 * setup%dt * 3600.;
        	dz = setup%sign_t * w0 * setup%dt * 3600.;
         
			call updateParticleLocation(dx,dy,dz,t,p)
			
	    endif ! (ncgrid%lsm(i_point,j_point,particles%levels(t,p)) .eq. 1.0)
	else
		continue
	endif ! (t < setup%numHoursOfTracking)

    end subroutine advectionDiffusion3D

    subroutine diffusion3D(i0,j0,udiff,vdiff,wdiff,t,p)
    use ptmModule
    implicit none
    real, intent(out) :: udiff, vdiff, wdiff
    integer :: sgnu, sgnv, sgnw, t, p, i0, j0
    real :: random, Dv

    sgnu = +1;
    sgnv = +1;
    sgnw = +1;
       
    call random_number(random)
    if (random>0.5) then
        sgnu = -1;
    endif

    call random_number(random)
    if (random>0.5) then
        sgnv = -1;
    endif
    
    ! in the surface layer, there can be only downward vertical diffusion:
    if (particles%levels(t,p)==1) then
    	sgnw = -1;
	else ! in the deeper layers we allow random vertical diffusion up or down:   	
		call random_number(random)
		if (random>0.5) then
		    sgnw = -1;
		endif
	endif
	
	! determine whether to use default fixed or model values of diffusivity:
	if (setup%useOceanModelDiffusivity) then
		! set possible missingValues (=9999.) to zero (these values occur during first step)
		if (sea%HorizontalDiffusivity(i0,j0,particles%levels(t,p),t).gt. 100.) then
			Dh = 0.0
		else
			Dh = sea%HorizontalDiffusivity(i0,j0,particles%levels(t,p),t)  
		endif
	
		if (sea%VerticalDiffusivity(i0,j0,particles%levels(t,p),t).gt. 100.) then
			Dv = 0.0
		else
			Dv = sea%VerticalDiffusivity(i0,j0,particles%levels(t,p),t)  
		endif	
	else ! default values
		Dh = setup%Dh
		Dv = 0.001 * setup%Dh
	endif
	
    call random_number(random)	
    	
    udiff = sgnu*sqrt(2*Dh/(setup%dt*3600))*random;
    vdiff = sgnv*sqrt(2*Dh/(setup%dt*3600))*random;
	wdiff = sgnv*sqrt(2*Dv/(setup%dt*3600))*random;
	
    end subroutine diffusion3D
