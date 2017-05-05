 subroutine updateParticleLocation(dx,dy,dz,t,p)
 	
	use ptmModule
	implicit none
	integer :: t,p, j_point, i_point
	real :: dx, dy, dz
	real :: d2r, r2d
    d2r = pi/180.;
    r2d = 180./pi;
    	
    if (t.lt.setup%numHoursOfTracking) then	
		! update particles lons:
		particles%lons(t+1,p) = particles%lons(t,p) &
		+ r2d * dx / (Rearth * cos(particles%lats(t,p)*d2r));

		! update particles lons:        	
		particles%lats(t+1,p) = particles%lats(t,p) &
		+ r2d * dy / Rearth ;

		if (setup%tracking3D) then
			! update particles levels:
			call updateParticleLevel(dz,t,p)
		else
			particles%levels(t+1,p) = setup%level
		endif
	
		! check again if diffusion ALONE ALSO kicks particles out of domain:

		call get_closest_point(particles%lats(t+1,p),&
		particles%lons(t+1,p), j_point, i_point)

		! return them back to domain if neccessary:
		if (ncgrid%lsm(i_point,j_point,particles%levels(t+1,p)) .eq. 0.0) then	
			particles%lons(t+1,p) = particles%lons(t,p) 
			particles%lats(t+1,p) = particles%lats(t,p)
			particles%levels(t+1,p) = particles%levels(t,p)
		endif
	endif
end subroutine updateParticleLocation
