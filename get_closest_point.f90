subroutine get_closest_point(lat_in,lon_in,j_point,i_point)
! This subroutine computes integer grid point indices j_point, i_point 
! in the grid ncgrid type from input latitude/longitude lat_in,lon_in.
! More info: matjaz.licer@mbss.org 

	use ptmModule
 	implicit none

    real, intent(in) :: lat_in,lon_in
    integer,intent(out) :: i_point, j_point
    integer :: j,i
	real :: dist,distance

	! find closest point:
		dist = 10000000000000000000.;
		j_point = 0
		i_point = 0

		do i=1,ncgrid%nLon
			do j = 1,ncgrid%nLat
				! oddaljenost tocke (i,j) od (lat_in,lon_in):

				call points_dist_sphere( lat_in*pi/180., lon_in*pi/180., ncgrid%lats(j)*pi/180., ncgrid%lons(i)*pi/180., Rearth, distance )

				if (distance .lt. 0.0 ) then
					print *,'get_closest_point.f90 ERROR: NEGATIVE DISTANCE ENCOUNTERED AT:'
					print *,'lat_in, lon_in, ncgrid%lats(j), ncgrid%lons(i):',lat_in, lon_in, ncgrid%lats(j), ncgrid%lons(i)
					print *,'Distance: ',distance
					stop
				endif
				
				if (distance .lt. dist) then
					i_point = i
					j_point = j
					dist=distance
				else
					continue
				endif
			enddo
		enddo 
		
		! check output integrity:
		if ( (i_point .eq. 0) .or. &
		     (j_point .eq. 0) .or. &
		     (i_point .gt. ncgrid%nLon) .or. &
		     (j_point .gt. ncgrid%nLat) ) then
			print *, "get_closest_point ERROR: CLOSEST POINT NOT FOUND!"
			print *, 'i_point, j_point: ', i_point, j_point
			stop
		endif
		
		return
		
end subroutine get_closest_point

	subroutine points_dist_sphere( lat1, long1, lat2, long2, radius, dist )
	!*******************************************************************************
	!! POINTS_DIST_SPHERE finds the distance between two points on a sphere.
	!  Discussion:
	!    The distance is measured on the surface of the sphere.
	!
	!  Author:
	!    John Burkardt,matjaz licer
	!  Parameters:
	!    Input, real LAT1, LONG1, LAT2, LONG2, the latitude and longitude of
	!    the two points, in RADIANS!!!
	!    Input, real RADIUS, the radius of the sphere.
	!    Output, real DIST, the distance between the points.
	!
	implicit none
	!
	real dist
	real lat1
	real lat2
	real long1
	real long2
	real radius
	real theta
	!
	!  theta = acos ( sin ( lat1 ) * sin ( lat2 ) &
	!               + cos ( lat1 ) * cos ( lat2 ) * cos ( long1 - long2 ) )

	! haversine formula: better numerically conditioned for small distances 
	! added by matjaz licer Sep 2013
	
	theta = atan(sqrt((cos(lat2)*sin(long1-long2))**2 + (cos(lat1)*sin(lat2)-sin(lat1)*cos(lat2)*cos(long1-long2))**2 ) &
	/(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(long1-long2)))
	dist = radius * theta

	return
	end subroutine points_dist_sphere
