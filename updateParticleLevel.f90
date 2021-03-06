subroutine updateParticleLevel(dz,t,p)
	use ptmModule
	implicit none
	integer :: t,p, closestLevel
	real :: dz
	
! previous step level:
!	particles%levels(t,p)

! previous step depth [m]:   
!	abs(ncgrid%levels(particles%levels(t,p)))
      	
! depth [m] after moving for dz in vertical:
!	ncgrid%levels(particles%levels(t,p)) + dz

! Therefore the new closest level to current depth is equal to 
! the location of the minimum difference between (all) ncgrid%level
! and previous depth, increased(decreased) by dz (",1" in the minloc
! call is a dummy argument which specifies array size of the minloc output,
! therefore here: 1 (scalar)):

	if (t.lt.setup%numHoursOfTracking) then
		particles%levels(t+1,p) =  minloc(nint( abs( abs(ncgrid%levels) - &
		( ncgrid%levels(particles%levels(t,p)) + dz ) ))	,1)	
	endif
end subroutine updateParticleLevel
