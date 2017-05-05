subroutine readOceanModelInput
! This is a wrapper subroutine for all NetCDF reading required by
! particleTracking.f90. Reading subroutines are in ptmModule module.
! More info: matjaz.licer@mbss.org

	use ptmModule
	implicit none

	call readNetcdfGrid(trim(adjustl(setup%existingFiles(1))))
	print *,"GRID SIZE: ",ncgrid%nLat, ncgrid%nLon, ncgrid%nLevels
		
	if (setup%tracking3D) then
		call readUV3D
	    call readOCEAN3D
    else
		call readUV2D   
	    call readOCEAN2D		 	   
    endif
    

    
end subroutine readOceanModelInput
