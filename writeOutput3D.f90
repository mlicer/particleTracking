subroutine writeOutput3D
	! This subroutine creates a NetCDF output after the computation in
	! particleTracking.f90 has been performed.
	! More info: matjaz.licer@mbss.org 

	use ptmModule
	use netcdf
	implicit none
	include 'netcdf.inc'

	character(len=132)                  :: infilename, outfilename
	integer                             :: infile_id, outfile_id
	integer                             :: lons_id, lats_id, mass_id, dates_id, level_id
	integer                             :: timedim_id, particledim_id, dim1D(1),dim2D(2) 
	integer :: gridlats_id,gridlons_id,gridlevels_id,gridmask_id
	integer :: latdim_id,londim_id,leveldim_id, us_id, ub_id, vs_id,vb_id, cs_id, cb_id
	integer :: dim_grid2D(2),dim_level1D(1),dim_mask3D(3)
	integer :: dim_vel3D(4), dim_pol3D(3)

	character*4 :: year
	character*2 :: month,day,hour


	print *, "writeOutput3D: writing output to NetCDF... "
	year=setup%startdate(1:4)
	month=setup%startdate(5:6)
	day=setup%startdate(7:8)
	hour=setup%startdate(9:10)


	! create output file
	call handle_err( nf_create('ptm_output_'//setup%startdate//'.nc', NF_CLOBBER, outfile_id) )

	print *,"Writing to NetCDF: "//'ptm_output_'//setup%startdate//'.nc'

	call handle_err(NF_PUT_ATT_TEXT (outfile_id, NF_GLOBAL, 'title', 24,&
	'Particle Tracking Output') )
	call handle_err(NF_PUT_ATT_TEXT (outfile_id, NF_GLOBAL, 'institution', 16,&
	'ARSO / NIB / FGG') )
	call handle_err(NF_PUT_ATT_TEXT (outfile_id, NF_GLOBAL, 'circulation_model', 10,&
	setup%model) )
	call handle_err(NF_PUT_ATT_TEXT (outfile_id, NF_GLOBAL, 'oilType', 20,&
	slick%oilType) )  
	call handle_err(NF_PUT_ATT_TEXT (outfile_id, NF_GLOBAL, '_FillValue', 4,&
	'9999') )  


	! define dimensions
	call handle_err( nf_def_dim(outfile_id, "numHoursOfTracking", setup%numHoursOfTracking, timedim_id) )
	call handle_err( nf_def_dim(outfile_id, "numberOfParticles", totalNumberOfParticles, particledim_id) )
	call handle_err( nf_def_dim(outfile_id, "numberOfGridLatitudes", ncgrid%nLat, latdim_id) )
	call handle_err( nf_def_dim(outfile_id, "numberOfGridLongitudes", ncgrid%nLon, londim_id) )
	call handle_err( nf_def_dim(outfile_id, "numberOfGridLevels", ncgrid%nLevels, leveldim_id) )

	! particles, times dimensions:
	dim1D=timedim_id

	dim2D(1)=timedim_id
	dim2D(2)=particledim_id
	! grid array dimensions:
	dim_grid2D(1)=londim_id
	dim_grid2D(2)=latdim_id
	! mask array dimensions:
	dim_mask3D(1)=londim_id
	dim_mask3D(2)=latdim_id
	dim_mask3D(3)=leveldim_id
	! velocity array dimensions:
	dim_vel3D(1)=londim_id
	dim_vel3D(2)=latdim_id
	dim_vel3D(3)=timedim_id
	! pollutant array dimensions:
	dim_pol3D(1)=londim_id
	dim_pol3D(2)=latdim_id
	dim_pol3D(3)=timedim_id

	print *, 'HOURS OF TRACKING: ',setup%numHoursOfTracking
	print *, 'TOTAL NUMBER OF PARTICLES: ',totalNumberOfParticles	
	print *,'NC GRID DIMENSIONS (LON,LAT,Z,T):', shape(slick%pollutantConcentration)

	! levels array dimension:
	dim_level1D(1)=leveldim_id

	! define variables
	call handle_err( nf_def_var(outfile_id, "dates", nf_int, 1, dim1D, dates_id) )

	! add dates attribute:
	if (setup%performBacktracking) then
		call handle_err( nf_put_att_text(outfile_id, dates_id,'units', 32, &
		'hours before '//year//'-'//month//'-'//day//' '//hour//':00:00'))
		call handle_err(NF_PUT_ATT_TEXT (outfile_id, NF_GLOBAL, 'trackingType', 13,&
		'back-tracking') )    	
	else
		call handle_err( nf_put_att_text(outfile_id, dates_id,'units', 31, &
		'hours since '//year//'-'//month//'-'//day//' '//hour//':00:00'))
		call handle_err(NF_PUT_ATT_TEXT (outfile_id, NF_GLOBAL, 'trackingType', 16,&
		'forward-tracking') )     			
	endif
	
	call handle_err( nf_def_var(outfile_id, "gridLatitudes", nf_double, 2, dim_grid2D, gridlats_id) )   
	call handle_err( nf_put_att_text(outfile_id, gridlats_id,'units', 25, &
	'grid latitudes in degrees'))   
	call handle_err( nf_def_var(outfile_id, "gridLongitudes", nf_double, 2, dim_grid2D, gridlons_id) )   
	call handle_err( nf_put_att_text(outfile_id, gridlons_id,'units', 26, &
	'grid longitudes in degrees'))    
	call handle_err( nf_def_var(outfile_id, "gridZLevels", nf_double, 1, dim_level1D, gridlevels_id) )   
	call handle_err( nf_put_att_text(outfile_id, gridlevels_id,'units', 31, &
	'meters bellow ocean surface [m]'))
	call handle_err( nf_def_var(outfile_id, "gridMask", nf_double, 3, dim_mask3D, gridmask_id) )   
	call handle_err( nf_put_att_text(outfile_id, gridmask_id,'units', 28, &
	'land sea mask: 1=sea, 0=land'))  
	call handle_err( nf_def_var(outfile_id, "u-velocity", nf_double, 3, dim_vel3D, us_id) )   
	call handle_err( nf_put_att_text(outfile_id, us_id,'units', 28, &
	'zonal current velocity in m/s')) 
	call handle_err( nf_def_var(outfile_id, "v-velocity", nf_double, 3, dim_vel3D, vs_id) )   
	call handle_err( nf_put_att_text(outfile_id, vs_id,'units', 34, &
	'meridional current velocity in m/s')) 	
	call handle_err( nf_def_var(outfile_id, "particleLats", nf_double, 2, dim2D, lats_id) )   
	call handle_err( nf_def_var(outfile_id, "particleLons", nf_double, 2, dim2D, lons_id) )
	call handle_err( nf_def_var(outfile_id, "particleLevels", nf_int, 2, dim2D, level_id) )
	call handle_err( nf_put_att_text(outfile_id, level_id,'units', 55, &
	'vertical integer index of current particle grid level'))
	call handle_err( nf_def_var(outfile_id, "particleMass", nf_double, 2, dim2D, mass_id ) )
	call handle_err( nf_def_var(outfile_id, "PollutantConcentration", nf_double, 3, dim_pol3D, cs_id) )   
	call handle_err( nf_put_att_text(outfile_id, cs_id,'units', 34, &
	'pollutant concentration in kg/m**3'))	

	call handle_err( nf_enddef(outfile_id) )

	! write variables
	!dates:
	call handle_err( nf_put_var_int(outfile_id, dates_id, particles%dates) )
	! latlon grid:
	call handle_err( nf_put_var_real(outfile_id, gridlats_id, ncgrid%lat2) )
	call handle_err( nf_put_var_real(outfile_id, gridlons_id, ncgrid%lon2) )
	! vertical levels:
	call handle_err( nf_put_var_real(outfile_id, gridlevels_id, ncgrid%levels) )
	! land sea mask:
	call handle_err( nf_put_var_real(outfile_id, gridmask_id, ncgrid%lsm) )
	! particle locations:
	call handle_err( nf_put_var_real(outfile_id, lats_id, particles%lats) )
	call handle_err( nf_put_var_real(outfile_id, lons_id, particles%lons) )
	! particle levels:
	call handle_err( nf_put_var_int(outfile_id, level_id, particles%levels) )
	! particle masses:
	call handle_err( nf_put_var_real(outfile_id, mass_id, particles%mass) )
	! surface current velocity:
	call handle_err( nf_put_var_real(outfile_id, us_id, current%u(:,:,1,:) ) )
	call handle_err( nf_put_var_real(outfile_id, vs_id, current%v(:,:,1,:) ) )
	! polutant concentration	
	call handle_err( nf_put_var_real(outfile_id, cs_id,slick%pollutantConcentration(:,:,1,:) ) )

	! close grid file
	call handle_err( nf_close(outfile_id) )

	print *, "writeOuput: ALL DONE."

end subroutine writeOutput3D
