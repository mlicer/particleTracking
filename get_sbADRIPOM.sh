#!/bin/bash
homedir=/home/momo/particleTracking
ncdir=${homedir}/sbADRIPOM_netcdf
nncdir=${homedir}/sbNADRIPOM_netcdf
days=4
RR=00
runDurationInDays=3
today=$(date +"%Y%m%d")
datestart=$(date --date="${today} 3 days ago" +"%Y%m%d")

# ear=$(date --date=$rundate +"%Y")
echo ${ncdir}
#mkdir -p ${ncdir}

k=0
while [ $k -lt $days ];    
do
	rundate=$(date --date="$datestart $k day" +"%Y%m%d")
	yearstart=$(date --date="${rundate}" +"%Y")
	monthstart=$(date --date="${rundate}" +"%m")
	daystart=$(date --date="${rundate}" +"%d")
	yearend=$(date --date="${rundate} $runDurationInDays day" +"%Y")
	monthend=$(date --date="${rundate} $runDurationInDays day" +"%m")
	dayend=$(date --date="${rundate} $runDurationInDays day" +"%d")
	startdate=${yearstart}-${monthstart}-${daystart}
	enddate=${yearend}-${monthend}-${dayend}
	
	#echo $rundate
	fname=zeta.ARSO.${startdate}_to_${enddate}.nc
	ncfile=/rundir_v3/momo_si/sbADRIPOM/sbADRIPOM_${rundate}${RR}/out/${fname}
	stat ${ncfile}

#	unlink ${ncdir}/${fname}
	
	if [ -f ${ncfile} ]; then
	echo $ncfile
	ln -s $ncfile $ncdir || true
	fi

	fname=zeta.ARSO.${startdate}_to_${enddate}.nc
	ncfile=/rundir_v3/momo_si/sbNADRIPOM/sbNADRIPOM_${rundate}${RR}/out/${fname}
	stat ${ncfile}

#	unlink ${ncdir}/${fname}
	
	if [ -f ${ncfile} ]; then
	echo $ncfile
	ln -s $ncfile $nncdir || true
	fi

	let k=k+1
done
