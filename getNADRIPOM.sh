#!/bin/bash
homedir=/home/momo/particleTracking
ncdir=$homedir/NADRIPOM_netcdf/
datestart=20111109
days=10
RR=00
# ear=$(date --date=$rundate +"%Y")

mkdir -p $ncdir

k=0
while [ $k -lt $days ];    
do
	rundate=$(date --date="$datestart $k day" +"%Y%m%d")
	#echo $rundate
	# treba je pognati mountmomo iz /home/mlicer, da mountamo nimbus na lokalni folder
	ncfile=/mnt/nimbus/home/momo/smsnadripom2_total/rundir/nadripom2_total_$rundate$RR/runmodel_/NADRIPOM_$rundate.nc
	ncfile=/scratch/momo_si/oper/results/nadripom2_total/nadripom2_total_$rundate$RR/NADRIPOM_$rundate.nc
        unlink $ncdir/NADRIPOM_$rundate.nc
	if [ -f $ncfile ]; then
	echo $ncfile
	ln -s $ncfile $ncdir || true
	fi
	let k=k+1
done
