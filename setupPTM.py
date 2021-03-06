#!/sw/local/anaconda2/bin/python
#  !/sw/local/python-3.5.1/bin/python3

# MAIN PREPROCESSING INPUT for fortran particle tracking code 
# particleTracking.f90

# ------------------ HOW TO RUN THE CODE:
# 0. Link neccessary NetCDF's using $sourcedir/getADRIPOM.sh script. 
# On VENTUS the main (test) folders are the following:
#			sourcedir=/home/momo/particleTracking/source(_test)
#			rundir=/home/momo/particleTracking/rundir(_test)
#			scriptdir=/home/momo/particleTracking/scripts
#			resultdir=/home/momo/particleTracking/results
# 1. Modify ptm() class in $sourcedir/setupPTM.py as needed, and :
#
#	qsub run_parallel_ptm(_test).sh

# More information: matjaz.licer@nib.si, September 2015

#=======================================================================
import os,sys, math
from sys import exit as q
import numpy as np
#from Scientific.IO.NetCDF import NetCDFFile
from netCDF4 import Dataset
#from dateutil import parser
from datetime import datetime,timedelta
from time import mktime
#timezones:
from pytz import *
#==================USER INPUT DATA HERE=================================
class ptm():
	# class containing all the basic simulation parameters.
	def __init__(self):
		# model name:
		self.model = 'sbNADRIPOM'
		# set home directory:
		self.homedir='/home/momo/particleTracking/source_sb/'
		# set out directory for namelist:
		self.outdir='/home/momo/particleTracking/rundir/'		
		# set netcdf directory (containing hydrodynamics netcdfs):
		self.ncdir='/home/momo/particleTracking/'+self.model+'_netcdf/'
		# start date of particle tracking (YYYYMMDDHH):
		startdate = str(2017032600)
		# initial location of spill:
		#self.lat0=44.806286
		#self.lon0=13.143596
		# release type: 
		#	'singlePoint' = point release for releaseDuration from the first releasePoints point
		#	'multiplePoints'= point release for releaseDuration from all releasePoints point
		#	'polygon' = instantaneous (releaseDuration will be set to 1!!) linear 
		#	release along the SIDES of releaseShape polygon points.
		#	Choosing multiple (N) points will increase total number of particles N-times!
		self.releaseType='singlePoint'
		#self.releaseType='multiplePoints'
		# self.releaseType='polygon'				
		# releasePoints: [ [lat1,lon1] , [lat2,lon2] , ... , [latN,lonN] ]
		#self.releasePoints=[ [45.7239,13.603] , [45.5042,12.742], [44.9711, 12.609] ]	
		self.releasePoints=[[45.5904065,13.6993]]
		# self.releasePoints=[ [45.1793,12.7857], [44.673,13.0229],[45.1793,13.26],[44.882,12.601],[44.882,13.4447],[45.1793,12.7857] ]			

		# depth level of the spill (1=surface)
		self.level=1
		# perform 3D tracking: (True=3D, False=single depth level)
		self.tracking3D=False		
		if self.tracking3D:
			print "WARNING: 3D tracking IS NOT YET SUPPORTED in sbADRIPOM!"
			print "Setting: self.tracking3D=False!"
			self.tracking3D=False		
		# perform back-tracking (True) or regular particle tracking (False):
		self.performBacktracking=False
		# number of hours of tracking simulation ('polygon' release type will set this to 1):
		self.numHoursOfTracking=24
		# perform daily hotstart (initialize particles from previous step) - to be continued:
		# self.performHotstart=False	
		# duration of particle release in hours:
		self.releaseDuration = 2		
		# number of particles PER RELEASE POINT:
		numberOfParticles=120
		# total mass of polutant [kg]:
		self.totalMass=20000.
		# oil type index:
		# 1 = arabian light
		# 2 = arabian medium
		# 3 = arabian heavy
		self.oilIndex=2
		# what percentage of particles evaporates completely:
		self.completeEvapPercentage=0.2
		# evaporation threshold for particles that do not evaporate completely:
		# i.e., self.evapThreshold=0.77 implies that 77 percent of each particle 
		# which does not evaporate completely, will evaporate. 
		self.evapThreshold=0.77		
		# do we read turbulent diffusivity from the ocean model or not:
		self.useOceanModelDiffusivity=False
		if self.useOceanModelDiffusivity:
			print "WARNING: useOceanModelDiffusivity IS NOT YET SUPPORTED in sbADRIPOM!!!"
			print "Setting: self.useOceanModelDiffusivity=False!!!"
			self.useOceanModelDiffusivity=False
		# default turbulent diffusivity 
		# (this is overridden by the Dh and Dv from the model, if
		# self.useOceanModelDiffusivity=True
		self.Dh=0.001
		# create startdate datetime object:
		self.startdate=datetime.strptime(startdate, "%Y%m%d%H")
		# specify number of hours in each model netcdf:
		if self.model=='sbADRIPOM':
			self.modelRuntimeInHours=74 #54
		elif self.model=='sbNADRIPOM':
			self.modelRuntimeInHours=72
		else:
			print "INVALID MODEL: "+self.model
			q()
		# specify time increment in hours in each model netcdf:
		self.dt=1		

#=======END OF USER INPUT===============================================
#=======================================================================
#=======DO NOT MODIFY THE CODE BELOW!===================================
#=======================================================================

		#determine temporal sign and limit duration of spill:
		if self.performBacktracking:
			self.sign_t = int(-1)
			self.releaseDuration=1
		else:
			self.sign_t = int(+1)

		#determine enddate:
		self.enddate= self.startdate + self.sign_t * timedelta(hours=self.numHoursOfTracking)
		self.startingHour = int(datetime.strftime(self.startdate,"%H"))
		
		# how many dates does the simulation span (we need to include starting Hour because this may 
		# brind an extra day, depending on the hours of tracking):
		self.numDaysOfTracking=int(math.ceil((self.startingHour + self.numHoursOfTracking)/24)+1)
		
		self.modelRuntimeInDays=int(self.modelRuntimeInHours/24)

		# determine release coordinates according to releaseType:
		if self.releaseType == 'singlePoint':
			self.releasePoints=[[self.releasePoints[0][0],self.releasePoints[0][1]]]
		# elif self.releaseType=='multiplePoints':
		# 	pass
		elif self.releaseType=='polygon':
			self.releaseDuration=1
		else:
			print("SETUP ERROR: unknown releaseType: "+self.releaseType)
			print("Should be 'singlePoint' or 'polygon'!")
			q()
			
		self.numberOfReleasePoints=len(self.releasePoints)		
		self.numberOfParticles = max(numberOfParticles, self.releaseDuration * self.numberOfReleasePoints)
		# if numberOfParticles<release duration, create at least one particle per step:

		self.releaseDuration=min(self.releaseDuration,self.numberOfParticles, self.numHoursOfTracking)

		self.numCreatedParticlesPerStep=max(1, int(self.numberOfParticles/self.releaseDuration ))
										
#=======================================================================
def readNC(fname,variable):
	# open netcdf:
	# f = NetCDFFile(fname.replace('\\',''), 'r')	
	f = Dataset(fname.replace('\\',''), 'r')	
	# read variable:
	if variable=='Time':
		modelRuntimeInNetcdfUnits=f.variables[variable].shape
	
		t0 = getattr(f.variables[variable],'units')[12:]	
		# create datetime object from t0:	
		t0 = datetime.strptime(t0, "%Y-%m-%d %H:%M:%S")
		# loop over hourly values and add to t0:
		hours=np.array(f.variables[variable])
		datesInFile = np.array([t0 + timedelta(hours=int(i)) for i in hours])
		return datesInFile
	else:
		return np.array(f.variables[variable])
#=======================================================================
def checkForNeccessaryFiles(ptm):
	print("-------------------------------------------")
	print("Starting particle tracking preprocessing...")
	print("Checking for neccessary files...")

	# create integers of start date and end date:
	startDate=int(datetime.strftime(ptm().startdate,"%Y%m%d%H"))
	endDate=int(datetime.strftime(ptm().enddate,"%Y%m%d%H"))

	# MAIN LOOP over days of tracking:
	numOfHoursToBeReadFromFiles=[]
	neccessaryFiles = []
	days=[]
	days_dt=[]
	files=[]
	hourReadStart=[]
	hourReadEnd = []	
	daysToRead=[]

	for k in range(ptm().numDaysOfTracking):
		# current date datetime object:
		currentDatetime = ptm().startdate+ptm().sign_t*timedelta(days=k)
		# current day string:
		day_str=datetime.strftime(currentDatetime,"%Y%m%d")		

		# netcdf file of current day:
		filename = getModelName(ptm,day_str)

		# add day to days if not already there:
		if day_str not in days:
			days=np.append(days,day_str)
			days_dt = np.append(days_dt,currentDatetime)
		
		# add current file to files if not already there:
		if filename not in files:
			files=np.append(files,filename)
		
		# filter out only dates for actually existing files:
		existingDates=[d for i,d in enumerate(days_dt) if os.path.isfile(files[i])]
		existingFiles=[f for i,f in enumerate(files) if os.path.isfile(files[i])]

	# check for first day:
	firstday=datetime.strftime(ptm().startdate,"%Y%m%d")
	# first file name - it's not yet known if it exists:
	firstfile = getModelName(ptm,firstday)
	# this will later be added to how many days we neet to read:
	increaseDaysToRead0=0

	if not os.path.isfile(firstfile):

		possibleFirstFileExists=False

		# loop over model runtime days at current date:
		for k in range(ptm().modelRuntimeInDays):
			# go back in time as many days as neccessary and possible to locate a
			# possible first file:
			currentDatetime=ptm().startdate-timedelta(days=k)
			daynowstr=datetime.strftime(currentDatetime,"%Y%m%d")
			filenow=getModelName(ptm,daynowstr)

			# if current file exists:
			if os.path.isfile(filenow) and not possibleFirstFileExists:
				# set flag to true and increase days to read by 1:
				possibleFirstFileExists=True
				increaseDaysToRead0=increaseDaysToRead0+1
				# update existing dates and files:
				if ptm().performBacktracking:
					#change nothing
					existingDates=existingDates
					existingFiles=existingFiles
				else:
					# prepend current date and filename:
					existingDates=np.insert(existingDates,0,currentDatetime)
					existingFiles=np.insert(existingFiles,0,filenow)
			break
	else:
		possibleFirstFileExists=True

	
	# overall check if we have the file containing the first day data:
	if not possibleFirstFileExists:
		print("\nERROR: first date "+ firstday +" data could not be retrieved from existing files:\n")
		print(existingFiles)
		q()

	# check for gaps in the data:
	for i in range(len(existingDates)-1): 
		# difference in days between consecutive dates in existing dates array:
		deltaDays=abs(int((existingDates[i+1]-existingDates[i]).days))
		daysToRead = np.append(daysToRead,deltaDays)

		# if difference in insurmountable by all the hours contained in the
		# last existing file, then abort:
		if deltaDays>ptm().modelRuntimeInDays:
			daynowstr=datetime.strftime(existingDates[i],"%Y%m%d")
			print("ERROR: data gap on date "+ daynowstr +" exceeds modelRuntimeInDays!")
			q()

	# check for last day (note that the loop above leaves the last day out):
	deltaEndHours = int( abs((existingDates[-1]-days_dt[-1]).seconds)/3600. )
	lastday=datetime.strftime(ptm().enddate,"%Y%m%d")
	allowed_days = [ptm().enddate-timedelta(days=k) for k in range(ptm().modelRuntimeInDays)]
	allowed_lastfiles = [getModelName(ptm,datetime.strftime(dy,"%Y%m%d")) for dy in allowed_days]	

	# check if last file exists:
	existLastFile=False
	for lastfile in allowed_lastfiles:
		if os.path.isfile(lastfile):
			existLastFile=True
			break

	if not existLastFile:
		print("ERROR: last date could not be extracted from existing files:" )
		print(lastday)
		print(allowed_lastfiles)
		q()

	# similar condition for the last date:
	if deltaEndHours>ptm().modelRuntimeInHours and not ptm().performBacktracking:
		print("ERROR: end date file missing and could not be recovered from previous dates!")
		q()
	# or in case of back-tracking, the initial date:
	elif ptm().performBacktracking and not os.path.isfile(lastfile):
		print("ERROR: neccessary file "+lastfile+" (with earliest dates) missing for backtracking!")
		q()		

	# if there where no aborts until now, the final day exists, 
	# therefore add final day to dayToRead:

	if not ptm().performBacktracking:
		daysToRead = np.append(daysToRead,1)
	else:
		daysToRead = np.insert(daysToRead,0, 1)
	
	# correct neccessary reading intervals for missing first day, if neccessary:
	if ptm().performBacktracking:
		daysToRead[0]=daysToRead[0]+increaseDaysToRead0

	# set temporal hourly indices for start and end of netcdf array 
	# reading IN FORTRAN 1-BASED NOTATION:
	hourReadStart=[]
	hourReadEnd=[]
	lastHourLastDay=abs(int(datetime.strftime(ptm().enddate,"%H")))


	for i in range(len(existingDates)): 
		print("I,LEN:",i,len(existingDates),range(len(existingDates)))
		# if we read only first file, up until runtimeInHours:
		# w/o backtracking:
		if i==0 and not ptm().performBacktracking and len(existingDates)==1: 
			hourReadStart=np.append(hourReadStart,\
				int(datetime.strftime(ptm().startdate,"%H"))+1)
			if ptm().modelRuntimeInHours<ptm().numHoursOfTracking:
				print("ERROR: numHoursOfTracking exceeds modelRuntimeInHours.")
				print("More NetCDF files are needed!")
				q()
			else:
				hourReadEnd=np.append(hourReadEnd,ptm().numHoursOfTracking)		

		#if we read more files than just first file:
		# first day w/o backtracking:
		if i==0 and not ptm().performBacktracking and len(existingDates)>1: 
			hourReadStart=np.append(hourReadStart,\
				int(datetime.strftime(ptm().startdate,"%H"))+1)
			hourReadEnd=np.append(hourReadEnd,\
				daysToRead[i]*24)
			print('d1 ',hourReadEnd)
		#first day w backtracking:
		elif i==0 and ptm().performBacktracking: 
			hourReadStart=np.append(hourReadStart,1)
			hourReadEnd=np.append(hourReadEnd,\
			abs(int(datetime.strftime(ptm().startdate,"%H"))+1+(daysToRead[i]-1)*24))
			print('d2 ' ,hourReadEnd)
		elif i>0 and i < len(existingDates)-1: # other days, but not last day:
			hourReadStart=np.append(hourReadStart,1)
			hourReadEnd=np.append(hourReadEnd,\
			abs(daysToRead[i]*24))
			print('d3 ',hourReadEnd 			)
			# last day w/o backtracking if last file exists:
		elif i>0 and i==len(existingDates)-1 and not ptm().performBacktracking and os.path.isfile(lastfile):
			hourReadStart=np.append(hourReadStart,1)
			hourReadEnd=np.append(hourReadEnd,\
			lastHourLastDay+24)	
			print('d4',	hourReadEnd		)
			# last day w/o backtracking if last file does NOT exist:
		elif i>0 and i==len(existingDates)-1 and not ptm().performBacktracking and not os.path.isfile(lastfile):
			hourReadStart=np.append(hourReadStart,1)
			hourReadEnd=np.append(hourReadEnd,\
			lastHourLastDay+24)
			print('d5'	,	hourReadEnd	)
		elif i>0 and i==len(existingDates)-1 and ptm().performBacktracking: # last day w backtracking:
			hourReadStart=np.append(hourReadStart,1)
			hourReadEnd=np.append(hourReadEnd,\
			abs(daysToRead[i]*24))	
			print('d6'	,	hourReadEnd	)
		else:
			continue
		
	# return to main:
	vsota=0
	print('ure:', hourReadStart, hourReadEnd)
	
	for l,ura in enumerate(hourReadEnd):
		vsota = vsota + abs(hourReadEnd[l] - hourReadStart[l] +1)
		print('vsota: ',vsota)
	
	print('led: ',existingDates)
	print('ef:',existingFiles)
    
	if len(existingDates)>1:
		sumHours = int(vsota)
	else:
		sumHours = int(vsota)-24

	existingAtmFiles = [f.replace('zeta','atm') for f in existingFiles]
	existingFiles = [f.replace('zeta.sbNADRIPOM','sbnadripom_uvtide') for f in existingFiles]

	return existingFiles,existingAtmFiles,hourReadStart,hourReadEnd,sumHours
	
#=======================================================================

def createPTMnamelist(ptm,existingFiles,existingAtmFiles,hourReadStart,hourReadEnd,sumHours,namelist_path,namelist_name):
	print("\nCreating F90 namelists in file: "+namelist_name)
	ptnml=open(namelist_path+namelist_name,'w')
	ptnml.write('%s\n' %('&initialParameters ! initial parameters:'))
	ptnml.write('%s\n' %('model=\''+ptm().model+'\''))
	ptnml.write('%s\n' %('modelRuntimeInHours='+str(ptm().modelRuntimeInHours)))
	ptnml.write('%s\n' %('dt='+str(ptm().dt)))
	ptnml.write('%s\n' %('numHoursOfTracking='+str(sumHours)))
	ptnml.write('%s\n' %('releaseDuration='+str(ptm().releaseDuration)))
	ptnml.write('%s\n' %('performBacktracking='+'.'+str(ptm().performBacktracking)+'.'))
	ptnml.write('%s\n' %('numberOfParticles='+str(ptm().numberOfParticles)))
	ptnml.write('%s\n' %('numCreatedParticlesPerStep='+str(ptm().numCreatedParticlesPerStep)))
	ptnml.write('%s\n' %('totalMass='+str(float(ptm().totalMass))))
	ptnml.write('%s\n' %('oilIndex='+str(float(ptm().oilIndex))))	
	ptnml.write('%s\n' %('completeEvapPercentage='+str(float(ptm().completeEvapPercentage))))	
	ptnml.write('%s\n' %('evapThreshold='+str(float(ptm().evapThreshold))))	
	ptnml.write('%s\n' %('useOceanModelDiffusivity=.'+str(ptm().useOceanModelDiffusivity)+'.'))
	ptnml.write('%s\n' %('Dh='+str(float(ptm().Dh))))	
	ptnml.write('%s\n' %('numOfExistingFiles='+str( len(existingFiles) ) ) )
	ptnml.write('%s\n' %('level='+str(ptm().level)))
	ptnml.write('%s\n' %('tracking3D=.'+str(ptm().tracking3D)+'.'))	
	ptnml.write('%s\n' %('sign_t='+str(ptm().sign_t)))
	ptnml.write('%s\n' %('startdate=\''+str(datetime.strftime(ptm().startdate,"%Y%m%d%H"))+'\''))
	ptnml.write('%s\n' %('enddate=\''+str(datetime.strftime(ptm().enddate,"%Y%m%d%H"))+'\''))
	ptnml.write('%s\n' %('releaseType=\''+str(ptm().releaseType)+'\''))				
	ptnml.write('%s\n' %('numberOfReleasePoints='+str(ptm().numberOfReleasePoints)		))
	ptnml.write('%s\n' %('/')	)
		
	ptnml.write('%s\n' %('&files ! files to be read:'))
	ptnml.write('%s' %('existingFiles='))
	for k,fname in enumerate(existingFiles):
		if (k<len(existingFiles)-1):
			ptnml.write('%s\n' %('\''+existingFiles[k]+'\''+','))	
		else:
			ptnml.write('%s\n' %('\''+existingFiles[k]+'\''+'\n/'))

	ptnml.write('%s\n' %('&atmfiles ! wind files to be read:'))
	ptnml.write('%s' %('existingAtmFiles='))
	for k,fname in enumerate(existingAtmFiles):
		if (k<len(existingAtmFiles)-1):
			ptnml.write('%s\n' %('\''+existingAtmFiles[k]+'\''+','))	
		else:
			ptnml.write('%s\n' %('\''+existingAtmFiles[k]+'\''+'\n/'))			
	
	ptnml.write('%s\n' %('&startTimes! beginning time indices for reading per file:'))
	for k,fname in enumerate(existingFiles):
		ptnml.write('%s\n' %('hourReadStart('+str(k+1)+')='+str(int(hourReadStart[k]))))
	ptnml.write('%s\n' %('/')	)
	
	ptnml.write('%s\n' %('&endTimes! ending time indices for reading per file:'))
	for k,fname in enumerate(existingFiles):
		ptnml.write('%s\n' %('hourReadEnd('+str(k+1)+')='+str(int(hourReadEnd[k]))))		
	ptnml.write('%s\n' %('/')	)		

	ptnml.write('%s\n' %('&releaseLats ! latitudes of particle release shape:'))
	for k in range(len(ptm().releasePoints)):
		ptnml.write('%s\n' %('releasePointsLats('+str(k+1)+')='+str(ptm().releasePoints[k][0]) ))
	ptnml.write('%s\n' %('/')	)
	
	ptnml.write('%s\n' %('&releaseLons ! longitudes of particle release shape:'))
	for k in range(len(ptm().releasePoints)):
		ptnml.write('%s\n' %('releasePointsLons('+str(k+1)+')='+str(ptm().releasePoints[k][1]) ))
	ptnml.write('%s\n' %('/')	)	
	
	ptnml.close()

#=======================================================================
def getModelName(ptm,dateString):
	if ptm().model=='ADRIPOM' or ptm().model=='NADRIPOM':
		# here, dateString should be in YYYYMMDD format	
		return ptm().ncdir+ptm().model+'_'+dateString+'.nc'
	elif ptm().model=='sbADRIPOM' or ptm().model=='sbNADRIPOM':
		dtobjStart=datetime.strptime(dateString,'%Y%m%d')
		dtobjEnd = dtobjStart + timedelta(hours=ptm().modelRuntimeInHours)
		strStart=datetime.strftime(dtobjStart,"%Y%m%d")
		return  ptm().ncdir+'zeta.'+ptm().model+'_'+strStart+'.nc'
	else:
		print("\nERROR in getModelName: unknown model name: "+ptm().model)
		q()
#=======================================================================
def displayContents(fname):
	print('\n------------------')
	print('Displaying contents of: '+fname+' :')
	print('------------------\n')
	os.system('head -10000 '+fname)
	print('\n')
	print('SETUP SUCCESSFULLY DONE!\n')
#=======================================================================
#=======================================================================
#=======================================================================
def main():
	# check for neccessary files:
	existingFiles,existingAtmFiles,hourReadStart,hourReadEnd,sumHours = checkForNeccessaryFiles(ptm)
	
	# create namelist for F90 code:
	namelist_path=ptm().homedir
	namelist_name='init.nml'
	os.system('rm -f '+namelist_path+'/'+namelist_name)
	createPTMnamelist(ptm,existingFiles,existingAtmFiles,hourReadStart,hourReadEnd,sumHours,namelist_path,namelist_name)	

	# display module in the terminal:
	#displayContents(modulepath+modulename)
	
	# display namelist in the terminal:
	displayContents(namelist_path+namelist_name)
	
	# copy namelist to outdir:
	os.system('cp -f '+ptm().homedir+'/'+namelist_name+' '+ptm().outdir+'/')

	#os.system('make clean; make')
	
#=======================================================================
#===================== MAIN ============================================
#=======================================================================
if __name__=='__main__':
	main()
