"""
This is a python script to run and manage the elements
of the ICalc program
Version 1.3
Written by Zoltan Szor
Last modified 2015.11.17.

Usage: python run.py project run.run

If you need to run secdec only:
Use 'secdec_single' command as 'runtype'

If you need to run multinumerics only, 
Use 'secdec_multi' command as 'runtype'
"""

#Import modules
import os
import sys
import re
import multiprocessing
import glob
import shutil
import subprocess
import time


#Define function to run shscripts
def run_sh(path,scriptname):
    """Run shellscript "scriptname" from dir named "path"
       and writes it's output into a log file """
# Get pwd
    pwd=os.getcwd()

#Check path existence
    if not os.path.exists(path):
      print "Error: Path does not exist!"
      sys.exit()

#OK go to path
    else:
      os.chdir(path)
#Check script existence
      if not os.path.exists(scriptname):
        print "Error: script named",scriptname,"does not exist!"
        sys.exit()
#Ok run script
      else:
        print "Running shellscript",scriptname,"at",path
# logfile for output
        logname=scriptname[:len(scriptname)-2]+'log'
        command='./'+scriptname+' > '+logname
        subprocess.check_call(command,shell=True)
#Go back to original directory
      os.chdir(pwd)

#End function run_sh

#Define function to check if a string is an integer
#NOTE: this implementation is from stackoverflow and slightly modified
def is_int(s):
    try:
        int(s)
        return True
    except ValueError:
        return False


#---------------------------------------------------

#Get arguments
projectname=sys.argv[1]
runfile=sys.argv[2]
runfilepath=projectname+'/'+runfile
maindir=os.getcwd()

#Check .run file
if not os.path.exists(runfilepath):
  print "Error: run file does not exist!"
  sys.exit()

#OK, open .run file
else:
  with open(runfilepath,'r') as runinput:

#First of all let set the parallel number of CUBA threads
#Default will be 1, other values can be set by the user modifying the following line 
    print "We set CUBACORES to 1. In case of other values needed the user must modify the line no. 89 in the 'run.py' script!"
    os.environ['CUBACORES'] = '1'

#Read each lines
    for line in runinput:

#Get variable values from the .run file
#  runname
     if re.match("runname=",line):
       print "Name of this run:",line[len("runname="):len(line)-1]
       runname=line[len("runname="):len(line)-1]

#  cpumod
     elif re.match("cpumod=",line):
       print "CPU mod:",line[len("cpumod="):len(line)-1]
       cpumod=line[len("cpumod="):len(line)-1]

#  runtype
     elif re.match("runtype=",line):
       print "Run type:",line[len("runtype="):len(line)-1]
       runtype=line[len("runtype="):len(line)-1]

#  collect
     elif re.match("collect=",line):
       print "Collect data at the end?",line[len("collect="):len(line)-1]
       collect=line[len("collect="):len(line)-1]

#Create directory runname
rundir=projectname+'/'+runname
if not os.path.exists(rundir):
  print "Creating directory",runname
  os.chdir(projectname)
  os.mkdir(runname)
  os.chdir(runname)

#Create directory "collected" & "results" in runname
  os.mkdir("collected")
  os.mkdir("results")
  os.chdir(maindir)

#Set number of paralel runs
ncoremax=multiprocessing.cpu_count()
#ncoremax=2

#Number mode
if is_int(cpumod):
  nthread=int(cpumod)
  if nthread>ncoremax:
    print "Error: value of cpumod (",nthread,") is higher than number of available cores (",ncoremax,")!"
    sys.exit()

#Low mode
elif cpumod=='low':
  nthread=1

#Moderate mode
elif cpumod=='moderate':
  nthread=int(round(ncoremax/2.0))

#Full mode
elif cpumod=='full':
  nthread=ncoremax

#Invalid mode
else:
  print "Error: Invalid cpumod!"
  sys.exit() 

#Precheck SecDec sub runtypes

#Generally we do everything
secdec_launch=True
secdec_multinum=True

#We only want ./launch
if runtype=='secdec_single':
  secdec_multinum=False
  runtype='secdec'

#We only want mymultinumerics
elif runtype=='secdec_multi':
  secdec_launch=False
  runtype='secdec'
   


#Get current local time to measure the cost of the process
tstart=time.localtime()
tstart_sec=time.time()


#-----Runtype SecDec-----
if runtype=='secdec':

#First check how many installed SecDecs do we have
  nsecdec_max=0
  for tmp in glob.glob("SecDec-*"):
    if not os.path.isdir(tmp):
      continue
    nsecdec_max=nsecdec_max+1


#Compare nsecdec with the desired number of threads
  if nthread>nsecdec_max:
    print "Warning: The number of desired threads (",nthread,") are higher than the number of available SecDecs (",nsecdec_max,")! Number of threads has been reset to ",nsecdec_max," from ",nthread,"!"
    nthread=nsecdec_max

#Path of SecDecsource  
  secdecsource=projectname+'/SecDecsource'


#Init an empty list to store script texts
  runscripts=[""]

#Create run directories in SecDec directories
  for i in range(1,nthread+1):
    runpath='SecDec-'+str(i)+'/general/'+runname
    if not os.path.exists(runpath):
      secdecpath='SecDec-'+str(i)+'/general'
      os.chdir(secdecpath)
      os.mkdir(runname)
      os.chdir(maindir)
# Begin runscripts list nthread times
    runscripts.append("#!/bin/sh\n")

#Go though all integral in SecDecsource
#and copy the into the corresponding SecDec run directory

# tnum init value  
  tnum=1

#Start loop over integrals
  for integral in os.listdir(secdecsource):

#Check if integral is a directory, if not go to next cycle
    if not os.path.isdir(secdecsource+'/'+integral):
      continue

#cd to integral dir
    os.chdir(secdecsource+'/'+integral)

#Search secdec .input files
    for secdecinput in glob.glob(integral+'_*.input'):

#Construct .m and values.input file names
      secdecmath=secdecinput[0:len(secdecinput)-5]+'m'
  
      if secdec_multinum:
        secdecvalues=integral+'_values_'+secdecinput[len(integral)+1:]
        valuessource=maindir+'/'+projectname+'/Values/'+secdecvalues

#Check .m and values.input file's existence
      if not os.path.exists(secdecmath):
        print "Error: Corresponding .m file is missing for",secdecinput
        sys.exit()

      if secdec_multinum: 
        if not os.path.exists(valuessource):
          print "Error: Corresponding values.input file is missing for",secdecinput
          sys.exit()
         
#Copy destination
      cpdest=maindir+'/SecDec-'+str(tnum)+'/general/'+runname+'/'
      
#Copy files to secdec directories
      shutil.copy(secdecinput,cpdest)
      shutil.copy(secdecmath,cpdest)
      if secdec_multinum:
        shutil.copy(valuessource,cpdest)

#Create runscript line
      scriptline="echo \"Running "+secdecinput+"\"\n"
      if secdec_launch:
        scriptline=scriptline+"./launch -d "+runname+" -p "+secdecinput+" -t "+secdecmath+"\n"
      if secdec_multinum:
        scriptline=scriptline+"perl mymultinumerics.pl -d "+runname+" -p "+secdecvalues+"\n"

#Add scriptline to the corresponding list member of runscript
      runscripts[tnum]=runscripts[tnum]+scriptline

# next time use the next thread
      tnum=tnum+1

# we've reached the max n of threads, go to the first one
      if tnum > nthread: tnum=1

#Go back to the maindir
    os.chdir(maindir)
#End of loop over integrals

#Create runscript files in SecDec directories
  shpaths=[] #container for scriptpaths for later use
  for i in range(1,nthread+1):
    scriptpath='SecDec-'+str(i)+'/general/secdec_'+runname+'.sh'
    shpaths.append('SecDec-'+str(i)+'/general')

    with open(scriptpath,'w') as script_file:
      script_file.write(runscripts[i])

#Make scripts executable
    chmodcmd='chmod a+x '+scriptpath
    os.system(chmodcmd)

#Run secdec scripts parallel
#Scriptname
  scriptname='secdec_'+runname+'.sh'

#Setup list of processes
  scriptprocs=[multiprocessing.Process(target=run_sh,args=(shpaths[i],scriptname)) for i in range(0,len(shpaths))]

#Run processes
  for proc in scriptprocs:
    proc.start()

#Exit completed processes
  for proc in scriptprocs:
    proc.join()

#Integration is done
  print "All integration is done!"

#Now copy results into run/results
  print "Now copy results into",rundir+'/results'

  for i in range(1,nthread+1):
    secdec='SecDec-'+str(i)+'/general'
    os.chdir(secdec)
#Scan integrand directories
    for integrand in os.listdir(runname):

#We only need directories
      if os.path.isdir(runname+'/'+integrand):
        resdest=maindir+'/'+rundir+'/results/'+integrand

#Create integrand directory at "results" if it doesn't exist
        if not os.path.exists(resdest):
          os.mkdir(resdest)

#Scan *full.res files in integrand directory and copy them to results
        for resfile in glob.glob(runname+'/'+integrand+'/*full.res'):
          shutil.copy(resfile,resdest+'/')
#Go back to maindir
    os.chdir(maindir)
 
#-----End of runtype secdec-----


#-----Runtype mb-----
#NOTE: at the moment this is specified to I2Si only & operates with
#the non-alt versions
elif runtype=='mb':

  mbsource=projectname+'/MBsource'
  os.chdir(mbsource)

#tnum init value  
  tnum=1

#Init an empty list to store script texts
  runscripts=[""]

#Begin runscripts list nthread times
  for i in range(1,nthread+1):
    runscripts.append("#!/bin/sh\n")

#Loop over integrals
  for integrand in glob.glob("I2S*"):

    mbvalues=integrand+'_values.input'
    valuessource=maindir+'/'+projectname+'/Values/'+mbvalues
    cpdest=integrand+'/'

#Check .input file
    if not os.path.exists(valuessource):
      print "Error: Corresponding values.input file is missing for", mbinput
      sys.exit()

#Copy .input file into integegrand directory
    shutil.copy(valuessource,integrand+'/')

#Create runscript line
    scriptline="echo \"Running "+integrand+" MB\"\n"
    scriptline=scriptline+"cd "+integrand+"\n"
    scriptline=scriptline+"math < "+integrand+"_MB.m\n"
    scriptline=scriptline+"cd ..\n"

#Add scriptline to the corresponding list member of runscript
    runscripts[tnum]=runscripts[tnum]+scriptline

# next time use the next thread
    tnum=tnum+1

# we've reached the max n of threads, go to the first one
    if tnum > nthread: tnum=1
#End of loop over integrals


#Create runscript files in SecDec directories
  shpaths=[] #container for scriptpaths for later use
  scriptnames=[] #container for scriptnames
  for i in range(1,nthread+1):
    scriptnames.append('mb_'+runname+'_'+str(i)+'.sh')
    shpaths.append('.')

    with open(scriptnames[i-1],'w') as script_file:
      script_file.write(runscripts[i])

#Make scripts executable
    chmodcmd='chmod a+x '+scriptnames[i-1]
    os.system(chmodcmd)

#Run mb scripts parallel

#Setup list of processes
  scriptprocs=[multiprocessing.Process(target=run_sh,args=(shpaths[i],scriptnames[i])) for i in range(0,len(shpaths))]

#Run processes
  for proc in scriptprocs:
    proc.start()

#Exit completed processes
  for proc in scriptprocs:
    proc.join()

#Integration is done
  print "All integration is done!"


#Now copy results into run/results
  print "Now copy results into",rundir+'/results'

#Scan integrand directories
  for integrand in glob.glob("I2S*"):

    resdest=maindir+'/'+rundir+'/results/'+integrand

#Create integrand directory at "results" if it doesn't exist
    if not os.path.exists(resdest):
      os.mkdir(resdest)

#Scan *full.res files in integrand directory and copy them to results
    for resfile in glob.glob(integrand+'/results/*full.res'):
      shutil.copy(resfile,resdest+'/')

#Go back to maindir
  os.chdir(maindir)

#-----End of runtype mb


#-----Invalid runtype
else:
  print "Error: Invalid runtype!"
  sys.exit()


#-----run collectres & makemath
if collect=="yes" :
#Create commands
  collectres_cmd='shscripts/collectres-all.sh '+projectname+' '+runname
  makemath_cmd='shscripts/makemath-all.sh '+projectname+' '+runname

#Run commands
  subprocess.check_call(collectres_cmd,shell=True)
  subprocess.check_call(makemath_cmd,shell=True)

#-----Print timing
tend_sec=time.time()
tend=time.localtime()
print "Process started at",time.asctime(tstart)
print "and ended at",time.asctime(tend)
print "Process cost", (tend_sec-tstart_sec)/60.0,"minutes"

