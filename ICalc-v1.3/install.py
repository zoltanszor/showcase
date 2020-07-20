"""
This is a python script to install the ICalc-v1.3 program.
Written by Zoltan Szor
Last modified: 2015.11.17
"""

#Import modules
import multiprocessing
import os
import sys
import shutil
import subprocess

#----Compile C programs----
print "Let us compile the cprograms.."
os.chdir("cprograms")
subprocess.check_call("./compile.sh")
os.chdir("..")

#----Create SecDec source dir-----
#Tar SecDec
print "Tar SecDec..."
subprocess.check_call("tar -xf SecDec-2.1.tar.gz",shell=True)

#Patch SecDec
print "Now patch SecDec..."
#my* scripts
subprocess.check_call("cp secdecpatch/my* SecDec-2.1/general/", shell=True)
#formfortran.m
subprocess.check_call("cp secdecpatch/formfortran.m SecDec-2.1/general/src/subexp/formfortran.m", shell=True)
#SDroutines.m
subprocess.check_call("cp secdecpatch/SDroutines.m SecDec-2.1/general/src/deco/SDroutines.m", shell=True)
#makemake.pm
subprocess.check_call("cp secdecpatch/makemake.pm SecDec-2.1/general/perlsrc/makemake.pm", shell=True)
#getinfo.pm
subprocess.check_call("cp secdecpatch/getinfo.pm SecDec-2.1/general/perlsrc/getinfo.pm", shell=True)
#makeint.pm
subprocess.check_call("cp secdecpatch/makeint.pm SecDec-2.1/general/perlsrc/makeint.pm", shell=True)
#polenumerics.pl
subprocess.check_call("cp secdecpatch/polenumerics.pl SecDec-2.1/general/perlsrc/polenumerics.pl", shell=True)
#preparenumerics.pl
subprocess.check_call("cp secdecpatch/preparenumerics.pl SecDec-2.1/general/perlsrc/preparenumerics.pl", shell=True)
print "Done.."

#----Install SecDec----
print "Now install SecDec."

#Check how many cores do we have
ncoremax=multiprocessing.cpu_count()
#ncore=3
print "Your machine has",ncoremax,"core(s) to run SecDec & MB..."
#Get the number of desired SecDecs from the user
message="Please enter the number of SecDecs you want to install ("+str(ncoremax)+" max): "
ncore=int(raw_input(message))
if ncore>ncoremax:
  print "Error: invalid number, install script quits! Please try again!"
  sys.exit()
  

#Create ncore SecDec libraries

source='SecDec-2.1'

for i in range(1,ncore+1):

  dest='SecDec-'+str(i)
  if not os.path.exists(dest):
    print "Creating folder:",dest
    shutil.copytree(source,dest)

#Install SecDec ncore times
for i in range(1,ncore+1):

  run='SecDec-'+str(i)
  print "Install",run 
  os.chdir(run)
  subprocess.check_call("./install")
  os.chdir("..")

#Remove SecDec source dir
subprocess.check_call("rm -r SecDec-2.1",shell=True)
print "Installation done.."
