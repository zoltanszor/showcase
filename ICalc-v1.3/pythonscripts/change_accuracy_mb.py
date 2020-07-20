"""
This is short python script which scans the lines of a MB .m file
given as an argument and do the necessary replacements.
"""

#Import modules
from tempfile import mkstemp
from shutil import move
from os import remove
import sys
import re
import glob

#Patterns what we want to...
patt1=re.compile('\d')

#...replace to this
newexp=sys.argv[2]
print "Replace to new accuracy " + str(newexp)

#Get source file name from argument
path=sys.argv[1]

#Create tmp file
fh, tmp_path=mkstemp()

print "Replacement has been made in:"

#Run through multiple files
for input_path in glob.glob(path):

#Open files
  with open(input_path,'r') as input_file, open(tmp_path,'w') as tmp_file:

#Scan lines
    for line in input_file: 

#We have a match
      if re.match("epsrel",line):
#  Do the replacement and copy
        newline=patt1.sub(newexp,line)
        tmp_file.write(newline)

#No match, just copy
      else:
        tmp_file.write(line)

#Delete old source file
  remove(input_path)

#Move tmp file into new source file
  move(tmp_path,input_path)

#Print success
  print 'in ' + input_path + ': ' + newline,

#Done & happy
