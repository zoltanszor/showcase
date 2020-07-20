#! /bin/sh

#Arguments: Project Run
runpath=$1/$2

workdir=`pwd`
resultsdir="results"
collecteddir="collected"

#Check if runpath exists
if [ -d $runpath ]; then

  echo "Let's collect all the .res files!"

#We go through all the dirs which can be found in "results" 
  for dir in `ls ${runpath}/${resultsdir}`;
  do
   outname="${dir}_all.res"

#Now go through the files in one specific dir
   for filename in `ls ${runpath}/${resultsdir}/${dir}/${dir}*.res`;
   do
#Run collectres.exe
   cprograms/collectres.exe $filename ${runpath}/${collecteddir}/$outname

   done
  done

  echo "Done!"

#Directory doesn't exist
else echo "Error: Your project and/or run directory does not exist!"

fi

 
