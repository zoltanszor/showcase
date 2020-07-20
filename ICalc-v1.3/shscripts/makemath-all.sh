#! /bin/sh

project=$1
run=$2
runpath=$1/$2
collecteddir="collected"
configdir="Configfiles"

#Check directory
if [ -d $runpath ]; then

#Check if we already have a .m which to append the new results
  if [ -f ${runpath}/${project}_${run}_all.m ]; then
    mv ${runpath}/${project}_${run}_all.m Iall.m
    mv ${runpath}/err${project}_${run}_all.m errIall.m
  fi

#Now go through all the .res files in "collected"
  for file in `ls ${project}/${configdir}`;
  do
#   echo $file
    cprograms/makemath.exe ${project}/${configdir}/${file} ${runpath}/${collecteddir}/
  done

  mv Iall.m ${runpath}/${project}_${run}_all.m
  mv errIall.m ${runpath}/err${project}_${run}_all.m

  echo "Final results at:" ${runpath}/${project}_${run}_all.m
  echo "Error values at:" ${runpath}/err${project}_${run}_all.m

else echo "Error: Your project and/or run directory does not exist!"

fi
