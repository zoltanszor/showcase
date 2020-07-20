#!/bin/sh
#makemath
echo "Compiling makemath.."
g++ makemath/makemath.cpp makemath/MData.cpp -o makemath.exe
echo "Done."

#collectres
echo "Compiling collectres.."
g++ collectres/collectres.cpp -o collectres.exe
echo "Done."

#makeGrid
