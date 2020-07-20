This showcase repository contains two projects to present programming skills 
in C++, Fortran90 and Python (and a little shell scripting).
The code to found here were mostly written during my PhD time in the period 
of 2013 - 2015.
Since both projects were intended for private use and aimed mostly to just do 
their job, beautiful coding style was not really a priority, though they are 
not too horrible.

ICalc:
This project consists various routines written in C++ and Python2 to perform 
massive integration jobs. 
During my PhD I had to calculate many complicated singular integrals in several 
thousand points. 
The integration itself can be done numerically using the program named 'SecDec' 
for example, however the program itself is not designed for mass calculations. 
In addition, all the results produced be SecDec are not in an appropiate format 
for further processing tasks. 
Since this is a job, which cries for automation, I decided to build a framework 
on top of SecDec, which can perform all tedious subtasks automated, such as distributing 
input files, running several integration routines parallel, collecting and transforming 
results into different file formats with a 'push of a button'.
ICalc certainly proved itself a very useful tool.

antikt-cluster
This project is written in Fortran90 and implements a newer way of jet clustering 
for the so-called anti-kt jet clustering algorithm, which is used in particle physics.
These kind of clustering algorithms take a set of particles as input and combine 
them into larger objects, what we name jets, according to their settings. 
During PhD I did some calculations which involved clustering with the antikt-algorithm. 
However this algorithm lacks certain properties, which would make possible the use of 
computational shortcuts, and slowed down my calculations too much. 
Therefore I started to dig into the literature in order to find a faster way and managed 
to develope an algorithm to speed up my calculations.
In order to test it, I wrote this piece of code (which includes the FJCORE and RAMBO programs 
from third parties), and updated it recently to produce results for my paper 
(see more here: https://arxiv.org/abs/1911.03487), as I realised on a boring lecture on a 
conference in 2019, that I could actually publish it.
