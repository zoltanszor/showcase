#ifndef USEFULFUNCTIONS_H
#define USEFULFUNCTIONS_H

#include <iostream>
#include <vector>
#include <fstream>
#include <string>

using namespace std;

/*Collection of useful functions collected from
 the internet,and written by Zoltan Szor*/

//Checking of file existence
bool FileExists(const char *filename){
  ifstream ifile(filename);
  return ifile;
};





#endif
