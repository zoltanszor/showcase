/*-------------collectres----------
This program is made for get data from text style .res
files (like the output of SecDec)
and put them into a table format.
Written by Zoltan Szor
Last modified 2015.04.30.
*/

#include<iostream>
#include<fstream>
#include<string>
#include<vector>
#include<sstream>
#include<cmath>
#include<cstdio>
#include<cstdlib>

using namespace std;

bool FileExists(const char *filename){
  ifstream ifile(filename);
  return ifile;
};


int main(int argc, char *argv[]){


 string tempstring;
 string point;
 string result;
 string error;
 vector<string> points;
 vector<string> results;
 vector<string> errors;
 bool filegood;
 int from,to;
 vector<int> poles;
 int pcount=0;

//Check file
if((filegood=FileExists(argv[1]))){

//OK open
 ifstream inputfile(argv[1],ifstream::in);

 while(!inputfile.eof()){
 getline(inputfile, tempstring);

 //Search for variable points and get all of them
  if(tempstring.find("point")!=-1){

   int pos=tempstring.find("=");
   string tempvariable;

   while(1){
    if(tempstring.find(",",pos+1)!=-1){
     for(int i=pos+1; i<tempstring.find(",",pos+1);i++) tempvariable.push_back(tempstring[i]);

     pos=tempstring.find("=",pos+1);
     points.push_back(tempvariable);
     //cout << tempvariable << endl;
     tempvariable.clear();
     }   
     //last variable
    else{
     for(int i=pos+1; i<tempstring.size();i++) tempvariable.push_back(tempstring[i]);
     points.push_back(tempvariable);
     //cout << tempvariable << endl;
     tempvariable.clear();
      break;
     }   
   }   
  }   

//polecheck
  if(((from=tempstring.find("eps^"))!=-1) && ((to=tempstring.find("coeff"))!=-1))
  {
   string tmp_pole;
   for(int i=from+4; i<to; i++) tmp_pole.push_back(tempstring[i]);

   poles.push_back(atoi(tmp_pole.c_str()));

//we missed one pole, fill it with 0s
  // if(poles[pcount]-poles[0]!=pcount){
  while(poles[pcount]-poles[0]>pcount){

    results.push_back("0");
    errors.push_back("0");
    poles.push_back(poles[pcount]); //put the rigth pole at the rigth place
    poles[pcount]=poles[0]+pcount; //insert the missing pole in the vector

    pcount++;
 
   }
 
   pcount++;

   //cout << tmp_pole << " " << pole << endl;
  }// end polecheck

 //Search results & erros and get them
   if((tempstring.find("result")!=-1) && (tempstring.find("Integration")==-1))
   {
    string tempvariable;
    for(int i=tempstring.find("=")+1; i<tempstring.size(); i++) tempvariable.push_back(tempstring[i]);
    //cout << tempvariable << endl;
    results.push_back(tempvariable);
    tempvariable.clear();

  //errors
    getline(inputfile, tempstring);
    for(int i=tempstring.find("=")+1; i<tempstring.size(); i++) tempvariable.push_back(tempstring[i]);
    //cout << tempvariable << endl;
    errors.push_back(tempvariable);
    tempvariable.clear();

   }

 }//while end

 inputfile.close();
}

else cout << "Error: Input doesn't exists!" << endl;

//out stream
if(filegood){
 fstream outputfile(argv[2], fstream::out | fstream::app);

 for(int i=0; i<points.size();i++) outputfile << points[i] << " ";
 for(int i=0; i<results.size();i++){

  if( atoi(errors[i].c_str()) < 0 ) outputfile << results[i] << " " << pow(10.0,atoi(errors[i].c_str())) << " ";
  else outputfile << results[i] << " " << errors[i] << " ";  
 }
 outputfile << endl;

 outputfile.close();
}



return 0;
}
