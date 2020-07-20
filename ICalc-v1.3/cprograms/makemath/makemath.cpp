/*makemath
Data conversion program written by Zoltan Szor
Last time modified: 2015.06.22.
*/

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <cstdlib>

#include "UsefulFunctions.h"
#include "MData.h"

using namespace std;


int main(int argc, char *argv[]){

//---------------about text-----------------

 cout << endl << endl;
 cout << "-----------------Makemath-----------------" << endl;
 cout << "----------written by Zoltan Szor----------" << endl;
 cout << endl << "For help see the README" << endl << endl;


//----------------read-in options-------------

 string tempstring;

 //option variables
 string type;
 string serial;
 string parameters;
 string inputfile;
 string outputfile;
 string polemin, polemax;
 vector<string> variablenames;
 vector<string> outvariables;

 cout << "Checking the config file..." << endl;

//checking the config file existence 
//does not exist
if(!FileExists(argv[1])){
 cout << "Error: Config file doesn't exist! Program will quit!" << endl;
 exit(EXIT_FAILURE);
}

//file is OK
else{
 cout << "Read data from configfile..." << endl;

 ifstream configfile(argv[1], ifstream::in);

 while(!configfile.eof()){
  //get line from ifstream
  getline(configfile, tempstring);
  //if not a comment line
  if(tempstring.find("#")==-1){
  //type 
    if(tempstring.find("type")!=-1){
     for(int i=tempstring.find("=")+1; i<tempstring.size();i++) type.push_back(tempstring[i]);
     cout << "Type: " << type << endl;
    }
  //serial 
    if(tempstring.find("serial")!=-1){
     for(int i=tempstring.find("=")+1; i<tempstring.size();i++) serial.push_back(tempstring[i]);
     cout << "Serial: " << serial << endl;
    }
  //parameters
    if(tempstring.find("parameters")!=-1){
     for(int i=tempstring.find("=")+1;i<tempstring.size();i++) parameters.push_back(tempstring[i]);
     cout << "Parameters: " << parameters << endl;
    }
  //inputfile
   if(tempstring.find("inputfile")!=-1){
     for(int i=tempstring.find("=")+1;i<tempstring.size();i++) inputfile.push_back(tempstring[i]);
     cout << "Inputfile: " <<  inputfile << endl;
    }
  //outputfile
   if(tempstring.find("outputfile")!=-1){
     for(int i=tempstring.find("=")+1;i<tempstring.size();i++) outputfile.push_back(tempstring[i]);
     cout << "Outputfile: " << outputfile << endl;
    }
  //poles
    if(tempstring.find("poles")!=-1){
     for(int i=tempstring.find("=")+1;i<tempstring.find(",");i++) polemin.push_back(tempstring[i]);
     for(int i=tempstring.find(",")+1;i<tempstring.size();i++) polemax.push_back(tempstring[i]);
     cout << "Min pole: " << polemin << " Max pole: " << polemax << endl;
    }
  //variables
    if(tempstring.find("variables")!=-1){
     cout << "Variables: ";
     int pos=tempstring.find("=");
     string tempvariable;
     while(1){
      if(tempstring.find(",",pos+1)!=-1){
       for(int i=pos+1; i<tempstring.find(",",pos+1);i++) tempvariable.push_back(tempstring[i]);
       pos=tempstring.find(",",pos+1);
       variablenames.push_back(tempvariable);
       cout << tempvariable << ", ";
       tempvariable.clear();
       }
       //last variable
      else{
       for(int i=pos+1; i<tempstring.size();i++) tempvariable.push_back(tempstring[i]);
       variablenames.push_back(tempvariable);
       cout << tempvariable << endl;
       tempvariable.clear();
        break;
       }
     }
    } 
  //outvariables
    if(tempstring.find("outvar")!=-1){
     cout << "Output variables: ";
     int pos=tempstring.find("=");
     string tempvariable;
     while(1){
      if(tempstring.find(",",pos+1)!=-1){
       for(int i=pos+1; i<tempstring.find(",",pos+1);i++) tempvariable.push_back(tempstring[i]);
       pos=tempstring.find(",",pos+1);
       outvariables.push_back(tempvariable);
       cout << tempvariable << ", ";
       tempvariable.clear();
       }
       //last variable
      else{
       for(int i=pos+1; i<tempstring.size();i++) tempvariable.push_back(tempstring[i]);
       outvariables.push_back(tempvariable);
       cout << tempvariable << endl;
       tempvariable.clear();
        break;
       }
     }
    } 

  }//not comment line end
 }//read-in end

 configfile.close();

//necessary parameters check
 //type
 if(type.size()==0){
  cout << "Error: type must be definied in the config file! Program will quit!" << endl;
  exit(EXIT_FAILURE);
 }
 //inputfile
 if(inputfile.size()==0){
  cout << "Error: inputfile must be definied in the config file! Program will quit!" << endl;
  exit(EXIT_FAILURE);
 }
 //outputfile
 if(outputfile.size()==0){
  cout << "Error: outputfile must be definied in the config file! Program will quit!" << endl;
  exit(EXIT_FAILURE);
 }


}//input file operations end

//Add .res file's path to inputfile
 inputfile=argv[2] + inputfile;


//-------------------Read-in data from .res file-------------
 MData data(variablenames.size(), atoi(polemax.c_str())-atoi(polemin.c_str())+1);

cout << endl << "Checking the inputfile..." << endl;
//checking .res input file existence
//doesn't exist
 if(!FileExists(inputfile.c_str())){
  cout << "Error: Input .res file doesn't exist, check your config file! Program will quit!" << endl;
  exit(EXIT_FAILURE);
 }

//input is OK
 else{
  cout << "Read data from inputfile..." << endl;

  vector<double> tempvariable;
  vector<Measurement> tempmeasurement;

  tempvariable.resize(data.GetVariableNumber());
  tempmeasurement.resize(data.GetMeasurementNumber());

 //set variable names
  for(int i=1; i<=variablenames.size();i++) data.SetVariableName(i,variablenames[i-1]);


  ifstream ifile(inputfile.c_str(), ifstream::in);

 //stream-in data from .res
   //variables
   for(int i=0; i<data.GetVariableNumber(); i++){
    ifile >> tempvariable[i];
   }
   //measurements
   for(int i=0; i<data.GetMeasurementNumber(); i++){
    ifile >> tempmeasurement[i].value;
    ifile >> tempmeasurement[i].error;
   }
//  int j=1;
 // while(!ifile.eof()){
  while(ifile.good()){
   data.AddLine(tempvariable, tempmeasurement);
   //variables
   for(int i=0; i<data.GetVariableNumber(); i++){
    ifile >> tempvariable[i];
   }
   //measurements
   for(int i=0; i<data.GetMeasurementNumber(); i++){
    ifile >> tempmeasurement[i].value;
    ifile >> tempmeasurement[i].error;
   }
//   cout << j << endl;
//   j++;
  }//stream end

  //data.PrintTable();
  
  ifile.close();
 }
//read-in data operations end



//----------Convert data into Mathematica format and write out into file------
 cout << endl << "Convert data into Mathematica format..." << endl;

 string errorfile = "err" + outputfile;

 fstream ofile(outputfile.c_str(),fstream::out | fstream::app);
 fstream errfile(errorfile.c_str(),fstream::out | fstream::app);
 
 stringstream datstream; 
 stringstream errstream;
//precision of digits
 datstream.precision(10);
 errstream.precision(10);

//I2C[1, x_,  0,  0, 0, 0] := SeriesData[e, 0, {}, -2, -2, 1]
//errI2C[1, x_,  0,  0, 0, 0] := SeriesData[e, 0, {}, -2, -2, 1]

 for(int i=1; i<=data.GetLineNumber(); i++){
 //type, serial
  datstream << "num" << type << "[" << serial; //value
  errstream << "err" << type << "[" << serial; //error

 //variables
  if(outvariables.size()){
   for(int j=0; j<outvariables.size();j++){
    datstream << fixed << "," << data.GetVariableValue(i, outvariables[j]);
    errstream << fixed << "," << data.GetVariableValue(i, outvariables[j]);
   }
  }

 //parameters
  if(parameters.size()){
   datstream << "," << parameters;
   errstream << "," << parameters;
  }

 //additional stuff
  datstream << "] := SeriesData[e, 0, {";
  errstream << "] := SeriesData[e, 0, {";

//NEW IMPROVEMENT: `100 precision has been added to numerical values
 //measurement values
  for(int j=1; j<=data.GetMeasurementNumber(); j++){
   if(j==data.GetMeasurementNumber()){
     datstream << fixed << data.GetMeasurementValue(i,j) << "`100";
     errstream << fixed << data.GetMeasurementError(i,j) << "`100";
   }
   else{
     datstream << fixed << data.GetMeasurementValue(i,j) << "`100" << ", ";
     errstream << fixed << data.GetMeasurementError(i,j) << "`100" << ", ";
   }
  }

 //other additional stuff and poles
  datstream << "}, " << polemin << ", " << atoi(polemax.c_str())+1 << ", 1];";
  errstream << "}, " << polemin << ", " << atoi(polemax.c_str())+1 << ", 1];";

  ofile << datstream.str() << endl;
  errfile << errstream.str() << endl;

  datstream.str("");
  errstream.str("");
  
 }

 ofile.close();
 errfile.close();
 
 cout << "Done... Data was written into " << outputfile << endl;
 cout << "Errors were written into " << errorfile << endl << endl;
//write out operation ends


return 0;
}
