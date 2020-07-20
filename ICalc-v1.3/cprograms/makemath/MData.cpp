/*-------------------MData-----------------------
Table style container for storing data with multiple
variables and measurement values and erros with
additional column names.
Written by Zoltán Szőr in 2013
Definition file
------------------------------------------------*/

#include <iostream>
#include <vector>
#include <string>
#include <cstdlib>
#include "MData.h"

using namespace std;

//--------------Constructor----------
MData::MData(const int variablenum, const int measurementnum) : 
 variablenumber(variablenum), measurementnumber(measurementnum) {
  variablenames.resize(variablenum);
  measurementnames.resize(measurementnum);
} 

//------------SetVariableName------------
void MData::SetVariableName(const int &n, const string &name){
 if((n>variablenumber) || (n==0)){
  cout << "Error: Set variable name: incorrect number! Program will quit!" << endl;
  exit(EXIT_FAILURE);
 }
 else{
  variablenames[n-1]=name;
 }
}//end

//---------SetMeasurementName-------------
void MData::SetMeasurementName(const int &n, const string &name){
 if((n>measurementnumber) || (n==0)){
  cout << "Error: Set measurement name: incorrect number! Program will quit!" << endl;
  exit(EXIT_FAILURE);
 }
 else{
  measurementnames[n-1]=name;
 }
}//end

//-------------GetVariableNumber------------
int MData::GetVariableNumber(){
 return variablenumber;
}//end

//--------------GetMeasurementNumber-------------
int MData::GetMeasurementNumber(){
 return measurementnumber;
}//end

//---------------GetVariableValue(number)-------------
double MData::GetVariableValue(const int &line, const int &n){
 if((line>table.size()) || (n>variablenumber) || (line==0) || (n==0)){
  cout << "Error: Get variable's value: incorrect line or/and variable number! Program will quit!" << endl;
  exit(EXIT_FAILURE);
 }
 else{
  return table[line-1].variables[n-1];
 }
}//end

//----------------GetVariableValue(name)------------
double MData::GetVariableValue(const int &line, const string &name){
 int n=-1;
 for(int i=0; i<variablenumber; i++){
  if(variablenames[i]==name) n=i;}

 if(line>table.size() || (line==0) || (n==-1)){
  cout << "Error: Get variable's value: incorrect line number or/and variable name haven't found! Program will quit!" << endl;
  exit(EXIT_FAILURE);
 }
 else{
  return table[line-1].variables[n];
 }
}//end

//-----------------GetMeasurementValue(number)-----------
double MData::GetMeasurementValue(const int &line, const int &n){ 
 if((line>table.size()) || (n>measurementnumber) || (line==0) || (n==0)){
  cout << "Error: Get measurement's value: incorrect line or/and measurement number! Program will quit!" << endl;
  exit(EXIT_FAILURE);
 }
 else{
  return table[line-1].measurements[n-1].value;
 }
}//end

//----------------GetMeasurementValue(name)-----------
double MData::GetMeasurementValue(const int &line, const string &name){
 int n=-1;
 for(int i=0; i<measurementnumber; i++){
  if(measurementnames[i]==name) n=i;}

 if(line>table.size() || (line==0) || (n==-1)){
  cout << "Error: Get measurement's value: incorrect line number or/and measurement name haven't found! Program will quit!" << endl;
  exit(EXIT_FAILURE);
 }
 else{
  return table[line-1].measurements[n].value;
 }
}//end

//----------------GetMeasurementError(number)--------
double MData::GetMeasurementError(const int &line, const int &n){
 if((line>table.size()) || (n>measurementnumber) || (line==0) || (n==0)){
  cout << "Error: Get measurement's error: incorrect line or/and measurement number! Program will quit!" << endl;
  exit(EXIT_FAILURE);
 }
 else{
  return table[line-1].measurements[n-1].error;
 }
}//end

//-----------------GetMeasurementError(name)----------
double MData::GetMeasurementError(const int &line, const string &name){
 int n=-1;
 for(int i=0; i<measurementnumber; i++){
  if(measurementnames[i]==name) n=i;}

 if(line>table.size() || (line==0) || (n==-1)){
  cout << "Error: Get measurement's error: incorrect line number or/and measurement name haven't found! Program will quit!" << endl;
  exit(EXIT_FAILURE);
 }
 else{
  return table[line-1].measurements[n].error;
 }
}//end

//-----------------AddLine---------------
void MData::AddLine(const vector<double> &variables, const vector<Measurement> &measurements){
 if(variables.size()==variablenumber && measurements.size()==measurementnumber){
  Line templine;
  templine.variables=variables;
  templine.measurements=measurements;
  table.push_back(templine);
 }
 else{
  cout << "Error: AddLine: Invalid size of variable or/and measurement input! Program will quit!" << endl;
  exit(EXIT_FAILURE);
 }
}//end

//-------------GetLineNumber-------------
int MData::GetLineNumber(){
 return table.size();
}//end

//-------------PrintLine-----------------
void MData::PrintLine(const int &n){
 if(n>table.size() || n==0){
  cout << "Error: PrintLine: Incorrect line number! Program will quit!" << endl;
  exit(EXIT_FAILURE);
 }
 else{
  cout << "Line(" << n << "): ";
  for(int i=0; i<variablenumber; i++) cout << table[n-1].variables[i] << " | ";
  for(int i=0; i<measurementnumber; i++) cout << table[n-1].measurements[i].value << "  " << table[n-1].measurements[i].error << " | ";
  cout << endl;
 }
}//end


//--------------PrintTable---------------
void MData::PrintTable(){
 cout << "Line(0): ";
 for(int i=0; i<variablenumber; i++) cout << variablenames[i] << " | ";
 for(int i=0; i<measurementnumber; i++) cout << measurementnames[i] << " | ";
 cout << endl;
 for(int i=1; i<=table.size(); i++) PrintLine(i);
}//end

