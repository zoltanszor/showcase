#ifndef MDATA_H
#define MDATA_H

/*-------------------MData-----------------------
Table style container for storing data with multiple
variables and measurement values and errors with
additional column names.
Written by Zoltán Szőr in 2013
Header file
------------------------------------------------*/

#include <iostream>
#include <vector>
#include <string>

using namespace std;

//structures
struct Measurement{
 double value;
 double error;
};

struct Line{
 vector<double> variables;
 vector<Measurement> measurements;
};

class MData{

 private:

  vector<Line> table;
  vector<string> variablenames;
  vector<string> measurementnames;
  int variablenumber;
  int measurementnumber; 

 public:

  MData(const int variablenum=1, const int measurementnum=1); 

  void SetVariableName(const int &n, const string &name);
  void SetMeasurementName(const int &n, const string &name);

  int GetVariableNumber();
  int GetMeasurementNumber();

  double GetVariableValue(const int &line, const int &n);
  double GetVariableValue(const int &line, const string &name);
  double GetMeasurementValue(const int &line, const int &n); 
  double GetMeasurementValue(const int &line, const string &name);
  double GetMeasurementError(const int &line, const int &n);
  double GetMeasurementError(const int &line, const string &name);

  void AddLine(const vector<double> &variables, const vector<Measurement> &measurements);
  int GetLineNumber();

  void PrintLine(const int &n);
  void PrintTable();

};//class end

#endif
