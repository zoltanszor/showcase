----------makemath README----------
Program written by Zoltán Szőr.

TransDat is a data conversion program to convert table style data
into Mathematica format automatically. Bug reports and notes are welcome
to siriuszoli@gmail.com.

Usage:

For compiling use the "compile" script, then please take a look at the "example.cfg" file.
The program uses a config file (extension is arbitrary) to get the initial options
and information. The possible options are the follows:

-type: type of the integral (like I2C, I2S etc.) - must be definied
-serial: serial of the integral
-inputfile: name of the .res inputfile - must be definied
-outputfile: name of the .m outputfile - must be definied
-parameters: list of additional parameters
-poles: minimum and the maximum pole order of the series
-variables: list of variables in order as they appear in the .res file.
-outvar: list of variables in order as they will appear in the Mathematica output file

Lines containing "#" (practical at the beginning of the line) character are comment
lines and won't be considered during the reading process. If some the options mentioned above are
unnecessary during the conversion then comment them out. Otherwise there could be some problems
with the output format.
If your outputfile already exists the new data would be written at the end of the file, it makes
easy to collect all of the integrals into one file.
Running the program: "./RunTD configfile"
