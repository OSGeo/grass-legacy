.TH m.in.stf1.tape
.SH NAME
\fIm.in.stf1.tape\fR \- Filter to extract lines from a text file based on
column contents, especially for Bureau of the Census STF1 files.
.br
.I "(GRASS Support Program)"
.SH SYNOPSIS
\fBm.in.stf1.tape\fR
.br
\fBm.in.stf1.tape help\fR
.br
\fBm.in.stf1.tape -f\fR
.br
\fBm.in.stf1.tape [-n] sc=str [sc=str . . .] < infile > outfile\fR

This program must be run in command mode only.

.SH PARAMETERS

\fIsc\fR is a starting column number of a desired field in the input file,
or is the name of one of the Identification Section field names for the 
STF1 records (all upper case letters), and \fIstr\fR is a string to match 
against input lines starting at column \fIsc\fR. \fIsc=str\fR may be 
repeated resulting in a conjunction (anding) of the results of each 
\fIsc=str\fR expression.  A '?' may be used as a single character wild card 
in \fIstr\fR; if \fIsc=str\fR contains '?' or other shell interpreted 
characters, it should be protected in quotes.  Preceeding \fIsc\fR by 'N', 
or preceeding the \fI=\fR by '!' reverses the sense of the test ("not equals").

Examples:
.br
m.in.stf1.tape 11=050  < infile > outfile
.br
m.in.stf1.tape SUMLEV=050  < infile > outfile
.br
m.in.stf1.tape 1=T450 '7=Bu??s' < infile > outfile
.br
m.in.stf1.tape 51=tract N37=9753 < infile > outfile
.br
m.in.stf1.tape 51=tract 37!=9753 < infile > outfile
.br
  
Running the program with the \fI-f\fR flag generates the list of STF1
Identification Section field names to stdout.

.SH DESCRIPTION

This is a text filter program written in C especially to work
with the Census STF1 files, but useful for selecting subsets of lines
from any text file.  It will work with arbitrarily long input lines,
up to 10,000 characters.  Input lines may be of variable length.

A \fIsc=str\fR condition which refers to columns beyond the end of
the input line is assumed to be true.

Multiple tests are 'anded' into a single test; that is, lines which
pass all tests are written to the output file.

Null characters are always filtered out; thus, this
program can be used as a null filter by specifying a \fIsc=str\fR condition
which will always be true (e.g., '1=?').  In the same way, files with lines
terminated with <LF><CR> or just <CR> can be "fixed" to have the
standard Unix <LF> terminator.

NOTE: One special property of \fIm.in.stf1.tape\fR is that it will pad
(with 0) its output lines to 4806 characters if the line begins with
the characters "STF1" and is greater than 4000 characters long.
Experience has shown that some STF1 records
are a few characters short (but no data has been omitted), and this
corrects them so that other programs will be able to read full lines.

.SH BUGS/FEATURES
Input lines must be terminated with <LF>, <CR> or <LF><CR>.
Output lines will be terminated with <LF> only.

Input characters lexically less than 'space' (32 decimal; the "control"
characters) which are not line terminators will be perceived as line
terminators and thus cause improper functioning.

.SH "SEE ALSO"
For extracting lines from files, Unix programs such as \fIgrep\fR,
or \fIawk\fR are somewhat more flexible than \fIm.in.stf1.tape\fR,
but have line length limitations and do not adapt to lines terminated
with <LF> and <CR>.

This program was created as a preprocessor for Census STF1 files,
to produce input files for the program \fIv.apply.census\fR.  The
program \fIm.in.stf1.db3\fR performs the same preprocessing function for the
Census STF1 files distributed in "dBase 3" format on CD-ROM.

.SH AUTHOR
Dr. James Hinthorne, GIS Laboratory, Central Washington University.

