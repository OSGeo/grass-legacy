.TH r.cats
.SH NAME
\fIr.cats\fR \- Prints category values and labels associated with user-specified
raster map layers.
.br
\fI(GRASS Raster Program)\fR
.SH SYNOPSIS
\fBr.cats\fR
.br
\fBr.cats help\fR
.br
\fBr.cats map\*=\fIname\fR [\fBcats\*=\fIrange\fR[,\fIrange\fR,...]] [\fBfs\*=\fIcharacter\||\|space\||\|tab\fR]
.SH DESCRIPTION
\fIr.cats\fR prints the category values and labels for the raster map layer
specified by map\*=\fIname\fR to standard output.
.LP
The user can specify all needed parameters on the command line,
and run the program non-interactively.
If the user does not specify any categories
(e.g., using the optional \fBcats\*=\fIrange\fR[\fI,range,...\fR] argument),
then all the category values and labels for the named raster map layer
that occur in the map are printed.
The entire \fImap\fR is read, using \fIr.describe\fR, to determine which
categories occur in the \fImap\fR.
If a listing of categories is specified,
then the labels for those categories only are printed.
The \fIcats\fR may be specified as single category values,
or as ranges of values.
The user may also (optionally) specify that a field separator other
than a space or tab be used to separate the category value from its
corresponding category label in the output,
by using the \fBfs\*=\fIcharacter\||\|space\||\|tab\fR option
(see example below).
If no field separator is specified by the user,
a tab is used to separate these fields in the output, by default.
.LP
The output is sent to standard output in the form of one category
per line, with the category value first on the line,
then an ASCII TAB character (or whatever single character or space is
specified using the \fIfs\fR parameter), then the label for the category.
.LP
If the user simply types \fBr.cats\fR without arguments on the command line
the program prompts the user for parameter values
using the standard GRASS parser
interface described in the manual entry for \fIparser\fR.
.SH EXAMPLES
\fBr.cats map\*=soils\fR
.LP
.RS
prints the values and labels associated with all of the categories
in the \fIsoils\fR raster map layer;
.RE

\fBr.cats map\*=soils cats\*=10,12,15-20\fR
.LP
.RS
prints only the category values and labels for
\fIsoils\fR map layer categories 10, 12, and 15 through 20;  and
.RE

\fBr.cats map\*=soils cats\*=10,20 fs\*= :\fR
.LP
.RS
prints the values and labels for \fIsoils\fR map layer categories 10 and 20,
but uses ":" (instead of a tab) as the character separating the category
values from the category values in the output.
.RE

Example output:
.LP
.RS
10:Dumps, mine, Cc
.br
20:Kyle clay, KaA
.RE
.SH NOTES
Any ASCII TAB characters which may be in the label are replaced by spaces.
.LP
The output from \fIr.cats\fR can be redirected into a file, or piped into
another program.
.SH SEE ALSO
UNIX Manual entries for \fIawk\fR and \fIsort\fR
.LP
.I r.coin,
.I r.describe,
.I r.rast.what,
.I r.support,
and
.I parser
.SH AUTHOR
Michael Shapiro, U.S. Army Construction Engineering Research Laboratory
