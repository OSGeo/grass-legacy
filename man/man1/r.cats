.TH r.cats 1 "" "" "" ""
.SH NAME
\*Lr.cats\*O - Prints 
category values and labels associated with user-specified raster map layers. 
.br
(GRASS Raster Program) 
.SH SYNOPSIS
\*Lr.cats\*O 
.br
\*Lr.cats help\*O 
.br
\*Lr.cats\*O 
\*Lmap=\*Oname]
.SH DESCRIPTION
r.cats to standard output.
.PP
The user can specify all needed parameters on the command line, and run the program non-interactively. If the user does not specify any categories (e.g., using the optional \*Lcats=\*Orange,...] argument), then all the category values and labels for the named raster map layer that occur in the map are printed. 
The entire map, 
to determine which categories occur in the map option (see example below). If no field separator is specified by the user, a tab is used to separate these fields in the output, by default. 
.PP
The output is sent to standard output in the form of one category per line, with the category value first on the line, then an ASCII TAB character (or whatever single
character or space is specified using the \*Lfs\*O parameter), then the label for the category.
.PP
If the user simply types \*Lr.cats\*O without arguments on the command line the program prompts the user for parameter values using the standard GRASS \*Lparser\*O interface.
.SH Parameter:
.VL 4m
.LI "\*Lvals=\*Ovalue
Comma separated value list: e.g. 1.4, 3.8, 13
.LE
.SH EXAMPLES
.PP
.VL 4m
.LI " \*Lr.cats map=\*Osoils 
prints the values and labels associated with all of the categories in the soils raster map layer; 
.LI " \*Lr.cats map=\*Osoils 
prints only the category values and labels for soils map layer categories 10, 12, and 15 through 20; and 
.LI " \*Lr.cats\*O map=\*Osoils cats=\*O10,20 fs=\*O :\*O 
prints the values and labels for soils map layer categories 10 and 20, but uses ":" (instead of a tab) as the character separating the category values from the category values in the output.
.LE
Example output: 
.PP
.VL 4m
10:Dumps, mine, Cc 
.br
20:Kyle clay, KaA 
.LE
.SH NOTES
Any ASCII TAB characters which may be in the label are replaced by spaces. 
.PP
The output from r.cats can be redirected into a file, or piped into another program.
.SH SEE ALSO
UNIX Manual entries for 
\*Wawk\*O and 
\*Wsort\*O
.PP
\*Lr.coin\*O
\*Lr.describe\*O
\*Ld.what.rast\*O
\*Lr.support\*O
\*Lparser\*O
.SH AUTHOR
Michael Shapiro, U.S. Army Construction Engineering 
Research Laboratory
