.\" %W% %G%
.TH examine.tape 1
.nh
.SH NAME
examine.tape \- Provides a description of the files on a 1/2 
inch magnetic tape
.br
.I (Available through
.B grass3
.I menu)
.SH SYNOPSIS
.B "examine.tape"  
.SH DESCRIPTION
.I examine.tape
is a convenient tool for previewing a tape and obtaining block size values.
.I examine.tape  
will prompt the user for the tape device path.  There are no default values for
this entry;  in most cases, it will be /dev/rmt0.
.I examine.tape  
reads the tape specified by the user and reports back the block size and 
the number of blocks for each file contained on the tape.
\fIexamine.tape\fR can be used on any 1/2 inch
magnetic tape.
The user has the option of storing the output into a file or viewing the output
on the screen.
.SH NOTES
Note that
.I "examine.tape"
cannot be used for ordinary files or 1/4 inch cartridges.
.SH "AUTHOR"
Michael Shapiro, U.S. Army Construction Engineering Research Laboratory
