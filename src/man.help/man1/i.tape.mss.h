.\" %w% %G%
.TH i.tape.mss.h 1
.nh
.SH NAME
.IP i.tape.mss.h\ \- 15
An imagery function that extracts header information from Landsat 
Multispectral Scanner imagery data stored on half inch tape
.SH SYNOPSIS
\fBi.tape.mss.h\fR \fItape_drive_name\fR
.SH DESCRIPTION
.I i.tape.mss.h
reads the header information on a Multispectral Scanner
tape.  This program reads the specified input file (the computer-compatible 
tape), and by default displays the output to the screen.  The user may
redirect output to a file by using the UNIX redirection mechanism.
For example:
.IP
\fIi.tape.mss.h /dev/rmt0\fR > \fIh.out\fR
.LP
The name of the tape drive depends on the computer being used.
.SH NOTE
This program is not interactive. The tape_drive_name must be suplied on
the command line.
.SH SEE ALSO
\fIGRASS Tutorial: Image Processing\fR
.br
\fIi.tape.mss[1]\fR
.SH AUTHOR
Michael Shapiro, U.S. Army Construction Engineering Research Laboratory
