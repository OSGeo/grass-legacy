.TH i.tape.mss.h
.nh
.SH NAME
\fIi.tape.mss.h\fR \-
An imagery function that extracts header information from LANDSAT
Multispectral Scanner (MSS) imagery data stored on half-inch tape.
.br
\fI(GRASS Image Processing Program)\fR
.SH SYNOPSIS
\fBi.tape.mss.h\fR
.br
\fBi.tape.mss.h help\fR
.br
\fBi.tape.mss.h\fR \fItape_drive_name\fR
.SH DESCRIPTION
.I i.tape.mss.h
reads the header information on a Multispectral Scanner (MSS)
tape.  This program reads the specified input file
(the computer-compatible tape \fItape_drive_name\fR),
and by default displays the output to the user's terminal.
The user may redirect output to a file by using the UNIX redirection
mechanism.  For example:
.IP
\fBi.tape.mss.h /dev/rmt0\fR > \fIh.out\fR
.LP
The name of the tape drive depends on the computer being used.

This program can be run either non-interactively or interactively.
The user can run the program by specifying program arguments
on the command line.
Alternately, the user can simply type \fBi.tape.mss.h\fR on the
command line, without program arguments.  In this event, the program
will prompt the user to enter a tape device name using the standard
user interface described in the manual entry for \fIparser\fR.
.SH SEE ALSO
\fBGRASS Tutorial: Image Processing\fR
.LP
\fIi.tape.mss\fR
and
\fIparser\fR
.SH AUTHOR
Michael Shapiro, U.S. Army Construction Engineering Research Laboratory
