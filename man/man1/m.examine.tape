.TH m.examine.tape
.nh
.SH NAME
\fIm.examine.tape\fR \- Provides a description of the files on a 1/2-inch magnetic tape.
.br
.I (GRASS File Management Tool)
.SH SYNOPSIS
.B m.examine.tape
.SH DESCRIPTION
.I m.examine.tape
is a convenient tool for previewing a tape and obtaining block size values.
After the user enters the command
.LP
.RS
\fBm.examine.tape\fR
.RE
.LP
the program will prompt the user for the full path name of the tape device
on which the tape is mounted.  In most cases, this will be /dev/rmt0.
However, the user must enter this, as there is no default value
for the tape device name.
The user is then prompted to enter the buffer size to be used
when reading the tape.
The buffer size is the amount of memory allocated
to read one physical record of the tape.
If the user hits RETURN, the program will assume a default buffer size
of 32767 bytes.
.LP
.I m.examine.tape
reads the tape specified by the user and reports back the block size
(record length) and the number of blocks for each file contained on the tape.
\fIm.examine.tape\fR can be used on any 1/2-inch magnetic tape.
The user has the option of sending the output into a file or viewing the output
on the terminal screen.
.SH NOTES
The buffer size is the amount of memory allocated to read one physical
record on the tape.  Magnetic tape devices will accept a request to read
more bytes than actually exist in any given record on a tape.
However, \fIm.examine.tape\fR reads only as many bytes as physically exist
on the tape and returns the number of bytes actually read.
The user is allowed to alter the buffer size in order to request smaller reads
for tape devices unable to handle requests this large (32767 bytes),
or to request larger reads for tapes with larger record sizes read on drives
able to handle larger record sizes.
.LP
Note that \fIm.examine.tape\fR cannot be used to examine the content of
either ordinary files or 1/4-inch tape cartridges.
.SH SEE ALSO
.I m.dem.examine,
.I m.dem.extract,
.I m.dmaUSGSread,
.I m.dted.examine,
.I m.dted.extract,
.I m.lulc.USGS,
.I m.lulc.read
.SH "AUTHOR"
Michael Shapiro, U.S. Army Construction Engineering Research Laboratory
