


i.tape.mss.h(1)      GRASS Reference Manual       i.tape.mss.h(1)



NAME
     i.tape.mss.h - An imagery function that extracts header
                    information from Landsat Multispectral
                    Scanner imagery data stored on half inch tape

SYNOPSIS
     i.tape.mss.h tape_drive_name

DESCRIPTION
     i.tape.mss.h reads the header information on a Multispectral
     Scanner tape.  This program reads the specified input file
     (the computer-compatible tape), and by default displays the
     output to the screen.  The user may redirect output to a
     file by using the UNIX redirection mechanism.  For example:

          i.tape.mss.h /dev/rmt0 > h.out

     The name of the tape drive depends on the computer being
     used.

NOTE
     This program is not interactive. The tape_drive_name must be
     suplied on the command line.

SEE ALSO
     GRASS Tutorial: Image Processing
     i.tape.mss[1]

AUTHOR
     Michael Shapiro, U.S. Army Construction Engineering Research
     Laboratory
























GRASS 3.2                U.S. Army CERL                         1



