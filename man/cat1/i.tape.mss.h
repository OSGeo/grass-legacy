


i.tape.mss.h <main>  GRASS Reference Manual   <main> i.tape.mss.h



NAME
     i.tape.mss.h - An imagery function	that extracts header
     information from LANDSAT Multispectral Scanner (MSS) imagery
     data stored on half-inch tape.
     (GRASS Image Processing Program)

SYNOPSIS
     i.tape.mss.h
     i.tape.mss.h help
     i.tape.mss.h tape_drive_name

DESCRIPTION
     i.tape.mss.h reads	the header information on a Multispectral
     Scanner (MSS) tape.  This program reads the specified input
     file (the computer-compatible tape	tape_drive_name), and by
     default displays the output to the	user's terminal.  The
     user may redirect output to a file	by using the UNIX
     redirection mechanism.  For example:

	  i.tape.mss.h /dev/rmt0 > h.out

     The name of the tape drive	depends	on the computer	being
     used.

     This program can be run either non-interactively or
     interactively.  The user can run the program by specifying
     program arguments on the command line.  Alternately, the
     user can simply type i.tape.mss.h on the command line,
     without program arguments.	 In this event,	the program will
     prompt the	user to	enter a	tape device name using the
     standard user interface described in the manual entry for
     parser.

SEE ALSO
     GRASS Tutorial: Image Processing

     i.tape.mss	and parser

AUTHOR
     Michael Shapiro, U.S. Army	Construction Engineering Research
     Laboratory














GRASS 4.2		Baylor University			1



