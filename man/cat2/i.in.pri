


i.in.pri <alpha>      GRASS Reference Manual	 <alpha> i.in.pri



NAME
     i.in.pri  - ERS1.SAR.PRI file to GRASS conversion.


     (GRASS Imagery Program)


SYNOPSIS
     i.in.pri


DESCRIPTION
     i.in.pri on the command line.  The program will then prompt
     the user for parameter values.


Parameters:
     Path location and name of file to import.

     raw band cell file



EXAMPLE
     OPTION:   Data information only (y/n) [n]
	  key: infonly
      default: n required: NO
      options: y,n,

     OPTION:   Volume directory filename
	  key: voldirname required: YES

     OPTION:   Leader filename
	  key: leadername required: YES

     OPTION:   Data filename
	  key: dataname required: YES

     OPTION:   Name for resultant raster map
	  key: output required: YES



NOTES
     This program was derived from i.in.gtc.


AUTHOR
     Olaf Hellwich, TUM, February 1994 Technische Universitaet
     Muenchen, Germany





GRASS 5.0beta8	      GRASS Development Team			1






i.in.pri <alpha>      GRASS Reference Manual	 <alpha> i.in.pri



NOTICE
     This program is part of the alpha section of the GRASS
     distribution.  Unlike the code in the main section of GRASS,
     the alpha code has not yet been fully tested for one release
     cycle.


















































2		      GRASS Development Team	   GRASS 5.0beta8



