


r.out.arc <main>      GRASS Reference Manual	 <main> r.out.arc



NAME
     r.out.arc	- Converts a raster map layer into an ESRI
     ARCGRID file.
     (GRASS Raster Data Export Program)

SYNOPSIS
     r.out.arc
     r.out.arc help
     r.out.arc [-h]  [-1] map=name [dp=value]

DESCRIPTION
     r.out.arc converts a user-specified raster map layer
     (map=name) into an ESRI ARCGRID ascii file suitable for
     export to other computer systems.	The dg=value is a number
     of the user's choice) can be used to request that numbers
     after decimal points are limited.	 However, to use this,
     the user should know the maximum number of digits that will
     occur in the output file.	The user can find the maximum
     number of digits occurring in the output file by running
     r.out.arc without the dg=value option.

     The GRASS program r.in.arc can be used to perform the
     reverse function, converting an ESRI ARCGRID ascii file in
     suitable format to GRASS raster file format. The order of
     cell values in file is from lower left to upper right
     (reverse to GRASS).

OPTIONS
Flags:
     Suppress printing of header information.  List one entry per
     line.

Parameters:
     Name of an existing raster map layer.  The minimum number of
     decimals (per cell) to be printed.	 r.out.arc can be run
     either non-interactively or interactively.	 The program will
     be run non-interactively if the user specifies the name of a
     raster map layer and (optionally) a value for dg, using the
     form r.out.arc map=name ] where name is the name of a raster
     map layer to be converted to ARCGRID format, and value is
     the minimum number of digits (per cell) to be printed to
     output.  The user can also the -h option to suppress the
     output of file header information.

     Alternately, the user can simply type r.out.arc on the
     command line, without program arguments.  In this case, the
     user will be prompted for parameter values using the
     standard GRASS parser interface.

NOTES
     The output from r.out.arc may be placed into a file by using
     the UNIX redirection mechanism;  e.g.: r.out.arc map=soils >



GRASS 5.0beta5	      GRASS Development Team			1






r.out.arc <main>      GRASS Reference Manual	 <main> r.out.arc



     out.grd The output file out.grd can then be copied onto a
     magnetic tape or floppy disk for export purposes.

SEE ALSO
     r.in.arc
     parser

AUTHOR
     Markus Neteler, University of Hannover, Germany, based on
     r.out.ascii from

     Michael Shapiro, U.S.Army Construction Engineering Research
     Laboratory










































2		      GRASS Development Team	   GRASS 5.0beta5



