


r.in.arc <main>	      GRASS Reference Manual	  <main> r.in.arc




     NAME r.in.arc - Convert an ESRI ARC/INFO ascii raster file
     (GRID) into a (binary) raster map layer.

     (GRASS Raster Data Import Program)


     SYNOPSIS r.in.arc

     r.in.arc help

     r.in.arc input=name output=name [title="phrase"]
     [mult=multiplier]


     DESCRIPTION r.in.arc allows a user to create a (binary)
     GRASS raster map layer from an ESRI ARC/INFO ascii GRID file
     with (optional) title.


     OPTIONS


     Parameters: Name of an existing ASCII raster file to be
     imported.	Name to be assigned to resultant binary raster
     map layer.	 Title to be assigned to resultant raster map
     layer.  Multiply all raster cell values by multiplier.
     multiplier is a floating point value, and has a default
     value of 1.0.  The input file has a header section which
     describes the location and size of the data, followed by the
     data itself.  The header has 6 lines:

     nrows: xllcorner: yllcorner: cellsize: or alternatively (not
     supported in r.in.arc):

     nrows: xllcenter: yllcenter: cellsize:



     NOTES <p>r.in.arc handles floating point cell values. The
     mult option allows the number of significant figures of a
     floating point cell to be increased before importing.
     Multiples of ten are the most functional multipliers.


     SEE ALSO r.out.arc


     AUTHOR Unknown German author, updated by Bill Brown to
     floating point support.





GRASS 5.0beta5	      GRASS Development Team			1



