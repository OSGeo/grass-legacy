                    RASTER/VECTOR/SITES CONVERSIONS

The commands listed below are used to convert data among GRASS raster,
vector, and sites data file formats.  Such conversion is needed when the
user wishes to run a GRASS program on data that are not in the format
expected by the program.

For more information on a specific command, select a command and press
ESC.

       Raster               Vector                 Sites
       Conversion           Conversion             Conversion
       Programs:            Programs:              Programs:
       ========================================================
       \-  r.line \         \-  v.to.rast \       \-  s.in.ascii \
       \-  r.poly \         \-  v.to.sites \      \-  s.out.ascii \
       \-  r.thin \
       \-  r.in.sunrast \

GRASS programs with the prefix "r." work on data in raster format.
GRASS programs with the prefix "v." work on data in vector format.
GRASS programs with the prefix "s." work on data in site_lists format.

Typically, linear and area edge (polygon) data are entered in vector
format, and converted to raster format for analysis.  Often, point data
are entered in site_lists format and analyzed either in this format or
after conversion to raster format.  Data can be displayed and printed
in any of these three file formats.

For more information on these GRASS data file formats, select the
section on "Raster, Vector, and Sites Data File Formats," located under
the "Digitizing and Map Development" menu (access from the Help System
Main Menu).








