                    RASTER/VECTOR/SITES CONVERSIONS

The commands listed below are used to convert data among GRASS raster,
vector, and sites data file formats.  Such conversion is needed when the
user wishes to run a GRASS program on data that are not in the format
expected by the program.

       Raster               Vector                 Sites
       Conversion           Conversion             Conversion
       Programs:            Programs:              Programs:
       ========================================================
       @ref("-  r.line ",Commands.def/rline.def)         @ref("-  v.to.rast ",Commands.def/v2rast.def)       @ref("-  s.in.ascii ",Commands.def/sinascii.def)
       @ref("-  r.poly ",Commands.def/rpoly.def)         @ref("-  v.to.sites ",Commands.def/v2sites.def)      @ref("-  s.out.ascii ",Commands.def/soutascii.def)
       @ref("-  r.thin ",Commands.def/rthin.def)
       @ref("-  r.in.sunrast ",17.manual/Help.pages/r.in.sunrast)

GRASS programs with the prefix "r." work on data in raster format.
GRASS programs with the prefix "v." work on data in vector format.
GRASS programs with the prefix "s." work on data in site_lists format.

Typically, linear and area edge (polygon) data are entered in vector
format, and converted to raster format for analysis.  Often, point data
are entered in site_lists format and analyzed either in this format or
after conversion to raster format.  Data can be displayed and printed
in any of these three file formats.
