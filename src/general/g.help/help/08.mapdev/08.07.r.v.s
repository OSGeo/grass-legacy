               RASTER, VECTOR, AND SITES DATA FILE FORMATS

GRASS commands operate on three basic forms of data:

  \-  raster data \
  \-  vector data \
  \-  sites data \


GRASS command naming conventions reflect this distinction:

      - GRASS commands with the prefix "r." typically operate on data
        stored in GRASS raster format.

      - GRASS commands with the prefix "v." typically operate on data
        stored in GRASS vector format.

      - GRASS commands with the prefix "s." typically operate on data
        stored in GRASS sites format.

If you need to convert data into a specific file format, GRASS contains
programs that will convert data to and from GRASS raster, vector, and
sites file formats. For example, "v.to.rast" converts vector data to
raster data, and "v.to.sites" converts vector data to sites data.  The
following GRASS programs are used to convert data among GRASS raster,
vector, and sites formats:

      \-  r.line \            \-  s.in.ascii \      \-  v.to.rast \
      \-  r.poly \            \-  s.out.ascii \     \-  v.to.sites \
      \-  r.thin \                                  \-  v.in.ascii \
      \-  r.in.ascii \                              \-  v.out.ascii \
      \-  r.out.ascii \
      \-  r.in.sunrast \
      \-  r.in.ll \

For information on the individual commands, select a command and press
ESC.

Other GRASS programs exist to convert data files in GRASS raster, vector,
or sites formats to other systems' data file formats, and to bring data
files in other systems' formats into GRASS' data formats.  For example,
\v.in.arc\ can be used to import ARC/INFO data into GRASS vector format,
and \v.out.arc\ can be used to export GRASS vector data to ARC/INFO's
data file format.

Users interested in reading further about data conversion can select the
topic area, Data Conversion: Importing and Exporting Data, located at
the Help System Main Menu.








