               RASTER, VECTOR, AND SITES DATA FILE FORMATS

GRASS commands operate on three basic forms of data:

  @ref("-  raster data ",02.db.setup/02.03.1.fmt.r)
  @ref("-  vector data ",02.db.setup/02.03.2.fmt.v)
  

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

      @ref("-  r.line ",Commands.def/rline.def)            @ref("-  s.in.ascii ",Commands.def/sinascii.def)      @ref("-  v.to.rast ",Commands.def/v2rast.def)
      @ref("-  r.poly ",Commands.def/rpoly.def)            @ref("-  s.out.ascii ",Commands.def/soutascii.def)     @ref("-  v.to.sites ",Commands.def/v2sites.def)
      @ref("-  r.thin ",Commands.def/rthin.def)                                  @ref("-  v.in.ascii ",Commands.def/vinasc.def)
      @ref("-  r.in.ascii ",Commands.def/rinasc.def)                              @ref("-  v.out.ascii ",Commands.def/voutasc.def)
      @ref("-  r.out.ascii ",Commands.def/routasc.def)
      @ref("-  r.in.sunrast ",17.manual/Help.pages/r.in.sunrast)
      @ref("-  r.in.ll ",Commands.def/rinll.def)

Other GRASS programs exist to convert data files in GRASS raster, vector,
or sites formats to other systems' data file formats, and to bring data
files in other systems' formats into GRASS' data formats.  For example,
@man("v.in.arc") can be used to import ARC/INFO data into GRASS vector format,
and @man("v.out.arc") can be used to export GRASS vector data to ARC/INFO's
data file format.
