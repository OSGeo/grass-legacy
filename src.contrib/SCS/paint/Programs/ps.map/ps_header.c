/* Function: write_PS_header
**
** Author: Paul W. Carlson	3/92
*/

#include "ps_info.h"

write_PS_header()
{
    /* write PostScript header */
    fprintf(PS.fp, "%%!PS-Adobe-2.0 EPSF-1.2\n");
    fprintf(PS.fp, "%%%%Title: ");
    if (PS.do_raster) fprintf(PS.fp, "Map layer = %s  ", PS.cell_name);
    fprintf(PS.fp, "Mapset = %s\n", PS.cell_mapset);
    fprintf(PS.fp, "%%%%EndComments\n");
}
