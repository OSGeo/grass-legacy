/* Function: write_PS_header
**
** Author: Paul W. Carlson	3/92
*/

#include <stdio.h>
#include <unistd.h>
#include "ps_info.h"

static long bb_offset;

write_PS_header()
{
    /* write PostScript header */
    /*fprintf(PS.fp, "%%!PS-Adobe-2.0 EPSF-1.2\n");*/
    fprintf(PS.fp, "%%!PS-Adobe-3.0 EPSF-3.0\n");
    bb_offset = ftell(PS.fp);
    fprintf(PS.fp, "                                       ");
    fprintf(PS.fp, "                                       \n");
    fprintf(PS.fp, "%%%%Title: ");
    if (PS.do_raster) fprintf(PS.fp, "Map layer = %s  ", PS.cell_name);
    fprintf(PS.fp, "Mapset = %s\n", PS.cell_mapset);
    fprintf(PS.fp, "%%%%EndComments\n");
}


write_bounding_box()
{
    int llx, lly, urx, ury;

    llx = (int)(72.0 * PS.left_marg);
    lly = (int)(PS.min_y - 0.5);
    urx = (int)(72.0 * (PS.page_width  - PS.right_marg));
    ury = (int)(72.0 * (PS.page_height - PS.top_marg));
    fseek(PS.fp, bb_offset, SEEK_SET);
    fprintf(PS.fp, "%%%%BoundingBox: %d %d %d %d", llx, lly, urx, ury);
}

