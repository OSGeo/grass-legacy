#include "gis.h"
#include "parms.h"

int 
getcats (void)
{
    if (G_read_cats (parms.cellname, parms.cellmapset, &parms.pcats) < 0)
	parms.with_colortable = 0;
}
