#include "gis.h"
#include "parms.h"

cellfile (name,mapset)
    char *name;
    char *mapset;
{
    char fullname[100];
    int n,num;

    parms.with_colortable = 0;
    if (parms.cellfd >= 0)
    {
	G_close_cell (parms.cellfd);
	free (parms.cellname);
	free (parms.cellmapset);
	free (parms.pattern);
	G_free_colors (&parms.pcolr);
	parms.cellfd = -1;
    }

    sprintf (fullname, "%s in %s", name, mapset);

    if (G_read_colors (name, mapset, &parms.pcolr) == -1)
    {
        error (fullname,"","can't read color table");
        return 0;
    }

/* open cell file for reading */

    if ((parms.cellfd = G_open_cell_old (name, mapset)) < 0)
    {
        error (fullname,"","can't open cell file");
	G_free_colors (&parms.pcolr);
        return 0;
    }

    strcpy (parms.celltitle, G_get_cell_title (name, mapset));
    G_strip (parms.celltitle);
    if (parms.celltitle[0] == 0)
	sprintf (parms.celltitle, "(%s)", name);

    parms.cellname   = G_store (name);
    parms.cellmapset = G_store (mapset);

    num = parms.pcolr.max - parms.pcolr.min + 1;

    parms.pattern = (PATTERN **) G_calloc(num+1, sizeof(PATTERN *));
    for (n = 0; n <= num; n++)
	parms.pattern[n] = (PATTERN *) NULL;
    return 1;
}
