
#include "gis.h"
main(argc, argv) char *argv[];
{
    char name[256];
    char rname[256], rmapset[256];
    char buf[1024];
    struct Cell_head cellhd;
    int cellhd_ok;
    int is_reclass;
    int error();

    G_gisinit (argv[0]);
    if (G_ask_cell_in_mapset ("Enter name of raster file for which you will create/modify support files", name) == NULL)
	exit(0) ;

/* cell header */
    cellhd_ok = G_get_cellhd (name, G_mapset(), &cellhd) >= 0 ;
    is_reclass = (G_is_reclass (name, G_mapset(), rname, rmapset) > 0);

    sprintf (buf, "Edit the header for [%s]? ", name);

    if (is_reclass)
    {
	printf ("\nNote: [%s] is a reclass of [%s in %s]\n\n",
		name, rname, rmapset);
    }
    else if (G_yes (buf, cellhd_ok?0:1))
    {
	G_clear_screen();
	sprintf (buf, "%s/etc/modhead - '%s'", G_gisbase(), name);
	system (buf);
	if(cellhd_ok = G_get_cellhd (name, G_mapset(), &cellhd) >= 0)
	{
	    hitreturn();
	    G_clear_screen();
	}
    }
    if (!cellhd_ok)
    {
	printf ("Can't continue\n");
	sleep(3);
	exit(1);
    }

/* check the histogram and range */
    check_stats (name);

/* category file */
    sprintf (buf, "Edit the category file for [%s]? ", name);
    if (G_yes (buf, 0))
    {
	G_clear_screen();
	sprintf (buf, "%s/etc/modcats '%s'", G_gisbase(), name);
	system (buf);
	hitreturn();
	G_clear_screen();
    }

/* color table */
    sprintf (buf, "Create/Update the color table for [%s]? ", name);
    if (G_yes (buf, 0))
    {
	G_clear_screen();
	sprintf (buf, "%s/etc/modcolr '%s'", G_gisbase(), G_fully_qualified_name(name, G_mapset()));
	system (buf);
	hitreturn();
	G_clear_screen();
    }

/* history file */
    sprintf (buf, "Edit the history file for [%s]? ", name);
    if (G_yes (buf, 0))
    {
	G_clear_screen();
	sprintf (buf, "%s/etc/modhist '%s'", G_gisbase(), name);
	system (buf);
	hitreturn();
	G_clear_screen();
    }

    exit(0);
}
G_clear_screen(){}
