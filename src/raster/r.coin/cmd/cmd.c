#include <string.h>
#include "coin.h"


int 
command_version (int argc, char *argv[])
{
	struct GModule *module;
    struct
    {
	struct Option *map1, *map2, *units;
    } parm;
    struct
    {
	struct Flag *w, *q;
    } flag;

	module = G_define_module();
	module->description =
			"Tabulates the mutual occurrence (coincidence) "
			"of categories for two raster map layers.";

    parm.map1 = G_define_option();
    parm.map1->key = "map1";
    parm.map1->required = YES;
    parm.map1->type = TYPE_STRING;
	parm.map1->gisprompt  = "old,cell,raster" ;
    parm.map1->description = "Name of first raster map";

    parm.map2 = G_define_option();
    parm.map2->key = "map2";
    parm.map2->required = YES;
    parm.map2->type = TYPE_STRING;
	parm.map2->gisprompt  = "old,cell,raster" ;
    parm.map2->description = "Name of second raster map";

    parm.units = G_define_option();
    parm.units->key = "units";
    parm.units->required = YES;
    parm.units->type = TYPE_STRING;
    parm.units->description = "Unit of measure";
    parm.units->options = "c,p,x,y,a,h,k,m";

    flag.q = G_define_flag();
    flag.q->key = 'q';
    flag.q->description = "Quiet";

    flag.w = G_define_flag();
    flag.w->key = 'w';
    flag.w->description = "Wide report, 132 columns (default: 80)";

    if (G_parser(argc, argv))
	exit(1);
    strcpy (map1name, parm.map1->answer);
    strcpy (map2name, parm.map2->answer);
    mapset1 = G_find_cell2 (map1name, "");
    if(!mapset1)
    {
	fprintf (stderr, "%s: <%s> raster map not found\n", argv[0], map1name);
	exit (1);
    }
    mapset2 = G_find_cell2 (map2name, "");
    if(!mapset2)
    {
	fprintf (stderr, "%s: <%s> raster map not found\n", argv[0], map2name);
	exit (1);
    }

    make_coin(!flag.q->answer);
    print_coin (*parm.units->answer, flag.w->answer?132:80, 0);

  return 0;
}
