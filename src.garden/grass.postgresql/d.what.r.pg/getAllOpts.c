/* GetAllOpts.c - passes full range of program options to G_parser. If
                  the user uses the [-s] option indicoling that he
                  is providing am input file with SQL commands the
                  function getSelectOpts is called in place of this
                  routine.

                  jaf 2/19/92
*/
#define GLOBAL
#include "what.h"
#include <stdlib.h>
#include <string.h>
#include "display.h"
#include "raster.h"
#include "glocale.h"

int getAllOpts(argc, argv)
     int argc;
     char **argv;

{
    struct Option *map, *keytable, *col, *hv;
    int button;
    int stat = 0;
    char *print_out;

    map = G_define_option();
    map->key = "map";
    map->gisprompt = "old,cell,raster";
    map->type = TYPE_STRING;
    map->required = YES;
    map->multiple = NO;
    map->description = _("Raster map to query:");

    keytable = G_define_option();
    keytable->key = "tab";
    keytable->type = TYPE_STRING;
    keytable->required = YES;
    keytable->multiple = NO;
    keytable->description = _("Table with categories:");

    col = G_define_option();
    col->key = "col";
    col->type = TYPE_STRING;
    col->required = YES;
    col->multiple = NO;
    col->description = _("Column with categories:");

    hv = G_define_option();
    hv->key = "hv";
    hv->type = TYPE_STRING;
    hv->answer = "v";
    hv->description = _("Database output format - [v(ert)/h(oriz)]:");


    /* Invoke parser */
    if (G_parser(argc, argv)) {
	system("d.what.r.pg -s help");
	exit(-1);
    }

    print_out = hv->answer;

    /* Initialize screen graphics and get mouse input */

    R_open_driver();
    D_setup(0);
    if ((fd = opencell(map->answer, mapset)) >= 0)
	do {
	    button = getCat(map->answer);
	    if ((button != 3) && (dbCat > 0)) {
		stat = buildPg(keytable, col, dbCat, print_out);
	    }
	} while (button != 3);

    R_close_driver();
    exit(stat);
}
