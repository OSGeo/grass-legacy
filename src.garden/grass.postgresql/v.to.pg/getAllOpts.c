#include <stdlib.h>
#include "gis.h"
#include "dbvect.h"
#include "display.h"
#include "glocale.h"

int getAllOpts(argc, argv)
     int argc;
     char **argv;

{

    struct Option *key, *vtype, *where, *tab, *map, *color;
    struct Flag *flag1, *flag2, *flag3, *flag4;
    char *mapset;
    int colr, fillcolr, retval;


    map = G_define_option();
    map->key = "map";
    map->gisprompt = "old,dig,vector";
    map->type = TYPE_STRING;
    map->required = YES;
    map->multiple = NO;
    map->description = _("Vector map:");

    vtype = G_define_option();
    vtype->key = "type";
    vtype->type = TYPE_STRING;
    vtype->required = YES;
    vtype->multiple = NO;
    vtype->options = "area,line";
    vtype->description = _("Select area or line.");

    key = G_define_option();
    key->key = "key";
    key->type = TYPE_STRING;
    key->required = YES;
    key->multiple = NO;
    key->description = _("Column with category IDs from the table:");


    tab = G_define_option();
    tab->key = "tab";
    tab->type = TYPE_STRING;
    tab->required = YES;
    tab->multiple = NO;
    tab->description = _("Table containing this column:");

    where = G_define_option();
    where->key = "where";
    where->type = TYPE_STRING;
    where->required = NO;
    where->multiple = NO;
    where->description = _("Query clause (e.g. obj='paved'):");

    color = G_define_option();
    color->key = "color";
    color->type = TYPE_STRING;
    color->required = NO;
    color->multiple = NO;
    color->description = _("Color to draw selected vectors:");


    flag1 = G_define_flag();
    flag1->key = 'f';
    flag1->description = _("Fill polygons");

    flag2 = G_define_flag();
    flag2->key = 't';
    flag2->description = _("Without reference table, i.e., all vectors");

    flag3 = G_define_flag();
    flag3->key = 'v';
    flag3->description = _("Verbose mode");

    flag4 = G_define_flag();
    flag4->key = 'p';
    flag4->description = _("Create and populate PostGIS table instead");



    /* Invoke parser */
    if (G_parser(argc, argv)) {
	system("v.to.pg -s help");
	exit(-1);
    }

    if (color->answer == NULL)
	colr = D_translate_color("white");
    else
	colr = D_translate_color(color->answer);


    fillcolr = flag1->answer;
    total_import = flag2->answer;
    verbose = flag3->answer;
    to_postgis = flag4->answer;


    if ((mapset = G_find_file2("dig", map->answer, "")) == NULL) {
	fprintf(stderr, _("Vector map %s not found.\n"), map->answer);
	exit(-1);
    }

    map_string = map->answer;
    key_string = key->answer;
    table_string = tab->answer;
    vtype_string = vtype->answer;

    retval = buildPg(where->answer, map->answer, mapset, colr, fillcolr);

    exit(retval);


}
