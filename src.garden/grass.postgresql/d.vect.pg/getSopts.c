/* getSelectOpts.c - passes select range of program options to G_parser.
                     The [-s] option indicates that an input
                     file with SQL commands is being provided. This
		     ability to include a well formed SQL command
		     file gives the user more control over output
		     columns and complex database joins etc.

                     If the sql file requires input from GRASS
                     (eg category val or coord X,Y use a [?]
                     as a placeholder as per PREPARE). The SQL
                     input file will be parsed and the [?] will 
                     replaced prior to executing the query.

                  jaf 2/19/92
*/

#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "dbvect.h"
#include "display.h"
#include "glocale.h"

int getSelectOpts(argc, argv)
     int argc;
     char **argv;

{

    char *mapset;
    int colr, fillcolr, i, stat=0;
    FILE *fp;
    char SQL_stmt[QRY_LENGTH];

    struct Option *map, *color, *sql;
    struct Flag *select, *flag1, *flag2;

    select = G_define_flag();
    select->key = 's';
    select->description =
	_("Use [s] flag to select db records using an input file.");

    map = G_define_option();
    map->key = "map";
    map->type = TYPE_STRING;
    map->gisprompt = "old,dig,vector";
    map->required = YES;
    map->multiple = NO;
    map->description = _("Name of existing vector file.");

    sql = G_define_option();
    sql->key = "sql";
    sql->key_desc = "file";
    sql->type = TYPE_STRING;
    sql->required = YES;
    sql->multiple = NO;
    sql->description = _("SQL statements specifying selection criteria. ");

    color = G_define_option();
    color->key = "color";
    color->type = TYPE_STRING;
    color->required = NO;
    color->multiple = NO;
    color->description = _("Color for vector draw.");

    flag1 = G_define_flag();
    flag1->key = 'f';
    flag1->description = _("Fill polygons");

    flag2 = G_define_flag();
    flag2->key = 'e';
    flag2->description = _("Extract vector objects to new map");



    /* Check for help flag */
    for (i = 0; i < argc; i++)
	if (strcmp(argv[i], "help") == 0)
	    argv[1] = "help";

    if ((argc == 2) && (strcmp(argv[1], "-s") == 0)) {	/* Run interactive parser */
	/*argv[1] == NULL ; */
	argc = 1;
    }


    /* Invoke parser */
    if (G_parser(argc, argv))
	exit(-1);
    if (color->answer == NULL)
	colr = D_translate_color("white");
    else
	colr = D_translate_color(color->answer);


    fillcolr = flag1->answer;
    extract_yes = flag2->answer;



    if ((mapset = G_find_file2("dig", map->answer, "")) == NULL) {
	fprintf(stderr, _("Vector file %s not found.\n"), map->answer);
	exit(-1);
    }

    if ((fp = fopen(sql->answer, "r")) == NULL) {
	fprintf(stderr, _("File read error on %s\n"), sql->answer);
	exit(-1);
    }

    fread(SQL_stmt, QRY_LENGTH, 1, fp);
    fclose(fp);

    stat = runPg(SQL_stmt, map->answer, mapset, colr, fillcolr);

    exit(stat);

}
