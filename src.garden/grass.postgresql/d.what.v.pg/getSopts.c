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

#include "what.h"
#include "display.h"
#include "raster.h"
#include "Vect.h"
#include <string.h>
#include <stdlib.h>
#include "glocale.h"

int getSelectOpts(argc, argv)
     int argc;
     char **argv;

{

    char *openvect();
    int button;
    int level;
    int stat = 0;
    struct Map_info P_map;
    struct Categories Cats;
    FILE *fp;
    char ch;
    char buf[QRY_LENGTH];
    char tmpstr[QRY_LENGTH];
    char SQL_stmt[QRY_LENGTH];
    char *print_out;

    struct Option *sql, *map, *color, *fillcolor, *hv;
    struct Flag *select, *flag1;
    int i, colr, end_l, k;
    int fillcolr = 0;
    int fill = 0;
    char *panell;

    panell = G_tempfile();


    memset(buf, '\0', sizeof(buf));
    memset(tmpstr, '\0', sizeof(tmpstr));
    memset(SQL_stmt, '\0', sizeof(SQL_stmt));



    select = G_define_flag();
    select->key = 's';
    select->description = _("Use [-s] for SQL command file input.");

    map = G_define_option();
    map->key = "map";
    map->gisprompt = "old,dig,vector";
    map->type = TYPE_STRING;
    map->required = YES;
    map->multiple = NO;
    map->description = _("Vector map to run query on:");

    sql = G_define_option();
    sql->key = "sql";
    sql->key_desc = "file";
    sql->type = TYPE_STRING;
    sql->required = YES;
    sql->multiple = NO;
    sql->description = _("Name of the SQL command file: ");

    color = G_define_option();
    color->key = "color";
    color->type = TYPE_STRING;
    color->required = NO;
    color->multiple = NO;
    color->description = _("Selected lines color:");
    color->answer = "yellow";

    flag1 = G_define_flag();
    flag1->key = 'f';
    flag1->description = _("Fill polygons?");

    fillcolor = G_define_option();
    fillcolor->key = "fillcolor";
    fillcolor->type = TYPE_STRING;
    fillcolor->answer = "yellow";
    fillcolor->options = D_color_list();
    fillcolor->description = _("Selected areas color (for polys):");

    hv = G_define_option();
    hv->key = "hv";
    hv->type = TYPE_STRING;
    hv->answer = "v";
    hv->description = _("Type of database output - [h/v]:");


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

    /* Initialize screen graphics and get mouse input */

    name = map->answer;
    print_out = hv->answer;
    fill = flag1->answer;

    if ((mapset = openvect(name)) == NULL) {
	fprintf(stderr, _("Unable to open %s\n"), map->answer);
	exit(1);
    }

    R_open_driver();
    colr = D_translate_color(color->answer);
    fillcolr = D_translate_color(fillcolor->answer);
    D_setup(0);

    level = Vect_open_old(&P_map, name, mapset);
    if (level < 0)
	G_fatal_error(_("Can't open vector file"));
    if (level < 2)
	G_fatal_error(_("You must first run v.support on vector file"));

    if (G_read_vector_cats(name, mapset, &Cats) < 0)
	Cats.num = -1;

    h_num = 1;

    do {
    	
	R_panel_save(panell, R_screen_top(), R_screen_bot(),
		     R_screen_left(), R_screen_rite());

	button = getCat(&P_map, &Cats, colr, fillcolr, fill);
	if ((button != 3) && (dbCat > 0)) {

	    /* read all lines of sql stmt into a var  */

	    if ((fp = fopen(sql->answer, "r")) == NULL) {
		fprintf(stderr, _("File read error on %s\n"), sql->answer);
		exit(-1);
	    }

	    end_l = 0;
	    k = 0;
	    strcpy(buf, "");

	    while (!feof(fp) || k >= (sizeof(buf)-1)) {
		ch = getc(fp);
		k++;
		if (ch == '?') {
		    sprintf(tmpstr, "%d", dbCat);
		    strncat(buf, tmpstr, strlen(tmpstr));
		}
		else {
		    sprintf(tmpstr, "%c", ch);
		    strncat(buf, tmpstr, 1);
		}


	    }
	    fclose(fp);


	    strncpy(SQL_stmt, buf, strlen(buf) - 1);
	    stat = runPg(SQL_stmt, print_out);
	}
	
	if (button != 2) R_panel_restore(panell);
	R_panel_delete(panell);

    } while (button != 3);

    R_close_driver();
    Vect_close(&P_map);

    exit(stat);
}
