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

#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "dbrast.h"
#include "glocale.h"

int getSelectOpts(argc, argv)
     int argc;
     char **argv;


{
    int i, stat, k;

    struct Option *sql, *input, *output;
    struct Flag *select;

    char buf[QRY_LENGTH];
    char SQL_stmt[QRY_LENGTH];
    char tmpstr[8];
    char ch;

    FILE *fp;


    memset(buf, '\0', sizeof(buf));
    memset(SQL_stmt, '\0', sizeof(SQL_stmt));
    memset(tmpstr, '\0', sizeof(tmpstr));

    stat = 0;

    select = G_define_flag();
    select->key = 's';
    select->description = _("Use [-s] flag for query input from file.");

    input = G_define_option();
    input->key = "input";
    input->gisprompt = "old,cell,raster";
    input->type = TYPE_STRING;
    input->required = YES;
    input->multiple = NO;
    input->description = _("Raster map (must exist):");

    sql = G_define_option();
    sql->key = "sql";
    sql->key_desc = "file";
    sql->type = TYPE_STRING;
    sql->required = YES;
    sql->multiple = NO;
    sql->description = _("SQL command file: ");

    output = G_define_option();
    output->key = "output";
    output->gisprompt = "new,cell,raster";
    output->type = TYPE_STRING;
    output->required = NO;
    output->multiple = NO;
    output->description = _("Reclass map (new):");


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

    if (!(G_find_cell(input->answer, ""))) {
	fprintf(stderr, _("Raster map %s not found.\n"), input->answer);
	exit(-1);
    }


    if ((fp = fopen(sql->answer, "r")) == NULL) {
	fprintf(stderr, _("File read error on select file (%s)\n"),
		sql->answer);
	exit(-1);
    }

    k = 0;

    while (!feof(fp) || k >= 1023) {
	ch = getc(fp);
	k++;

	sprintf(tmpstr, "%c", ch);
	strncat(buf, tmpstr, 1);
    }
    fclose(fp);

    strncpy(SQL_stmt, buf, strlen(buf) - 1);
    stat = runPg(SQL_stmt, input->answer, output->answer, 0);
    return (stat);
}
