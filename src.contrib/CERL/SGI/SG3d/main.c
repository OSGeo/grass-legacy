
/*
**  Written by Dave Gerdes  Summer 1990
**  US Army Construction Engineering Research Lab
*/


#define USAGE	"cell_file"

#define MAIN
/*
#include "options.h"
*/
#include "gis.h"
#include "externs.h"

main(argc, argv)
	int argc;
	char **argv;
{
    struct Flag *swrit;
    struct Option *elev, *colr, *vect, *aut;

    char *name1 = NULL;
    char *name2 = NULL;
    char *name3 = NULL;
    int i;

    G_gisinit (argv[0]);

    elev = G_define_option();
    elev->key                    = "elevation";
    elev->type                   = TYPE_STRING;
    elev->required               = YES;
    elev->multiple               = NO;
    elev->gisprompt              = "old,cell,Raster";
    elev->description            = "Raster file for Elevation";

    colr = G_define_option();
    colr->key                    = "color";
    colr->type                   = TYPE_STRING;
    colr->required               = YES;
    colr->multiple               = YES;
    colr->gisprompt              = "old,cell,Raster";
    colr->description            = "Raster file(s) for Color (1 or 3 files)";

    vect = G_define_option();
    vect->key                    = "vector";
    vect->type                   = TYPE_STRING;
    vect->required               = NO;
    vect->multiple               = NO;
    vect->gisprompt              = "old,dig,Vector";
    vect->description            = "Vector overlay file";

    aut = G_define_option();
    aut->key                    = "script";
    aut->type                   = TYPE_STRING;
    aut->required               = NO;
    aut->multiple               = NO;
    aut->description            = "Automatically run script file";

    swrit = G_define_flag ();
    swrit->key = 'w';
    swrit->description = "Enable writing to script files";

    if (G_parser (argc, argv))
	exit (-1);


    AUTO_FILE = aut->answer;  /* either file name or NULL */

    Write_script = swrit->answer;

    for (i = 0 ; colr->answers[i] ; i++)
	;

    if (i != 3 && i != 1)
	G_fatal_error ("Must supply 1 or 3(RGB) files for color");

    if (i == 3)
    {
	name2 = colr->answers[1];
	name3 = colr->answers[2];
    }
    name1 = colr->answers[0];


	Dcell(elev->answer, name1, name2, name3, vect->answer);
}
