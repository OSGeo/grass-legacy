
/*
**  Written by Dave Gerdes  Summer 1990
**  Enhanced by Bill Brown, 1992        
**  US Army Construction Engineering Research Lab
*/


/*
** Copyright USA CERL 1992. All rights reserved.
*/



#define MAIN
/*
#include "gis.h"
*/
#include "externs.h"

main(argc, argv)
	int argc;
	char **argv;
{
    struct Flag *swrit, *sitez;
    struct Option *elev, *colr, *vect, *site, *sitecol, *aut, *view;

    char *name1 = NULL;
    char *name2 = NULL;
    char *name3 = NULL;
    char *vname = NULL;
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

    view = G_define_option();
    view->key                    = "3dview";
    view->type                   = TYPE_STRING;
    view->required               = NO;
    view->multiple               = NO;
    view->gisprompt              = "old,3d.view,3dview";
    view->description            = "3D viewing parameters";

    site = G_define_option();
    site->key                    = "sites";
    site->type                   = TYPE_STRING;
    site->required               = NO;
    site->multiple               = NO;
    site->gisprompt              = "old,site_lists,Sites";
    site->description            = "Sites overlay file";

    sitecol = G_define_option();
    sitecol->key                    = "scolor";
    sitecol->type                   = TYPE_STRING;
    sitecol->required               = NO;
    sitecol->multiple               = NO;
    sitecol->gisprompt              = "old,cell,Raster";
    sitecol->description            = "Raster file for site category color";

    aut = G_define_option();
    aut->key                    = "script";
    aut->type                   = TYPE_STRING;
    aut->required               = NO;
    aut->multiple               = NO;
    aut->description            = "Automatically run script file";

    swrit = G_define_flag ();
    swrit->key = 'w';
    swrit->description = "Enable writing to script files";

    sitez = G_define_flag ();
    sitez->key = 'z';
    sitez->description = "Use site category as Z value";

    if (G_parser (argc, argv))
	exit (-1);


    AUTO_FILE = aut->answer;  /* either file name or NULL */

    Write_script = swrit->answer;

    Site_cat_isZ = sitez->answer;

    for (i = 0 ; colr->answers[i] ; i++)
	;

    if (i != 3 && i != 1)
	G_fatal_error ("Must supply 1 or 3(RGB) files for color");

    if (i == 3)
    {
	name2 = colr->answers[1];
	name3 = colr->answers[2];
	strcpy(Cellname[1], name2);
	strcpy(Cellname[2], name3);
    }
    name1 = colr->answers[0];

    strcpy(Cellname[0], name1);
    strcpy(Elevname, elev->answer);

    if(view->answer){
	View_file = 1;
	strcpy(Viewname, view->answer);
    }
    else
	View_file = 0;


    Dcell(elev->answer, name1, name2, name3, vect->answer,
		    site->answer, sitecol->answer, view->answer);
}



