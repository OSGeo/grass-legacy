/* main:
** the main function routine 
**/

#include <X11/Xmu/Editres.h>
#include "interface.h"
#include "gis.h"


XtAppContext app;

int 
main (int argc, char *argv[])
{
char rescmd[120];

    data_cell data;
    
    sprintf(rescmd, "xrdb -m $GISBASE/etc/app-defaults/%s", argv[0]);
    system(rescmd); 


    data.toplevel = XtInitialize(argv[0],"MenuTest",NULL,0,&argc,argv);

    XtAddEventHandler(data.toplevel, (EventMask) 0, True, _XEditResCheckMessages, NULL);


    G_gisinit (argv[0]);
    init1(&data);
    parse_command(&data, argc, argv);
    make_primary_controls(&data, argc, argv);
    init_lights(&data);
    XtMainLoop();
}

int 
parse_command (data_cell *dc, int argc, char **argv)
{
    struct Option *elev, *colr, *tricolr, *vct, *site, *view;

    char *name1 = NULL;
    char *name2 = NULL;
    char *name3 = NULL;
    char *vname = NULL;
    int i;


    elev = G_define_option();
    elev->key                    = "elevation";
    elev->type                   = TYPE_STRING;
    elev->required               = YES;
    elev->multiple               = YES;
    elev->gisprompt              = "old,cell,Raster";
    elev->description            = "Raster file(s) for Elevation";
/*
    colr = G_define_option();
    colr->key                    = "color";
    colr->type                   = TYPE_STRING;
    colr->required               = NO;
    colr->multiple               = YES;
    colr->gisprompt              = "old,cell,Raster";
    colr->description            = "Raster file(s) for Color";
*/

/*
    tricolr = G_define_option();
    tricolr->key            = "tricolor";
    tricolr->type           = TYPE_STRING;
    tricolr->required       = NO;
    tricolr->multiple       = YES;
    tricolr->gisprompt      = "old,cell,Raster";
    tricolr->description    = "Raster file(s) for Color (redfile, greenfile, bluefile)";
*/

    vct = G_define_option();
    vct->key                    = "vector";
    vct->type                   = TYPE_STRING;
    vct->required               = NO;
    vct->multiple               = YES;
    vct->gisprompt              = "old,dig,Vector";
    vct->description            = "Vector overlay file(s)";

    site = G_define_option();
    site->key                    = "sites";
    site->type                   = TYPE_STRING;
    site->required               = NO;
    site->multiple               = YES;
    site->gisprompt              = "old,site_lists,Sites";
    site->description            = "Sites overlay file(s)";

/*
    view = G_define_option();
    view->key                    = "3dview";
    view->type                   = TYPE_STRING;
    view->required               = NO;
    view->multiple               = NO;
    view->gisprompt              = "old,3d.view,3dview";
    view->description            = "3D viewing parameters";
*/

#ifdef XSCRIPT
    aut = G_define_option();
    aut->key                    = "script";
    aut->type                   = TYPE_STRING;
    aut->required               = NO;
    aut->multiple               = NO;
    aut->description            = "Automatically run script file";

    swrit = G_define_flag ();
    swrit->key = 'w';
    swrit->description = "Enable writing to script files";
#endif


    if (G_parser (argc, argv))
	exit (-1);

    {
    int defs[MAX_ATTS];

    defs[ATT_TOPO] = 0;
    defs[ATT_COLOR] = DEFAULT_SURF_COLOR;
    defs[ATT_MASK] = 0;
    defs[ATT_TRANSP] = 0;
    defs[ATT_SHINE] = 75;
    defs[ATT_EMIT] = 0;

    GS_set_att_defaults(defs);
    }	


#ifdef XSCRIPT
    AUTO_FILE = aut->answer;
    /* either file name or NULL */

    Write_script = swrit->answer;
#endif

    init2(dc);

    if(elev->answers){
	for (i = 0 ; elev->answers[i] ; i++){
	    load_new_surface(dc, elev->answers[i], 0.0);
	}
	if(i > 1)
	    set_default_wirecolors(dc, i);
    }
    if(vct->answers){
	for (i = 0 ; vct->answers[i] ; i++){
	    load_new_vector(dc, vct->answers[i], 1);
	}
    }
    if(site->answers){
	for (i = 0 ; site->answers[i] ; i++){
	    load_new_site(dc, site->answers[i], 1);
	}
    }

    init3(dc);
    


}




