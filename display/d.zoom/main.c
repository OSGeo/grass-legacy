/*
 *   d.zoom
 *
 *   Get region through graphics
 */

#include <stdlib.h>
#include <string.h> 
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "Vect.h"
#define	MAIN
#include "local_proto.h"
#include "glocale.h"

int 
main (int argc, char **argv)
{
    int stat;
#ifdef QUIET
    struct Flag *quiet;
#endif
    struct Flag *just, *full, *hand, *pan, *last;
    struct Option *rmap, *vmap, *zoom;
    struct GModule *module;
    double magnify;
    int i, first=1;
    char *mapset, *map;
    double ux1, uy1, ux2, uy2;
    struct Cell_head window, defwin, currwin, tmpwin;

    /* Initialize globals */
    rast = vect = NULL;
    nrasts = nvects = 0;

    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    module = G_define_module();
    module->description = 
	    "Allows the user to change the current geographic "\
             "region settings interactively, with a mouse.";

    /* Conditionalize R_open_driver() so "help" works, open quiet as well */
    R__open_quiet();
    if (R_open_driver() == 0)
    {
        if(D_get_cell_list (&rast, &nrasts) < 0)
            rast = NULL;
        else
        {
            rast = (char **)G_realloc(rast, (nrasts+1)*sizeof(char *));
            rast[nrasts] = NULL;
        }

        if(D_get_dig_list (&vect, &nvects) < 0)
            vect = NULL;
        else
        {
            vect = (char **)G_realloc(vect, (nvects+1)*sizeof(char *));
            vect[nvects] = NULL;
        }

        R_close_driver();
    }
                    
    rmap = G_define_option();
    rmap->key = "rast";
    rmap->type = TYPE_STRING;
    rmap->multiple = YES;
    if (rast)
          rmap->answers = rast;
    rmap->required = NO;
    rmap->gisprompt = "old,cell,raster" ;
    rmap->description = "Name of raster map";
                                                        
    vmap = G_define_option();
    vmap->key = "vector";
    vmap->type = TYPE_STRING;
    vmap->multiple = YES;
    if (vect)
          vmap->answers = vect;
    vmap->required = NO;
    vmap->gisprompt = "old,dig,vector" ;
    vmap->description = "Name of vector map";
                                                        
    zoom = G_define_option() ;
    zoom->key        = "zoom" ;
    zoom->type       = TYPE_DOUBLE ;
    zoom->required   = NO ;
    zoom->answer     = "0.75" ;
    zoom->options    = "0.001-1000.0" ;
    zoom->description= "magnification: >1.0 zooms in, <1.0 zooms out" ;

#ifdef QUIET
    quiet = G_define_flag();
    quiet->key = 'q';
    quiet->description = "Quiet";
#endif

    full = G_define_flag();
    full->key = 'f';
    full->description = "Full menu (zoom + pan) & Quit menu";

    pan = G_define_flag();
    pan->key = 'p';
    pan->description = "Pan mode";

    hand = G_define_flag();
    hand->key = 'h';
    hand->description = "Handheld mode";

    just = G_define_flag();
    just->key = 'j';
    just->description = "Just redraw given maps using default colors";

    last = G_define_flag();
    last->key = 'r';
    last->description = "Return to previous zoom";

    if(!rast && !vect )
    {
	rmap->required = YES;
	just->answer = 1;
    }

    if ((argc > 1 || (!rast && !vect )) && G_parser(argc,argv))
	exit(1);

    if(getenv("GRASS_ANOTHER_BUTTON")){
	    leftb   = 1; lefts   = "Left:  ";
	    middleb = 3; middles = "Right: ";
	    rightb  = 2; rights  = "Middle:";
    }else{
	    leftb   = 1; lefts   = "Left:  ";
	    middleb = 2; middles = "Middle:";
	    rightb  = 3; rights  = "Right: ";
    }

    if( (full->answer + pan->answer + hand->answer) > 1)
	G_fatal_error("Please choose only one mode of operation");

    sscanf(zoom->answer,"%lf", &magnify);

#ifdef QUIET
    /* if map was found in monitor: */
    if (rast || vect ) 
       quiet->answer=1;
#endif

    cmd = NULL;
    if (!just->answer)
    {
        if (R_open_driver() != 0)
	    G_fatal_error ("No graphics device selected");
	stat = R_pad_get_item("list", &list, &nlists);
	R_close_driver();
	if (stat || !nlists)
	{
	    fprintf(stderr, _("ERROR: can not get \"list\" items\n"));
	    fprintf(stderr, _("-j flag forced\n"));
	    just->answer = 1;
	}
	else
	{
	    cmd = (char *)G_malloc(strlen(list[0])+1);
	    strcpy(cmd, list[0]);
	    for(i=1; i<nlists; i++)
	    {
		cmd = (char *)G_realloc(cmd, strlen(cmd)+strlen(list[i])+2);
		strcat(cmd, "\n");
		strcat(cmd, list[i]);
	    }
	}
    }

    if (just->answer)
    {
    	if (rmap->answers && rmap->answers[0])
	    rast = rmap->answers;
	else
	{
	    rast = NULL;
	    nrasts = 0;
	}
    	if (vmap->answers && vmap->answers[0])
	    vect = vmap->answers;
	else
	{
	    vect = NULL;
	    nvects = 0;
	}
    }


/* Make sure map is available */
    if (rmap->required == YES && rmap->answers == NULL)
    {
	fprintf(stderr, _("ERROR: No map is displayed in GRASS monitor\n"));
	exit(1);
    }

    if (rast)
    {
        struct Cell_head window;

	for(i=0; rast[i]; i++);
	nrasts = i;

	for(i=0; i<nrasts; i++){
    		mapset = G_find_cell2 (rast[i], "");
    		if (mapset == NULL)
    		{
			char msg[256];
			sprintf(msg,"Raster file [%s] not available", rast[i]);
			G_fatal_error(msg) ;
		}
		else
		{
	 		if(G_get_cellhd(rast[i], mapset, &window) >= 0)
			{
	 			if(first)
	 			{
					first = 0;
					U_east = window.east;
					U_west = window.west;
					U_south = window.south;
					U_north = window.north;
	 			}
	 			else
				{
					if(window.east > U_east)
						U_east = window.east;
					if(window.west < U_west)
						U_west = window.west;
					if(window.south < U_south)
						U_south = window.south;
					if(window.north > U_north)
						U_north = window.north;
				}
			}
		}
	}
    }

    if (vmap->required == YES && vmap->answers == NULL)
	exit(0);

    if (vect)
    {
        struct Map_info Map;
	BOUND_BOX box;

	for(i=0; vect[i]; i++);
	nvects = i;

	for(i=0; i<nvects; i++){
    		mapset = G_find_vector2 (vect[i], "");
    		if (mapset == NULL)
    		{
			char msg[256];
			sprintf(msg,"Vector file [%s] not available", vect[i]);
			G_fatal_error(msg) ;
		}
		else
		{
			if(Vect_open_old(&Map, vect[i], mapset) >= 2)
			{
			        Vect_get_map_box (&Map, &box );
	 			if(first)
	 			{
					first = 0;
					U_east = box.E;
					U_west = box.W;
					U_south = box.S;
					U_north = box.N;
	 			}
	 			else
				{
					if(box.E > U_east)
						U_east = box.E;
					if(box.W < U_west)
						U_west = box.W;
					if(box.S < U_south)
						U_south = box.S;
					if(box.N > U_north)
						U_north = box.N;
				}
			}
		}
	}
    }

#ifdef BOUNDARY
    if(!first)
    {
    /*
	    if(U_east == U_west)
	    {
		    U_east += 100;
		    U_west -= 100;
	    }
	    if(U_south == U_north)
	    {
		    U_south -= 100;
		    U_north += 100;
	    }
    */

	    U_east += 0.05 * (U_east - U_west);
	    U_west -= 0.05 * (U_east - U_west);
	    U_south -= 0.05 * (U_north - U_south);
	    U_north += 0.05 * (U_north - U_south);
    }
#endif

    if (R_open_driver() != 0)
        G_fatal_error ("No graphics device selected");
    
    D_setup(0);

    if ( !hand->answer ) { 
        fprintf(stderr, _("%d raster%s, %d vector%s\n"),
		    nrasts, (nrasts > 1 ? "s":""),
		    nvects, (nvects > 1 ? "s":""));
    }

   if (last->answer)  /* restoring temporary region */
   {
      map = G_find_file ("windows", "zoom_temp", "");
      
      if (!map) {
	fprintf(stderr,_("this is the first d.zoom run; no previous zoom availible\n"));
	return(0);
      }

      G__get_window (&tmpwin, "windows", "zoom_temp", map);
 
      fprintf(stderr, "returning to last zoom\n") ;
 
      ux1 = tmpwin.east;
      ux2 = tmpwin.west;
      uy1 = tmpwin.north;
      uy2 = tmpwin.south;

      set_win(&tmpwin, ux1, uy1, ux2, uy2, hand);
      
      exit(0);
    }

    /* Do the zoom */
    G_get_window(&window);
    G__put_window(&window, "windows", "zoom_temp"); /* Save current region before it is changed */
    G_get_window(&currwin);
    G_get_default_window(&defwin);
    if ( full->answer == 1 )
	stat = zoomwindow(&window, 1, magnify);
    else if(pan->answer == 1)
	    do_pan(&window);
    else {
	if ( hand->answer == 0 )
            make_window_box (&window, magnify, 0, 0);
	else
	    make_window_box (&window, magnify, 0, 1);
    }
    
    if (full->answer) {
      quit(&defwin,&currwin); /* calling the quit menu function */
    }

    R_close_driver();

    if (rast)
      R_pad_freelist(rast, nrasts);

    if (vect)
      R_pad_freelist(vect, nvects);

    fprintf(stdout,_("Zooming finished.\n"));
    exit(stat);
}
