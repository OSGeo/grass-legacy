/*
 *   d.zoom
 *
 *   Get region through graphics
 */

#define	MAIN

#include "gis.h"
#include "datetime.h"
#include "display.h"
#include "raster.h"
#include "Vect.h"
#include "local_proto.h"

int 
main (int argc, char **argv)
{
    int stat;
    int rotate;
    struct Flag *quiet;
    struct Option *action;
    struct Option *rmap, *vmap, *smap, *zoom;
    double magnify;
    int i, first=1;
    char *mapset;
                        
/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;
    R_open_driver();

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

    if(D_get_site_list (&site, &nsites) < 0)
	site = NULL;
    else
    {
	site = (char **)G_realloc(site, (nsites+1)*sizeof(char *));
	site[nsites] = NULL;
    }

    R_close_driver();
                    
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
                                                        
    smap = G_define_option();
    smap->key = "site";
    smap->type = TYPE_STRING;
    smap->multiple = YES;
    if (site)
          smap->answers = site;
    smap->required = NO;
    smap->gisprompt = "old,site_lists,sites" ;
    smap->description = "Name of site file";
                                                        
    if(!rast && !vect && !site)
    {
	    rmap->required = YES;
    }

    action = G_define_option();
    action->key = "action";
    action->type = TYPE_STRING;
    action->description = "Type of zoom (for lat/lon databases only)";
    action->options = "zoom,rotate";
    action->required = NO;
    action->answer = NULL; /* do NOT set a default, please */

    zoom = G_define_option() ;
    zoom->key        = "zoom" ;
    zoom->type       = TYPE_DOUBLE ;
    zoom->required   = NO ;
    zoom->answer     = "0.75" ;
    zoom->options    = "0.001-1000.0" ;
    zoom->description= "magnification: >1.0 zooms in, <1.0 zooms out" ;

    quiet = G_define_flag();
    quiet->key = 'q';
    quiet->description = "Quiet";

    if (argc > 1 && G_parser(argc,argv))
	exit(1);

    sscanf(zoom->answer,"%lf", &magnify);

/* Make sure map is available */
    if (rmap->required == YES && rmap->answers == NULL)
	exit(0);

    if (rmap->answers)
    {
        struct Cell_head window;

	if (!nrasts)
	{
		for(i=0; rmap->answers[i]; i++);
		nrasts = i;
	}
	for(i=0; i<nrasts; i++){
    		mapset = G_find_cell2 (rmap->answers[i], "");
    		if (mapset == NULL)
    		{
			char msg[256];
			sprintf(msg,"Raster file [%s] not available", rmap->answers[i]);
			G_fatal_error(msg) ;
		}
		else
		{
	 		if(G_get_cellhd(rmap->answers[i], mapset, &window) >= 0)
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

    if (vmap->answers)
    {
        struct Map_info Map;

	if (!nvects)
	{
		for(i=0; vmap->answers[i]; i++);
		nvects = i;
	}
        Vect_set_open_level(1);
	for(i=0; i<nvects; i++){
    		mapset = G_find_vector2 (vmap->answers[i], "");
    		if (mapset == NULL)
    		{
			char msg[256];
			sprintf(msg,"Vector file [%s] not available", vmap->answers[i]);
			G_fatal_error(msg) ;
		}
		else
		{
			if(Vect_open_old(&Map, vmap->answers[i], mapset) == 1)
			{
	 			if(first)
	 			{
					first = 0;
					U_east = Map.head.E;
					U_west = Map.head.W;
					U_south = Map.head.S;
					U_north = Map.head.N;
	 			}
	 			else
				{
					if(Map.head.E > U_east)
						U_east = Map.head.E;
					if(Map.head.W < U_west)
						U_west = Map.head.W;
					if(Map.head.S < U_south)
						U_south = Map.head.S;
					if(Map.head.N > U_north)
						U_north = Map.head.N;
				}
			}
		}
	}
    }

    if (smap->required == YES && smap->answers == NULL)
	exit(0);

    if (smap->answers)
    {
	FILE *fp;
	Site mysite;

	mysite.dim_alloc = mysite.dbl_alloc = mysite.str_alloc = 0;

	if (!nsites)
	{
		for(i=0; smap->answers[i]; i++);
		nsites = i;
	}
	for(i=0; i<nsites; i++){
    		mapset = G_find_sites2 (smap->answers[i], "");
    		if (mapset == NULL)
    		{
			char msg[256];
			sprintf(msg,"Site file [%s] not available", smap->answers[i]);
			G_fatal_error(msg) ;
		}
		else
		{
			if(NULL != (fp = G_fopen_sites_old(smap->answers[i], mapset)))
			{
				/*
				while(!feof(fp))
				{
					if(G_site_get(fp, &mysite))
						continue;
				*/
				while(G_site_get(fp, &mysite) == 0)
				{
					fprintf(stderr,"NEVER REACH HERE, STRANGE !!!");
					if(first)
					{
						first = 0;
						U_east = mysite.east;
						U_west = mysite.east;
						U_south = mysite.north;
						U_north = mysite.north;
					}
					else
					{
						if(mysite.east > U_east)
							U_east = mysite.east;
						if(mysite.east < U_west)
							U_west = mysite.east;
						if(mysite.north < U_south)
							U_south = mysite.north;
						if(mysite.north > U_north)
							U_north = mysite.north;
					}
				}
				fclose(fp);
			}
		}
	}
    }

    /* if map was found in monitor: */
    if (rast || vect || site) 
       quiet->answer=1;

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

/* find out for lat/lon if zoom or rotate option */
    rotate = 0;
    if (G_projection() == PROJECTION_LL)
    {
	if (action->answer)
	    rotate = strcmp (action->answer,"rotate") == 0;
	else
	    rotate = ask_rotate();
    }


    do
    {
        R_open_driver();
    
        D_setup(0);
    
    /* Do the zoom */
        stat = zoomwindow(quiet->answer, rotate, magnify);
    
        R_close_driver();
    } while(stat == 2);

    if (rast)
      R_pad_freelist(rast, nrasts);

    if (vect)
      R_pad_freelist(vect, nvects);

    if (site)
      R_pad_freelist(site, nsites);

    exit(stat);
}

#undef	MAIN

