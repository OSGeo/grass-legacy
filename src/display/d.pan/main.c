/*
 *   d.pan
 *
 *   Change region through graphics - select new center
 */

#include "gis.h"
#include "site.h"
#include "display.h"
#include "raster.h"
#include "Vect.h"
#define MAIN
#include "local_proto.h"

int main (int argc, char **argv)
{
    int stat;
    char command[128];
#ifdef QUIET
    struct Flag *quiet;
#endif
    struct Flag *just;
    struct Option *rmap, *vmap, *smap, *zoom;
    double magnify;
    char *mapset;
    int i, first=1;

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
                                                        
    zoom = G_define_option() ;
    zoom->key        = "zoom" ;
    zoom->type       = TYPE_DOUBLE ;
    zoom->required   = NO ;
    zoom->answer     = "1.0" ;
    zoom->options    = "0.001-1000.0" ;
    zoom->description= "magnification: >1.0 zooms in, <1.0 zooms out" ;

#ifdef QUIET
    quiet = G_define_flag();
    quiet->key = 'q';
    quiet->description = "Quiet";
#endif

    just = G_define_flag();
    just->key = 'j';
    just->description = "Just redraw given maps using default colors";

    if(!rast && !vect && !site)
    {
	    rmap->required = YES;
	    just->answer = 1;
    }

    if (argc > 1 && G_parser(argc,argv))
	exit(1);

    sscanf(zoom->answer,"%lf", &magnify); 

#ifdef QUIET
    /* if map was found in monitor: */
    if (rast || vect || site) 
       quiet->answer=1;
#endif

    cmd = NULL;
    if (!just->answer)
    {
        R_open_driver();
	stat = R_pad_get_item("list", &list, &nlists);
	R_close_driver();
	if (stat || !nlists)
	{
	    fprintf(stderr, "ERROR: can not get \"list\" items\n");
	    fprintf(stderr, "-j flag forced\n");
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
    	if (smap->answers && smap->answers[0])
	    site = smap->answers;
	else
	{
	    site = NULL;
	    nsites = 0;
	}
    }

/* Make sure map is available */
    if (rmap->required == YES && rmap->answers == NULL)
    {
	fprintf(stderr, "ERROR: No map is given\n");
	exit(1);
    }

    if (rast)
    {
        struct Cell_head window;

	if (!nrasts)
	{
		for(i=0; rast[i]; i++);
		nrasts = i;
	}
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

	if (!nvects)
	{
		for(i=0; vect[i]; i++);
		nvects = i;
	}
        Vect_set_open_level(1);
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
			if(Vect_open_old(&Map, vect[i], mapset) == 1)
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

    if (site)
    {
	FILE *fp;
	Site *s;
	int rtype, ndim, nstr, ndec;

	if (!nsites)
	{
		for(i=0; site[i]; i++);
		nsites = i;
	}
	for(i=0; i<nsites; i++){
    		mapset = G_find_sites2 (site[i], "");
    		if (mapset == NULL)
    		{
			char msg[256];
			sprintf(msg,"Site file [%s] not available", site[i]);
			G_fatal_error(msg) ;
		}
		else
		{
			if(NULL != (fp = G_fopen_sites_old(site[i], mapset)))
			{
				rtype = -1;
				G_site_describe(fp, &ndim, &rtype, &nstr, &ndec);
				s = G_site_new_struct(rtype, ndim, nstr, ndec);
				/*
				while(G_site_get(fp, s) == 0)
				{
				*/
				while(!feof(fp))
				{
					if(G_site_get(fp, s))
						continue;
					if(first)
					{
						first = 0;
						U_east = s->east;
						U_west = s->east;
						U_south = s->north;
						U_north = s->north;
					}
					else
					{
						if(s->east > U_east)
							U_east = s->east;
						if(s->east < U_west)
							U_west = s->east;
						if(s->north < U_south)
							U_south = s->north;
						if(s->north > U_north)
							U_north = s->north;
					}
				}

				/* is 100 enough to contain one point from
				 * boundary?
				 */
				U_east += 100;
				U_west -= 100;
				U_south -= 100;
				U_north += 100;

				G_free(s);
				fclose(fp);
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

    R_open_driver();

    D_setup(0);

    if (G_projection() == PROJECTION_LL)
    {
    }

/* Do the pan */
#ifndef QUIET
    stat = pan(1,magnify) ;
#else
    stat = pan(quiet->answer,magnify) ;
#endif

    R_close_driver();

    if (rast)
      R_pad_freelist(rast, nrasts);

    if (vect)
      R_pad_freelist(vect, nvects);
    
    if (site)
      R_pad_freelist(site, nsites);
    
    exit(stat);
}

