/*
 * d.extend: set window region from displayed maps.
 *
 *	Copyright (C) 2000 by the GRASS Development Team
 *	Author: Huidae Cho <grass4u@gmail.com>
 *
 *	This program is free software under the GPL (>=v2)
 *	Read the file COPYING coming with GRASS for details.
 *
 */

#include <stdlib.h>
#include <grass/gis.h>
#include <grass/site.h>
#include <grass/Vect.h>
#include <grass/raster.h>
#include <grass/display.h>
#include <grass/glocale.h>

int 
main (int argc, char **argv)
{
    struct GModule *module;
    int i, first=1;
    char *mapset;
    char **rast, **vect, **site;
    int nrasts, nvects, nsites;
    struct Cell_head window, temp_window;

    G_gisinit(argv[0]) ;

    module = G_define_module();
    module->keywords = _("display, setup");
    module->description =
		"Set window region so that all currently displayed raster, "
		"vector and sites maps can be shown in a monitor.";

    if(argc > 1 && G_parser(argc, argv))
	    exit(-1);


    if (R_open_driver() != 0)
	G_fatal_error ("No graphics device selected");

    if(D_get_cell_list (&rast, &nrasts) < 0)
	rast = NULL;

    if(D_get_dig_list (&vect, &nvects) < 0)
	vect = NULL;

    if(D_get_site_list (&site, &nsites) < 0)
	site = NULL;

    R_close_driver();

    if (rast == NULL && vect == NULL && site == NULL)
    	G_fatal_error("No raster, vector or sites file displayed");

    G_get_window(&window);

    if (rast)
    {
	for(i=0; i<nrasts; i++){
    		mapset = G_find_cell2 (rast[i], "");
    		if (mapset == NULL)
    		{
			char msg[256];
			sprintf(msg,"Raster file [%s] not available", rast[i]);
			G_fatal_error(msg) ;
		}
	 	if(G_get_cellhd(rast[i], mapset, &temp_window) >= 0)
		{
			if(first)
			{
				first = 0;
				G_copy(&window, &temp_window, sizeof(window));
			}
			else
			{
				if(window.east < temp_window.east)
					window.east = temp_window.east;
				if(window.west > temp_window.west)
					window.west = temp_window.west;
				if(window.south > temp_window.south)
					window.south = temp_window.south;
				if(window.north < temp_window.north)
					window.north = temp_window.north;
				/*
				if(window.ns_res < nsres)
					nsres = window.ns_res;
				if(window.ew_res < ewres)
					ewres = window.ew_res;
				*/
			}
		}
	}

	G_adjust_Cell_head3(&window, 0, 0, 0);
    }

    if (vect)
    {
        struct Map_info Map;

	G_copy(&temp_window, &window, sizeof(window));

        Vect_set_open_level(2);
	for(i=0; i<nvects; i++){
    		mapset = G_find_vector2 (vect[i], "");
    		if (mapset == NULL)
    		{
			char msg[256];
			sprintf(msg,"Vector file [%s] not available", vect[i]);
			G_fatal_error(msg) ;
		}
		if(Vect_open_old_head(&Map, vect[i], mapset) == 2)
		{
			if(first)
			{
				first = 0;
				window.east = Map.plus.box.E;
				window.west = Map.plus.box.W;
				window.south = Map.plus.box.S;
				window.north = Map.plus.box.N;
			}
			else
			{
				if(window.east < Map.plus.box.E)
					window.east = Map.plus.box.E;
				if(window.west > Map.plus.box.W)
					window.west = Map.plus.box.W;
				if(window.south > Map.plus.box.S)
					window.south = Map.plus.box.S;
				if(window.north < Map.plus.box.N)
					window.north = Map.plus.box.N;
			}
			Vect_close(&Map);
		}
	}

	if(window.north == window.south)
	{
		window.north += 0.5 * temp_window.ns_res;
		window.south -= 0.5 * temp_window.ns_res;
	}
	if(window.east == window.west)
	{
		window.east += 0.5 * temp_window.ew_res;
		window.west -= 0.5 * temp_window.ew_res;
	}

	G_align_window(&window, &temp_window);
    }

    if (site)
    {
	FILE *fp;
	Site *s;
	int rtype, ndim, nstr, ndec;

	G_copy(&temp_window, &window, sizeof(window));

	for(i=0; i<nsites; i++){
    		mapset = G_find_sites2 (site[i], "");
    		if (mapset == NULL)
    		{
			char msg[256];
			sprintf(msg,"Site file [%s] not available", site[i]);
			G_fatal_error(msg) ;
		}
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
					window.east = s->east;
					window.west = s->east;
					window.south = s->north;
					window.north = s->north;
				}
				else
				{
					if(window.east < s->east)
						window.east = s->east;
					if(window.west > s->east)
						window.west = s->east;
					if(window.south > s->north)
						window.south = s->north;
					if(window.north < s->north)
						window.north = s->north;
				}
			}

			/* is 100 enough to contain one point from
			 * boundary?
			east += 100;
			west -= 100;
			south -= 100;
			north += 100;
			 */

			G_free(s);
			fclose(fp);
		}
	}

	if(window.north == window.south)
	{
		window.north += 0.5 * temp_window.ns_res;
		window.south -= 0.5 * temp_window.ns_res;
	}
	if(window.east == window.west)
	{
		window.east += 0.5 * temp_window.ew_res;
		window.west -= 0.5 * temp_window.ew_res;
	}

	G_align_window(&window, &temp_window);
    }

    G_adjust_Cell_head3(&window, 0, 0, 0);
    G_put_window(&window);

    exit(0);
}
