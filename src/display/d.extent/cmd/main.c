/*
 * d.extent: set window region from displayed maps.
 *
 * $Id$
 *
 *	Copyright (C) 2000 by the GRASS Development Team
 *	Author: Huidae Cho <hdcho@geni.knu.ac.kr>
 *		Hydro Laboratory, Kyungpook National University
 *		South Korea
 *
 *	This program is free software under the GPL (>=v2)
 *	Read the file COPYING coming with GRASS for details.
 *
 * $Log$
 * Revision 1.2  2000-11-05 13:09:39  cho
 * added -v flag
 *
 * Revision 1.1  2000/11/05 12:56:11  cho
 * added, set window region from currently displayed maps.
 *
 *
 */


#include "gis.h"
#include "Vect.h"

int 
main (int argc, char **argv)
{
/*
    struct Flag *v;
*/
    char v=0;
    int i, first=1;
    char buf[128];
    char *mapset;
    char **rast, **vect, **site;
    int nrasts, nvects, nsites;
    double east, west, south, north, nsres, ewres;
                        
/*
    v = G_define_flag();
    v->key = 'v';
    v->description = "Verbose output";
*/


    G_gisinit(argv[0]) ;

/*
    if(G_parser(argc, argv))
    {
	    exit(-1);
    }
*/
    if(argc == 2 && !strncmp(argv[1], "-v", 2))
	    v = 1;
    else if(argc != 1)
    {
	    fprintf(stderr, "\n"
			    "Usage:\n"
			    " d.extent [-v]\n"
			    "\n"
			    "Flags:\n"
			    "  -v   Verbose output\n"
			    "\n");
	    exit(-1);
    }


    R_open_driver();

    if(D_get_cell_list (&rast, &nrasts) < 0)
	rast = NULL;

    if(D_get_dig_list (&vect, &nvects) < 0)
	vect = NULL;

    if(D_get_site_list (&site, &nsites) < 0)
	site = NULL;

    R_close_driver();
                    

    if (rast)
    {
        struct Cell_head window;

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
					east = window.east;
					west = window.west;
					south = window.south;
					north = window.north;
					nsres = window.ns_res;
					ewres = window.ew_res;
	 			}
	 			else
				{
					if(window.east > east)
						east = window.east;
					if(window.west < west)
						west = window.west;
					if(window.south < south)
						south = window.south;
					if(window.north > north)
						north = window.north;
					if(window.ns_res < nsres)
						nsres = window.ns_res;
					if(window.ew_res < ewres)
						ewres = window.ew_res;
				}
			}
		}
	}
    }

    if (vect)
    {
        struct Map_info Map;

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
					east = Map.head.E;
					west = Map.head.W;
					south = Map.head.S;
					north = Map.head.N;
	 			}
	 			else
				{
					if(Map.head.E > east)
						east = Map.head.E;
					if(Map.head.W < west)
						west = Map.head.W;
					if(Map.head.S < south)
						south = Map.head.S;
					if(Map.head.N > north)
						north = Map.head.N;
				}
			}
		}
	}
    }

    if (site)
    {
	FILE *fp;
	Site *s;
	int rtype, ndim, nstr, ndec;

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
						east = s->east;
						west = s->east;
						south = s->north;
						north = s->north;
					}
					else
					{
						if(s->east > east)
							east = s->east;
						if(s->east < west)
							west = s->east;
						if(s->north < south)
							south = s->north;
						if(s->north > north)
							north = s->north;
					}
				}

				/* is 100 enough to contain one point from
				 * boundary?
				 */
				east += 100;
				west -= 100;
				south -= 100;
				north += 100;

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
	    if(east == west)
	    {
		    east += 100;
		    west -= 100;
	    }
	    if(south == north)
	    {
		    south -= 100;
		    north += 100;
	    }
    */

	    east += 0.05 * (east - west);
	    west -= 0.05 * (east - west);
	    south -= 0.05 * (north - south);
	    north += 0.05 * (north - south);
    }
#endif

    if(!rast)
    {
	    nsres = (north - south) / 512;
	    ewres = (east - west) / 512;
    }

    sprintf(buf, "g.region n=%lf s=%lf e=%lf w=%lf nsres=%lf ewres=%lf",
		    north, south, east, west, nsres, ewres);
    system(buf);

/*
    if(v->answer)
	    fprintf(stderr, "%s\n", buf);
*/
    if(v)
	    fprintf(stderr, "%s\n", buf);

    exit(0);
}

