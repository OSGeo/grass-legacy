/***************************************************************************
 *
 * MODULE: 	g.region (commandline)
 * AUTHOR(S):	Michael Shapiro, CERL
 *              datum added by Andreas Lange <andreas.lange@rhein-main.de>
 * PURPOSE: 	Program to manage and print the boundary definitions for the
 *              geographic region.
 * 
 * COPYRIGHT:  	(C) 2000 by the GRASS Development Team
 *
 *   	    	This program is free software under the GPL (>=v2)
 *   	    	Read the file COPYING that comes with GRASS for details.
 ****************************************************************************
 *
 */

#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "G3d.h"
#include "Vect.h"
#include "local_proto.h"
#include "projects.h"
#include "glocale.h"

static int nsew(char *,char *,char *,char *);
static void die(struct Option *);
static char *llinfo(char *,char *,int);

int main (int argc, char *argv[])
{
	int i;
	int print_flag, dist_flag;
	int set_flag;
	double x;
	struct Cell_head window, temp_window;
	char *value;
	char *name;
	char *mapset;
	char *err;
	char *G_align_window();
	int projection;
	char **rast_ptr;

	struct GModule *module;
	struct
	    {
		struct Flag
		*update,
		*print,
		*gprint,
		*lprint,
		*eprint,
		*center,
		*res_set,
		*dist_res,
		*dflt,
		*z;
	} flag;
	struct
	    {
		struct Option
		*north,*south,*east,*west,*top,*bottom,
		*res, *nsres, *ewres, *res3, *tbres,
		*save, *region, *view,
		*raster, *raster3d, *align, *zoom, *vect;
	} parm;

	G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		_("Program to manage the boundary definitions for the "
		"geographic region.");

	/* flags */

	flag.dflt = G_define_flag();
	flag.dflt->key         = 'd';
	flag.dflt->description = _("Set from default region");

	flag.gprint = G_define_flag();
	flag.gprint->key         = 'g';
	flag.gprint->description = _("Print the current region (shell script style)");

	flag.print = G_define_flag();
	flag.print->key         = 'p';
	flag.print->description = _("Print the current region");

	flag.lprint = G_define_flag();
	flag.lprint->key         = 'l';
	flag.lprint->description = _("Print the current region in lat/long");

	flag.eprint = G_define_flag();
	flag.eprint->key         = 'e';
	flag.eprint->description = _("Print the current region extent");

	flag.center = G_define_flag();
	flag.center->key         = 'c';
	flag.center->description = _("Print the current region map center coordinates");

        flag.dist_res= G_define_flag();
        flag.dist_res->key         = 'm';
        flag.dist_res->description = _("Print region resolution in meters (geodesic)");

        flag.res_set= G_define_flag();
        flag.res_set->key         = 'a';
        flag.res_set->description = _("Align region to resolution (default = align to bounds, "
	    			    "works only for 2D resolution )");

	flag.update = G_define_flag();
	flag.update->key         = 'u';
	flag.update->description = _("Do not update the current region");

	flag.z = G_define_flag();
	flag.z->key         = '3';
	flag.z->description = _("Print also 3D settings");

	/* parameters */

	parm.region = G_define_option();
	parm.region->key         = "region";
	parm.region->key_desc    = "name";
	parm.region->required    = NO;
	parm.region->multiple    = NO;
	parm.region->type        = TYPE_STRING;
	parm.region->description = _("Set current region from named region");
	parm.region->gisprompt   = "old,windows,region";

	parm.raster = G_define_option();
	parm.raster->key         = "rast";
	parm.raster->key_desc    = "name";
	parm.raster->required    = NO;
	parm.raster->multiple    = YES;
	parm.raster->type        = TYPE_STRING;
	parm.raster->description = _("Set region to match this raster map");
	parm.raster->gisprompt   = "old,cell,raster";

	parm.raster3d = G_define_option();
	parm.raster3d->key         = "rast3d";
	parm.raster3d->key_desc    = "name";
	parm.raster3d->required    = NO;
	parm.raster3d->multiple    = NO;
	parm.raster3d->type        = TYPE_STRING;
	parm.raster3d->description = _("Set region to match this 3D raster map (both 2D and 3D values)");
	parm.raster3d->gisprompt   = "old,grid3,raster 3D";

	parm.vect = G_define_option();
	parm.vect->key         = "vect";
	parm.vect->key_desc    = "name";
	parm.vect->required    = NO;
	parm.vect->multiple    = NO;
	parm.vect->type        = TYPE_STRING;
	parm.vect->description = _("Set region to match this vector map");
	parm.vect->gisprompt   = "old,vector,vector";

	parm.view = G_define_option();
	parm.view->key         = "3dview";
	parm.view->key_desc    = "name";
	parm.view->required    = NO;
	parm.view->multiple    = NO;
	parm.view->type        = TYPE_STRING;
	parm.view->description = _("Set region to match this 3dview file");

	parm.north = G_define_option();
	parm.north->key         = "n";
	parm.north->key_desc    = "value";
	parm.north->required    = NO;
	parm.north->multiple    = NO;
	parm.north->type        = TYPE_STRING;
	parm.north->description = llinfo("Value for the northern edge", G_lat_format_string(), window.proj);

	parm.south = G_define_option();
	parm.south->key         = "s";
	parm.south->key_desc    = "value";
	parm.south->required    = NO;
	parm.south->multiple    = NO;
	parm.south->type        = TYPE_STRING;
	parm.south->description = llinfo("Value for the southern edge", G_lat_format_string(), window.proj);

	parm.east = G_define_option();
	parm.east->key         = "e";
	parm.east->key_desc    = "value";
	parm.east->required    = NO;
	parm.east->multiple    = NO;
	parm.east->type        = TYPE_STRING;
	parm.east->description = llinfo("Value for the eastern edge ", G_lon_format_string(), window.proj);

	parm.west = G_define_option();
	parm.west->key         = "w";
	parm.west->key_desc    = "value";
	parm.west->required    = NO;
	parm.west->multiple    = NO;
	parm.west->type        = TYPE_STRING;
	parm.west->description = llinfo("Value for the western edge ", G_lon_format_string(), window.proj);

	parm.top = G_define_option();
	parm.top->key         = "t";
	parm.top->key_desc    = "value";
	parm.top->required    = NO;
	parm.top->multiple    = NO;
	parm.top->type        = TYPE_STRING;
	parm.top->description = _("Value for the top edge");

	parm.bottom = G_define_option();
	parm.bottom->key         = "b";
	parm.bottom->key_desc    = "value";
	parm.bottom->required    = NO;
	parm.bottom->multiple    = NO;
	parm.bottom->type        = TYPE_STRING;
	parm.bottom->description = _("Value for the bottom edge");

	parm.res = G_define_option();
	parm.res->key         = "res";
	parm.res->key_desc    = "value";
	parm.res->required    = NO;
	parm.res->multiple    = NO;
	parm.res->type        = TYPE_STRING;
	parm.res->description = _("Grid resolution 2D (both north-south and east-west)");

	parm.res3 = G_define_option();
	parm.res3->key         = "res3";
	parm.res3->key_desc    = "value";
	parm.res3->required    = NO;
	parm.res3->multiple    = NO;
	parm.res3->type        = TYPE_STRING;
	parm.res3->description = _("3D grid resolution (north-south, east-west and top-bottom)");

	parm.nsres = G_define_option();
	parm.nsres->key         = "nsres";
	parm.nsres->key_desc    = "value";
	parm.nsres->required    = NO;
	parm.nsres->multiple    = NO;
	parm.nsres->type        = TYPE_STRING;
	parm.nsres->description = llinfo("North-south grid resolution 2D ", G_llres_format_string(), window.proj);

	parm.ewres = G_define_option();
	parm.ewres->key         = "ewres";
	parm.ewres->key_desc    = "value";
	parm.ewres->required    = NO;
	parm.ewres->multiple    = NO;
	parm.ewres->type        = TYPE_STRING;
	parm.ewres->description = llinfo("East-west grid resolution 2D ", G_llres_format_string(), window.proj);

	parm.tbres = G_define_option();
	parm.tbres->key         = "tbres";
	parm.tbres->key_desc    = "value";
	parm.tbres->required    = NO;
	parm.tbres->multiple    = NO;
	parm.tbres->type        = TYPE_STRING;
	parm.tbres->description = _("Top-bottom grid resolution 3D");

	parm.zoom = G_define_option();
	parm.zoom->key         = "zoom";
	parm.zoom->key_desc    = "name";
	parm.zoom->required    = NO;
	parm.zoom->multiple    = NO;
	parm.zoom->type        = TYPE_STRING;
	parm.zoom->description = _("Raster map to zoom into");
	parm.zoom->gisprompt   = "old,cell,raster";

	parm.align = G_define_option();
	parm.align->key         = "align";
	parm.align->key_desc    = "name";
	parm.align->required    = NO;
	parm.align->multiple    = NO;
	parm.align->type        = TYPE_STRING;
	parm.align->description = _("Raster map to align to");
	parm.align->gisprompt   = "old,cell,raster";

	parm.save = G_define_option();
	parm.save->key         = "save";
	parm.save->key_desc    = "name";
	parm.save->required    = NO;
	parm.save->multiple    = NO;
	parm.save->type        = TYPE_STRING;
	parm.save->description = _("Save the current region to region definition file");
	parm.save->gisprompt   = "new,windows,region";

	if (G_parser(argc,argv))
		exit(1);

	/* get current region.
	 * if current region not valid, set it from default
	 * note: G_get_default_window() dies upon error
	 */
	if (G__get_window (&window, "", "WIND", G_mapset()) != NULL)
	{
		G_get_default_window (&window);
		G_put_window (&window);
	}
	projection = window.proj;

	set_flag = ! flag.update->answer;
	if (flag.eprint->answer)
		print_flag = 5;
	else if (flag.center->answer)
		print_flag = 4;
	else if (flag.lprint->answer)
		print_flag = 3;
	else if (flag.gprint->answer)
		print_flag = 2;
	else if (flag.print->answer)
		print_flag = 1;
	else
		print_flag = 0;

	/* Flag for reporting distance in meters */
	if (flag.dist_res->answer) {
		dist_flag = 1;
		/* Set -g default output */
		if ( print_flag == 0)
		     print_flag = 2;
	} else
		dist_flag = 0;

	if (flag.dflt->answer)
		G_get_default_window (&window);

	/* region= */
	if (name = parm.region->answer)
	{
		mapset = G_find_file ("windows", name, "");
		if (!mapset)
			G_fatal_error (_("region <%s> not found"), name);
		if (G__get_window (&window, "windows", name, mapset) != NULL)
			G_fatal_error (_("can't read region <%s> in <%s>"), name, mapset);
	}

	/* 3dview= */
	if (name = parm.view->answer)
	{
		struct G_3dview v;
		FILE *fp;
		int ret;
		
		mapset = G_find_file2 ("3d.view", name, "");
		if (!mapset)
			G_fatal_error (_("3dview file <%s> not found"), name);

		G_3dview_warning(0); /* suppress boundary mismatch warning */

		if(NULL == (fp = G_fopen_old("3d.view",name,mapset)))
		    G_fatal_error (_("can't open 3dview file <%s> in <%s>"), name, mapset);

		G_copy (&temp_window, &window, sizeof(window));

		if(0 > (ret = G_get_3dview(name, mapset, &v)))
		    G_fatal_error (_("can't read 3dview file <%s> in <%s>"), name, mapset);
		if (ret == 0)
		    G_fatal_error (_("Old 3dview file. Region not found in <%s> in <%s>"), name, mapset);

                 
		window.north = v.vwin.north;
		window.south = v.vwin.south;
		window.west  = v.vwin.west;
		window.east  = v.vwin.east;

		window.rows = v.vwin.rows;
		window.cols = v.vwin.cols;
		window.ns_res = v.vwin.ns_res;
		window.ew_res = v.vwin.ew_res;

		fclose (fp);

	}

	/* raster= */
	if (parm.raster->answer)
	{
		int first = 0;
		rast_ptr = parm.raster->answers;
		for (; *rast_ptr != NULL; rast_ptr++)
		{
			char rast_name[100];
			strcpy (rast_name, *rast_ptr);
			mapset = G_find_cell2 (rast_name, "");
			if (!mapset)
				G_fatal_error (_("raster map <%s> not found"), rast_name);
			if (G_get_cellhd (rast_name, mapset, &temp_window) < 0)
				G_fatal_error (_("can't read header for <%s> in <%s>"),
						rast_name, mapset);
			if (!first) {
				G_copy (&window, &temp_window, sizeof(window));
				first = 1;
			} else {
				window.north = (window.north > temp_window.north) ?
					window.north : temp_window.north;
				window.south = (window.south < temp_window.south) ?
					window.south : temp_window.south;
				window.east = (window.east > temp_window.east) ?
					window.east : temp_window.east;
				window.west = (window.west < temp_window.west) ?
					window.west : temp_window.west;
			}
		}
		G_adjust_Cell_head3(&window,0,0,0);
	}
				

	/* raster3d= */
	if (name = parm.raster3d->answer)
	{
	    	G3D_Region win;
		
		if( (mapset = G_find_grid3(name, "")) == NULL )
			G_fatal_error (_("3D raster map <%s> not found"), name);
		    
		if ( G3d_readRegionMap (name, mapset, &win) < 0 ) 
			G_fatal_error (_("can't read header for 3D raster <%s> in <%s>"), name, mapset);

		window.proj = win.proj;
		window.zone = win.zone;
		window.north = win.north;
		window.south = win.south;
		window.east = win.east;
		window.west = win.west;
		window.top = win.top;
		window.bottom = win.bottom;
		window.rows = win.rows;
		window.rows3 = win.rows;
		window.cols = win.cols;
		window.cols3 = win.cols;
		window.depths = win.depths;
		window.ns_res = win.ns_res;
		window.ns_res3 = win.ns_res;
		window.ew_res = win.ew_res;
		window.ew_res3 = win.ew_res;
		window.tb_res = win.tb_res;
	}

	/* vect= */
	if (name = parm.vect->answer)
	{
		struct Map_info Map;
		BOUND_BOX box;
		
		mapset = G_find_vector2 (name, "");
		if (!mapset)
			G_fatal_error (_("vector map <%s> not found"), name);

		G_copy (&temp_window, &window, sizeof(window));

		Vect_set_open_level (2);
		if (2 != Vect_open_old (&Map, name, mapset))
			G_fatal_error (_("can't open vector file <%s> in <%s>"), name, mapset);

		Vect_get_map_box (&Map, &box );
		window.north = box.N;
		window.south = box.S;
		window.west  = box.W;
		window.east  = box.E;

       	        if(window.north == window.south)
       	        {
       	              window.north = window.north + 0.5 * temp_window.ns_res;
                      window.south = window.south - 0.5 * temp_window.ns_res;
                }
                if(window.east==window.west)
                {
                      window.west = window.west - 0.5 * temp_window.ew_res;
                      window.east = window.east + 0.5 * temp_window.ew_res;
                }

		G_align_window (&window, &temp_window);

		Vect_close (&Map);
	}

	/* n= */
	if (value = parm.north->answer)
	{
		if(i = nsew(value, "n+", "n-", "s+"))
		{
			if (!G_scan_resolution (value+2, &x, window.proj))
				die(parm.north);
			switch(i)
			{
			case 1:
				window.north += x;
				break;
			case 2:
				window.north -= x;
				break;
			case 3:
				window.north = window.south + x;
				break;
			}
		}
		else if (G_scan_northing (value, &x, window.proj))
			window.north = x;
		else
			die(parm.north);
	}

	/* s= */
	if (value = parm.south->answer)
	{
		if(i = nsew(value, "s+", "s-", "n-"))
		{
			if (!G_scan_resolution (value+2, &x, window.proj))
				die(parm.south);
			switch(i)
			{
			case 1:
				window.south += x;
				break;
			case 2:
				window.south -= x;
				break;
			case 3:
				window.south = window.north - x;
				break;
			}
		}
		else if (G_scan_northing (value, &x, window.proj))
			window.south = x;
		else
			die(parm.south);
	}

	/* e= */
	if (value = parm.east->answer)
	{
		if(i = nsew(value, "e+", "e-", "w+"))
		{
			if (!G_scan_resolution (value+2, &x, window.proj))
				die(parm.east);
			switch(i)
			{
			case 1:
				window.east += x;
				break;
			case 2:
				window.east -= x;
				break;
			case 3:
				window.east = window.west + x;
				break;
			}
		}
		else if (G_scan_easting (value, &x, window.proj))
			window.east = x;
		else
			die(parm.east);
	}

	/* w= */
	if (value = parm.west->answer)
	{
		if(i = nsew(value, "w+", "w-", "e-"))
		{
			if (!G_scan_resolution (value+2, &x, window.proj))
				die(parm.west);
			switch(i)
			{
			case 1:
				window.west += x;
				break;
			case 2:
				window.west -= x;
				break;
			case 3:
				window.west = window.east - x;
				break;
			}
		}
		else if (G_scan_easting (value, &x, window.proj))
			window.west = x;
		else
			die(parm.west);
	}

	/* t= */
	if (value = parm.top->answer)
	{
		if(i = nsew(value, "t+", "t-", "b+"))
		{
			if ( sscanf (value+2, "%lf", &x) != 1 )
				die(parm.top);
			switch(i)
			{
			case 1:
				window.top += x;
				break;
			case 2:
				window.top -= x;
				break;
			case 3:
				window.top = window.bottom + x;
				break;
			}
		}
		else if ( sscanf (value, "%lf", &x) == 1 )
			window.top = x;
		else
			die(parm.top);
	}

	/* b= */
	if (value = parm.bottom->answer)
	{
		if(i = nsew(value, "b+", "b-", "t-"))
		{
			if (  sscanf (value+2, "%lf", &x) != 1 )
				die(parm.bottom);
			switch(i)
			{
			case 1:
				window.bottom += x;
				break;
			case 2:
				window.bottom -= x;
				break;
			case 3:
				window.bottom = window.top - x;
				break;
			}
		}
		else if ( sscanf (value, "%lf", &x) == 1 )
			window.bottom = x;
		else
			die(parm.bottom);
	}

	/* res= */
	if (value = parm.res->answer)
	{
		if (!G_scan_resolution (value, &x, window.proj))
			die(parm.res);
		window.ns_res = x;
		window.ew_res = x;
	
		if (flag.res_set->answer) {
			window.north =  ceil(window.north/x) * x ;
			window.south = floor(window.south/x) * x ;
			window.east = ceil(window.east/x) * x ;
			window.west = floor(window.west/x) * x ;
                }
	}

	/* res3= */
	if (value = parm.res3->answer)
	{
		if (!G_scan_resolution (value, &x, window.proj))
			die(parm.res);
		window.ns_res3 = x;
		window.ew_res3 = x;
		window.tb_res = x;
	}

	/* nsres= */
	if (value = parm.nsres->answer)
	{
		if (!G_scan_resolution (value, &x, window.proj))
			die(parm.nsres);
		window.ns_res = x;
	
		if (flag.res_set->answer) {
			window.north = 2 * x * ( (int)(window.north/2/x));
                	window.south = 2 * x * ( (int)(window.south/2/x));
		}
	}

	/* ewres= */
	if (value = parm.ewres->answer)
	{
		if (!G_scan_resolution (value, &x, window.proj))
			die(parm.ewres);
		window.ew_res = x;
		
		if (flag.res_set->answer) {
			window.east =  2 * x * ( (int)(window.east/2/x));
                	window.west =  2 * x * ( (int)(window.west/2/x));
		}
	}

	/* tbres= */
	if (value = parm.tbres->answer)
	{
		if ( sscanf (value, "%lf", &x) != 1 )
			die(parm.tbres);
		window.tb_res = x;

		if (flag.res_set->answer) {
			window.top =  2 * x * ( (int)(window.top/2/x));
                	window.bottom =  2 * x * ( (int)(window.bottom/2/x));
		}
	}

	/* zoom= */
	if (name = parm.zoom->answer)
	{
		mapset = G_find_cell2 (name, "");
		if (!mapset)
			G_fatal_error (_("raster map <%s> not found"), name);
		zoom (&window, name, mapset);
	}

	/* align= */
	if (name = parm.align->answer)
	{
		mapset = G_find_cell2 (name, "");
		if (!mapset)
			G_fatal_error (_("raster map <%s> not found"), name);
		if (G_get_cellhd (name, mapset, &temp_window) < 0)
			G_fatal_error (_("can't read header for <%s> in <%s>"),
			    name, mapset);
		if (err = G_align_window (&window, &temp_window))
			G_fatal_error (_("%s in %s: %s"), name, mapset, err);
	}

	/* save= */
	if (name = parm.save->answer)
	{
		if (G_legal_filename (name) < 0)
			G_fatal_error (_("<%s> - illegal region name"), name);
		G_copy (&temp_window, &window, sizeof(window));
		adjust_window (&temp_window);
		if (G__put_window (&temp_window, "windows", name) < 0)
			G_fatal_error (_("can't write region <%s>"), name);
	}

	adjust_window (&window);
	if (set_flag)
	{
		if (G_put_window (&window) < 0)
			G_fatal_error (_("unable to update current region"));
	}
	if (print_flag)
	{
		print_window (&window, print_flag, dist_flag, flag.z->answer);
	}

	exit(0);
}

static void die(struct Option *parm)
{
    /*
    G_usage();
    */
    G_fatal_error (_("<%s=%s> ** invalid input **"), parm->key, parm->answer);
}

static int nsew(char *value,char *a,char *b,char *c)
{
	if (strncmp (value, a, strlen(a)) == 0 ) return 1;
	if (strncmp (value, b, strlen(b)) == 0 ) return 2;
	if (strncmp (value, c, strlen(c)) == 0 ) return 3;
	return 0;
}

static char *llinfo(char *msg,char *llformat,int proj)
{
	char buf[256];
	if (proj != PROJECTION_LL)
		return msg;

	sprintf (buf, "%s (format %s)", msg, llformat);
	return G_store(buf);
}
