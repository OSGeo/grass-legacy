#include <string.h>
#include <unistd.h>
#include <math.h>
#include "display.h"
#include "vask.h"
#include "D.h"
#include "gis.h"
#include "options.h"
#include "l_proto.h"

static char g_erase[16] ;
static char linesonly[2] ;
static char donull[2] ;
static char doaverage[2] ;
static char linecolor[16] ;
static char boxcolor[16] ;
static double resolution ;

int 
get_inputs (int *do_it, char *g_erase_color)
{
	static int here_already = 0 ;
	static char go[2] ;
	char ebuf[30], wbuf[30], sbuf[30], nbuf[30];

	if(! here_already)
	{
		get_defaults() ;
		strcpy(go, "Y") ;
		here_already = 1 ;
	}

V_clear() ;
V_line ( 1,"--------------------------------------------------------------------------") ;
V_line ( 2,"            VIEWING REGION              | RUN? Y/N") ;
V_line ( 3,"            N:                          | Erase Color") ;
V_line ( 4,"   W:           ---|--- E:              | Vertical Exaggerat.") ;
V_line ( 5,"            S:                          | Field of View (deg)") ;
V_line ( 6,"                                        | Lines Only? Y/N") ;
V_line ( 7,"           VIEW COORDINATES:            | Line Color") ;
V_line ( 8,"Eye Position              Center of view| Line Frequency") ;
V_line ( 9,"           <- Northing (y) ->           | Resolution") ;
V_line (10,"           <- Easting  (x) ->           | Plot null elev? Y/N") ;
V_line (11,"           <- Height   (z) ->           | Box color");
V_line (12,"                                        | Average elevs? Y/N") ;
V_line (13,"--------------------------------------------------------------------------") ;
V_line (14,"Eye -----                          | Colors: red orange yellow green blue") ;
V_line (15," \\                    N            |   indigo violet brown gray white black") ;
V_line (16,"  \\     /MAP----------/            |") ;
V_line (17,"   \\   /      X      /             | Special 'colors':") ;
V_line (18,"     W/_____________/E             |   'None' available for 'Erase Color'") ;
V_line (19,"                   S               |   'color' available for 'Line Color'") ;
V_line (20,"--------------------------------------------------------------------------") ;

	G_format_northing (window.north, nbuf, window.proj);
	G_format_northing (window.south, sbuf, window.proj);
	G_format_easting (window.east, ebuf, window.proj);
	G_format_easting (window.west, wbuf, window.proj);
	/*V_float_accuracy(3) ;*/
	V_const( sbuf           ,   's',  5, 15, 10) ;
	V_const( nbuf           ,   's',  3, 15, 10) ;
	V_const( wbuf           ,   's',  4,  6, 10) ;
	V_const( ebuf           ,   's',  4, 27, 10) ;
	V_ques ( go             ,   's',  2, 64,  2) ;
	V_ques ( g_erase          ,   's',  3, 64,  8) ;
	V_ques ( &exag          ,   'd',  4, 64,  7) ;
	V_float_accuracy(2) ;
	V_ques ( &field         ,   'd',  5, 64,  7) ;
	V_ques ( linesonly      ,   's',  6, 64,  2) ;
	V_ques ( linecolor      ,   's',  7, 64,  8) ;
	V_ques ( &line_freq     ,   'i',  8, 64,  5) ;
	V_ques ( &resolution    ,   'd',  9, 64, 10) ;
	V_ques ( donull         ,   's', 10, 64,  2) ;
	V_ques ( boxcolor       ,   's', 11, 64,  8) ;
	V_ques ( doaverage      ,   's', 12, 64,  2) ;
	V_ques ( &from_northing ,   'd',  9,  1, 10) ;
	V_ques ( &from_easting  ,   'd', 10,  1, 10) ;
	V_ques ( &from_height   ,   'd', 11,  1, 10) ;
	V_ques ( &to_northing   ,   'd',  9, 30, 10) ;
	V_ques ( &to_easting    ,   'd', 10, 30, 10) ;
	V_ques ( &to_height     ,   'd', 11, 30, 10) ;

	V_call() ;
	window.ew_res = window.ns_res = resolution ;

	if (donull[0] == 'Y' || donull[0] == 'y')
		do_null = 1 ;
	else
		do_null = 0 ;

	if (doaverage[0] == 'Y' || doaverage[0] == 'y')
		do_average = 1 ;
	else
		do_average = 0 ;

	if (linesonly[0] == 'Y' || linesonly[0] == 'y')
		lines_only = 1 ;
	else
		lines_only = 0 ;

	if (go[0] == 'Y' || go[0] == 'y')
		*do_it = 1 ;
	else
		*do_it = 0 ;

	if (g_erase[0] == 'N' || g_erase[0] == 'n')
		g_erase_color[0] = '\0' ;
	else
		strcpy(g_erase_color, g_erase) ;

	if (! strcmp(linecolor, "color"))
		line_color = -1 ;
	else
		line_color = D_translate_color(linecolor) ;
	if (line_color == 0)
		line_color = D_translate_color("white") ;

	if (! strcmp(boxcolor, "none"))
		box_color = -1 ;
	else
		box_color = D_translate_color(boxcolor) ;
	if (box_color == 0)
		box_color = D_translate_color("grey") ;

	return(0) ;
}

int get_defaults (void)
{
	char name[128] ;
	char *mapset ;
	DCELL min, max ;
	struct G_3dview v;
	int ret;

	fprintf(stderr,"\n\nIf you have previously saved 3-D viewing options in this mapset\n") ;
	fprintf(stderr,"you may recover them by entering the name under which they were saved.\n\n") ;
	fprintf(stderr,"Enter nothing and hit <RETURN> to have default values calculated.\n\n") ;

	mapset = G_ask_old("Enter name of saved 3-d viewing options to be used: ",
		name, "3d.view", "3d.view") ;

	if(mapset != NULL)
	{
	    if(0 > (ret = G_get_3dview(name, mapset, &v)))
	    {
		    fprintf(stderr,"File %s in data element %s in mapset %s not readable.\n", 
			    name, "3d.view", mapset) ;
		    fprintf(stderr,"Generating default values instead\n") ;
		    sleep(1) ; 
		    goto defaults ;
	    }
	    
	    to_easting = v.from_to[1][0];
	    to_northing = v.from_to[1][1];
	    to_height = v.from_to[1][2];
	    from_easting = v.from_to[0][0];
	    from_northing = v.from_to[0][1];
	    from_height = v.from_to[0][2];
	    exag = v.exag;
	    line_freq = (int)(((double)v.mesh_freq/v.poly_freq) + 0.5); /* round */	
	    field = v.fov;
	    resolution = v.poly_freq*(v.vwin.north - v.vwin.south)/v.vwin.rows;
	    if(v.display_type == 1)
		strcpy(linesonly,"Y");
	    else
		strcpy(linesonly,"N");
	    if(v.colorgrid)
		strcpy(linecolor,"color");
	    else
		strcpy(linecolor,v.grid_col);
	    if(v.dozero)
		strcpy(donull, "Y") ;
	    else
		strcpy(donull, "N") ;
	    strcpy(boxcolor, v.other_col) ;
	    strcpy(g_erase, v.bg_col) ;
	    if(v.doavg)
		strcpy(doaverage, "Y") ;
	    else
		strcpy(doaverage, "N") ;

	    if(exag){
		to_height *= exag;
		from_height *= exag;
	    }

	    return 0;
	}

defaults:
	/* else generate defaults */

	do_null = 0 ;
	do_average = 0 ;
	get_range(&min, &max) ;
	to_easting =  (window.east  + window.west ) / 2 ;
	to_northing = (window.north + window.south) / 2 ;
	from_easting = window.west - (window.east - window.west) ;
	from_northing = window.south - (window.north - window.south) ;
	exag = 2.0 ;
        while((exag * max - exag * min) > (window.east - window.west)/5.)
	     exag /= 5.;
	to_height = (double)exag * (max + min) / 2 ;
	from_height = to_height + .5 * sqrt(
		(to_easting - from_easting)*(to_easting - from_easting) +
		(to_northing - from_northing)*(to_northing - from_northing)) ;
/*	from_height = (double)(2 * max + exag * (max - min)) ; */
	line_freq = 1 ;
	field = 30.0 ;
	for (resolution = window.ns_res; ; resolution *= 2.)
	{
		if (20. > ((window.east - window.west) / resolution) )
		break ;
	}

	strcpy(linesonly, "Y") ;
	strcpy(donull, "N") ;
	strcpy(doaverage, "N") ;
	strcpy(linecolor, "color") ;
	strcpy(boxcolor, "none") ;
	strcpy(g_erase, "black") ;

	return 0;
}

int 
save_defaults (void)
{
	char name[128] ;
	char *mapset ;
	struct G_3dview v;
	struct Cell_head w;


	fprintf(stderr,"\n\nYou can now save your viewing information for later use by this program.\n") ;
	fprintf(stderr,"Enter nothing and hit <RETURN> to not save this information.\n\n") ;

	mapset = G_ask_any("Enter name for saving 3-d viewing options: ",
		name, "3d.view", "3d.view", 1) ;

	if(mapset != NULL)
	{
	    G_get_set_window (&w);
	    G_get_3dview_defaults(&v,&w);
	    
	    strcpy(v.pgm_id,"d.3d");
	    v.from_to[1][0] = to_easting;
	    v.from_to[1][1] = to_northing;
	    v.from_to[1][2] = exag? to_height/exag: to_height;
	    v.from_to[0][0] = from_easting;
	    v.from_to[0][1] = from_northing; 
	    v.from_to[0][2] = exag? from_height/exag: from_height;
	    v.exag = exag;
	    v.fov = field;
	    v.mesh_freq = line_freq;	
	    v.poly_freq = 1;
	    v.vwin.rows = (int)((w.north - w.south)/resolution);
	    v.vwin.cols = (int)((w.east - w.west)/resolution);
	    v.vwin.ns_res = v.vwin.ew_res = resolution;

	    /* undo window adjustment done in main */
	    v.vwin.rows -= 2 ;
	    v.vwin.cols -= 2 ;
	    v.vwin.north -= v.vwin.ns_res ;
	    v.vwin.south += v.vwin.ns_res ;
	    v.vwin.east  -= v.vwin.ew_res ;
	    v.vwin.west  += v.vwin.ew_res ;

	    v.display_type = lines_only? 1: 3;
	    v.dozero = do_null;
	    if(!strcmp(linecolor,"color"))
		v.colorgrid = 1;
	    else
		v.colorgrid = 0;

	    strcpy(v.grid_col, linecolor) ;
	    strcpy(v.other_col, boxcolor) ;
	    strcpy(v.bg_col, g_erase) ;
	    if(!strcmp(doaverage, "Y"))
		v.doavg = 1;
	    else
		v.doavg = 0;

	    G_put_3dview(name, mapset, &v, NULL);

	}

	return 0;
}
