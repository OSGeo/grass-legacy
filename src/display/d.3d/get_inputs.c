#include "options.h"
#include "gis.h"
#include "math.h"

static char erase[16] ;
static char linesonly[2] ;
static char dozero[2] ;
static char doaverage[2] ;
static char linecolor[16] ;
static char boxcolor[16] ;
static double resolution ;

get_inputs(do_it, erase_color)
	int *do_it ;
	char *erase_color ;
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
V_line (10,"           <- Easting  (x) ->           | Plot zero elev? Y/N") ;
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
	V_ques ( erase          ,   's',  3, 64,  8) ;
	V_ques ( &exag          ,   'd',  4, 64,  7) ;
	V_float_accuracy(2) ;
	V_ques ( &field         ,   'd',  5, 64,  7) ;
	V_ques ( linesonly      ,   's',  6, 64,  2) ;
	V_ques ( linecolor      ,   's',  7, 64,  8) ;
	V_ques ( &line_freq     ,   'i',  8, 64,  5) ;
	V_ques ( &resolution    ,   'd',  9, 64, 10) ;
	V_ques ( dozero         ,   's', 10, 64,  2) ;
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

	if (dozero[0] == 'Y' || dozero[0] == 'y')
		do_zero = 1 ;
	else
		do_zero = 0 ;

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

	if (erase[0] == 'N' || erase[0] == 'n')
		erase_color[0] = NULL ;
	else
		strcpy(erase_color, erase) ;

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

get_defaults()
{
	char buffer[128] ;
	char name[128] ;
	char *mapset ;
	FILE *info ;
	char *fgets() ;
	CELL min, max ;
	double sqrt() ;

	fprintf(stderr,"\n\nIf you have previously saved 3-D viewing options in this mapset\n") ;
	fprintf(stderr,"you may recover them by entering the name under which they were saved.\n\n") ;
	fprintf(stderr,"Enter nothing and hit <RETURN> to have default values calculated.\n\n") ;

	mapset = G_ask_old("Enter name of saved 3-d viewing options to be used: ",
		name, "3d.view", "3d.view") ;

	if(mapset != NULL)
	{
		info = G_fopen_old("3d.view",name,mapset) ;
		if (info == NULL)
		{
			fprintf(stderr,"File %s in data element %s in mapset %s not available.\n", 
				name, "3d.view", mapset) ;
			fprintf(stderr,"Generating default values instead\n") ;
			sleep(1) ; 
			goto defaults ;
		}

		sscanf(fgets(buffer, 80, info),"%lf",&to_easting) ;
		sscanf(fgets(buffer, 80, info),"%lf",&to_northing) ;
		sscanf(fgets(buffer, 80, info),"%lf",&to_height) ;
		sscanf(fgets(buffer, 80, info),"%lf",&from_easting) ;
		sscanf(fgets(buffer, 80, info),"%lf",&from_northing) ;
		sscanf(fgets(buffer, 80, info),"%lf",&from_height) ;
		sscanf(fgets(buffer, 80, info),"%lf",&exag) ;
		sscanf(fgets(buffer, 80, info),"%d",&line_freq) ;
		sscanf(fgets(buffer, 80, info),"%lf",&field) ;
		sscanf(fgets(buffer, 80, info),"%lf",&resolution) ;
		sscanf(fgets(buffer, 80, info),"%s",linesonly) ;
		sscanf(fgets(buffer, 80, info),"%s",dozero) ;
		sscanf(fgets(buffer, 80, info),"%s",linecolor) ;
		sscanf(fgets(buffer, 80, info),"%s",boxcolor) ;
		sscanf(fgets(buffer, 80, info),"%s",erase) ;
		sscanf(fgets(buffer, 80, info),"%s",doaverage) ;

		fclose(info) ;
		return ;
	}

defaults:
	/* else generate defaults */

	do_zero = 0 ;
	do_average = 0 ;
	get_range(&min, &max) ;
	to_easting =  (window.east  + window.west ) / 2 ;
	to_northing = (window.north + window.south) / 2 ;
	from_easting = window.west - (window.east - window.west) ;
	from_northing = window.south - (window.north - window.south) ;
	exag = 2.0 ;
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
	strcpy(dozero, "N") ;
	strcpy(doaverage, "N") ;
	strcpy(linecolor, "color") ;
	strcpy(boxcolor, "none") ;
	strcpy(erase, "black") ;
}

save_defaults()
{
	char name[128] ;
	char *mapset ;
	FILE *info ;
	char *fgets() ;

	fprintf(stderr,"\n\nYou can now save your viewing information for later use by this program.\n") ;
	fprintf(stderr,"Enter nothing and hit <RETURN> to not save this information.\n\n") ;

	mapset = G_ask_new("Enter name for saving 3-d viewing options: ",
		name, "3d.view", "3d.view") ;

	if(mapset != NULL)
	{
		info = G_fopen_new("3d.view",name,mapset) ;
		if (info == NULL)
		{
			fprintf(stderr,"File %s in data element %s in mapset %s not available.\n", 
				name, "3d.view", mapset) ;
			sleep(1) ; 
		}

		fprintf(info,"%lf\n",to_easting) ;
		fprintf(info,"%lf\n",to_northing) ;
		fprintf(info,"%lf\n",to_height) ;
		fprintf(info,"%lf\n",from_easting) ;
		fprintf(info,"%lf\n",from_northing) ;
		fprintf(info,"%lf\n",from_height) ;
		fprintf(info,"%lf\n",exag) ;
		fprintf(info,"%d\n",line_freq) ;
		fprintf(info,"%lf\n",field) ;
		fprintf(info,"%lf\n",resolution) ;
		fprintf(info,"%s\n",linesonly) ;
		fprintf(info,"%s\n",dozero) ;
		fprintf(info,"%s\n",linecolor) ;
		fprintf(info,"%s\n",boxcolor) ;
		fprintf(info,"%s\n",erase) ;
		fprintf(info,"%s\n",doaverage) ;

		fclose(info) ;
	}
}
