#include "options.h"
#include "gis.h"

static char erase[16] ;
static char linesonly[2] ;
static char dozero[2] ;
static char linecolor[16] ;
static double resolution ;

get_inputs(do_it, erase_color)
int *do_it, *erase_color ;
{
	static int here_already = 0 ;
	static char go[2] ;

	if(! here_already)
	{
		get_defaults() ;
		strcpy(go, "Y") ;
		here_already = 1 ;
	}

V_clear() ;
V_line ( 1,"--------------------------------------------------------------------------") ;
V_line ( 2,"            VIEWING WINDOW              |  VIEWING OPTIONS") ;
V_line ( 3,"            N:                          |") ;
V_line ( 4,"   W:           ---|--- E:              | RUN? Y/N") ;
V_line ( 5,"            S:                          | Erase Color") ;
V_line ( 6,"                                        | Vertical Exaggerat.") ;
V_line ( 7,"           VIEW COORDINATES:            | Field of View (deg)") ;
V_line ( 8,"Eye Position              Center of view| Lines Only? Y/N") ;
V_line ( 9,"           <- Northing (y) ->           | Line Color") ;
V_line (10,"           <- Easting  (x) ->           | Line Frequency") ;
V_line (11,"           <- Height   (z) ->           | Resolution") ;
V_line (12,"                                        | Plot zero elev? Y/N") ;
V_line (13,"--------------------------------------------------------------------------") ;
V_line (14,"Eye -----                          | Colors: red orange yellow green blue") ;
V_line (15," \\                                 |         indigo violet gray white black") ;
V_line (16,"  \\     /MAP----------/N           |") ;
V_line (17,"   \\   /      X      /             | Special 'colors':") ;
V_line (18,"      /_____________/S             |   'None' available for 'Erase Color'") ;
V_line (19,"     W             E               |   'color' available for 'Line Color'") ;
V_line (20,"--------------------------------------------------------------------------") ;

	V_ques ( go             ,   's',  4, 64,  2) ;
	V_const( &(window.south),   'd',  5, 15, 10) ;
	V_const( &(window.north),   'd',  3, 15, 10) ;
	V_const( &(window.west ),   'd',  4,  6, 10) ;
	V_const( &(window.east ),   'd',  4, 27, 10) ;
	V_ques ( &from_northing ,   'd',  9,  1, 10) ;
	V_ques ( &from_easting  ,   'd', 10,  1, 10) ;
	V_ques ( &from_height   ,   'd', 11,  1, 10) ;
	V_ques ( &to_northing   ,   'd',  9, 30, 10) ;
	V_ques ( &to_easting    ,   'd', 10, 30, 10) ;
	V_ques ( &to_height     ,   'd', 11, 30, 10) ;
	V_ques ( erase          ,   's',  5, 64,  8) ;
	V_float_accuracy(3) ;
	V_ques ( &exag          ,   'd',  6, 64,  7) ;
	V_float_accuracy(2) ;
	V_ques ( &field         ,   'd',  7, 64,  7) ;
	V_ques ( linesonly      ,   's',  8, 64,  2) ;
	V_ques ( linecolor      ,   's',  9, 64,  8) ;
	V_ques ( &line_freq     ,   'i', 10, 64,  5) ;
	V_ques ( &resolution    ,   'd', 11, 64, 10) ;
	V_ques ( dozero         ,   's', 12, 64,  2) ;

	V_call() ;
	window.ew_res = window.ns_res = resolution ;

	if (dozero[0] == 'Y' || dozero[0] == 'y')
		do_zero = 1 ;
	else
		do_zero = 0 ;

	if (linesonly[0] == 'Y' || linesonly[0] == 'y')
		lines_only = 1 ;
	else
		lines_only = 0 ;

	if (go[0] == 'Y' || go[0] == 'y')
		*do_it = 1 ;
	else
		*do_it = 0 ;

	if (erase[0] == 'N' || erase[0] == 'n')
		*erase_color = -1 ;
	else
		*erase_color = D_translate_color(erase) ;

	if (! strcmp(linecolor, "color"))
		line_color = -1 ;
	else
		line_color = D_translate_color(linecolor) ;
	if (line_color == 0)
		line_color = D_translate_color("white") ;

	return(0) ;
}

get_defaults()
{
	char buffer[128] ;
	char name[128] ;
	char *mapset ;
	FILE *info ;
	char *fgets() ;

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
		sscanf(fgets(buffer, 80, info),"%lf",&line_freq) ;
		sscanf(fgets(buffer, 80, info),"%lf",&field) ;
		sscanf(fgets(buffer, 80, info),"%lf",&resolution) ;
		sscanf(fgets(buffer, 80, info),"%s",linesonly) ;
		sscanf(fgets(buffer, 80, info),"%s",dozero) ;
		sscanf(fgets(buffer, 80, info),"%s",linecolor) ;
		sscanf(fgets(buffer, 80, info),"%s",erase) ;

		fclose(info) ;
		return ;
	}

defaults:
	/* else generate defaults */
	to_easting =  (window.east  + window.west ) / 2 ;
	to_northing = (window.north + window.south) / 2 ;
	to_height = 0 ;
	from_easting = window.west - (window.east - window.west) ;
	from_northing = window.south - (window.north - window.south) ;
	from_height = 20000 ;
	exag = 2.0 ;
	line_freq = 10 ;
	field = 30.0 ;
	resolution = window.ns_res ;

	strcpy(linesonly, "N") ;
	strcpy(dozero, "Y") ;
	strcpy(linecolor, "white") ;
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
		fprintf(info,"%lf\n",line_freq) ;
		fprintf(info,"%lf\n",field) ;
		fprintf(info,"%lf\n",resolution) ;
		fprintf(info,"%s\n",linesonly) ;
		fprintf(info,"%s\n",dozero) ;
		fprintf(info,"%s\n",linecolor) ;
		fprintf(info,"%s\n",erase) ;

		fclose(info) ;
	}
}
