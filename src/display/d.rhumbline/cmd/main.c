#include "gis.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
	int line_color;
	int text_color = 0;
	int use_mouse;
	double lon1,lat1,lon2,lat2;
	char msg[100];
	struct GModule *module;
	struct
	    {
		struct Option *lcolor, *tcolor, *coor;
	} parm;

	G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Displays the rhumbline joining two user-specified "
		"points, in the active frame on the user's graphics monitor.";

	parm.coor = G_define_option() ;
	parm.coor->key        = "coor" ;
	parm.coor->key_desc   = "lon1,lat1,lon2,lat2";
	parm.coor->type       = TYPE_STRING ;
	parm.coor->required   = NO ;
	parm.coor->description= "Starting and ending coordinates" ;

	parm.lcolor = G_define_option() ;
	parm.lcolor->key        = "lcolor" ;
	parm.lcolor->type       = TYPE_STRING ;
	parm.lcolor->required   = NO ;
	parm.lcolor->description= "Line color" ;
	parm.lcolor->options    = D_color_list();
	parm.lcolor->answer     = "white";

#ifdef CAN_DO_DISTANCES
	parm.tcolor = G_define_option() ;
	parm.tcolor->key        = "tcolor" ;
	parm.tcolor->type       = TYPE_STRING ;
	parm.tcolor->required   = NO ;
	parm.tcolor->description= "Text color" ;
	parm.tcolor->options    = D_color_list();
#endif

	if (argc > 1 && G_parser(argc, argv))
		exit(-1);

	if (G_projection() != PROJECTION_LL)
	{
		sprintf (msg, "%s: database is not a %s database\n",
		    argv[0], G__projection_name(PROJECTION_LL));
		G_fatal_error (msg);
		exit(1);
	}

	use_mouse = 1;
	if (parm.coor->answer)
	{
		if (!G_scan_easting (parm.coor->answers[0], &lon1, G_projection()))
		{
			fprintf (stderr, "%s - illegal longitude\n", parm.coor->answers[0]);
			G_usage();
			exit(-1);
		}
		if (!G_scan_northing (parm.coor->answers[1], &lat1, G_projection()))
		{
			fprintf (stderr, "%s - illegal longitude\n", parm.coor->answers[1]);
			G_usage();
			exit(-1);
		}
		if (!G_scan_easting (parm.coor->answers[2], &lon2, G_projection()))
		{
			fprintf (stderr, "%s - illegal longitude\n", parm.coor->answers[2]);
			G_usage();
			exit(-1);
		}
		if (!G_scan_northing (parm.coor->answers[3], &lat2, G_projection()))
		{
			fprintf (stderr, "%s - illegal longitude\n", parm.coor->answers[3]);
			G_usage();
			exit(-1);
		}
		use_mouse = 0;
	}

	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");

	line_color = D_translate_color (parm.lcolor->answer);
	if (!line_color)
		line_color = D_translate_color (parm.lcolor->answer = "white");

#ifdef CAN_DO_DISTANCES
	if(strcmp (parm.lcolor->answer, "white") == 0)
		deftcolor = "red";
	else
		deftcolor = "white";

	if (parm.tcolor->answer == NULL)
		parm.tcolor->answer = deftcolor;
	text_color = D_translate_color (parm.tcolor->answer);
	if (!text_color)
		text_color = D_translate_color (deftcolor);
#endif

	setup_plot();
	if (use_mouse)
		mouse (line_color, text_color);
	else
	{
		plot (lon1, lat1, lon2, lat2, line_color, text_color);
		D_add_to_list(G_recreate_command()) ;
	}

	R_close_driver();
	exit(0);
}

