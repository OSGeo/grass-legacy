#define GLOBAL
#include "what.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"

int main (int argc, char **argv)
{
	struct Cell_head window ;
	char temp[128] ;
	char **maps;
	int nmaps;
	int t, b, l, r ;
	int i, j;
	int width, mwidth;
	char **ptr;
	struct Flag *once, *terse, *colrow;
	struct Option *opt1, *fs;
	struct GModule *module;

	/* Initialize the GIS calls */
	G_gisinit (argv[0]) ;

	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");

	if (D_get_cell_list (&rast, &nrasts) < 0)
		rast = NULL;
	else
	{
		rast = (char **)G_realloc(rast, (nrasts+1)*sizeof(char *));
		rast[nrasts] = NULL;
	}

	R_close_driver();

	opt1 = G_define_option() ;
	opt1->key        = "map" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = NO ;
	opt1->multiple   = YES ;
	if (rast)
		opt1->answers = rast;
	opt1->gisprompt  = "old,cell,raster" ;
	opt1->description= "Name of existing raster map(s)";

	fs = G_define_option ();
	fs->key 	= "fs";
	fs->type 	= TYPE_STRING;
	fs->required	= NO;
	fs->answer	= ":";
	fs->description = "Field separator (terse mode only)";

	once = G_define_flag();
	once->key         = '1';
	once->description = "Identify just one location";

	terse = G_define_flag();
	terse->key 	   = 't';
	terse->description = "Terse output. For parsing by programs";

	colrow = G_define_flag();
	colrow->key 	    = 'c';
	colrow->description = "Print out col and row for entire map region";

	module = G_define_module();
	module->description = 
	  "Allows the user to interactively query the category contents "
	  "of multiple raster map layers at user specified locations "
	  "within the current geographic region. ";

	if (!rast)
		opt1->required = YES;

	if ((argc > 1 || !rast) && G_parser(argc, argv))
		exit(-1);

	if (opt1->answers && opt1->answers[0])
		rast = opt1->answers;

	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");

	if (D_get_cur_wind(temp))
		G_fatal_error("No current graphics window") ;

	if (D_set_cur_wind(temp))
		G_fatal_error("Current graphics window not available") ;

	/* Read in the map window associated with window */
	G_get_window(&window) ;

	if (D_check_map_window(&window))
		G_fatal_error("Setting graphics window") ;

	if (G_set_window(&window) == -1)
		G_fatal_error("Can't set current graphics window") ;

	/* Determine conversion factors */
	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting graphics window coordinates") ;
	if (D_do_conversions(&window, t, b, l, r))
		G_fatal_error("Error in calculating conversions") ;

	if (rast)
	{
		for(i=0; rast[i]; i++);
		nrasts = i;

		fd = (int *)G_malloc(nrasts*sizeof(int));
		name = (char **)G_malloc(nrasts*sizeof(char *));
		mapset = (char **)G_malloc(nrasts*sizeof(char *));
		cats = (struct Categories *)G_malloc(nrasts*sizeof(struct Categories));

		width = mwidth = 0;
		for (i=0; i<nrasts; i++)
		{
			name[i] = (char *)G_malloc(80);
			mapset[i] = (char *)G_malloc(80);

			if ((fd[i] = opencell (rast[i], name[i], mapset[i])) < 0)
			{
				char msg[256];
				sprintf(msg, "Raster file [%s] not available", rast[i]);
				G_fatal_error(msg);
			}

			j = strlen(name[i]);
			if(j > width)
				width = j;

			j = strlen(mapset[i]);
			if(j > mwidth)
				mwidth = j;

			if (G_read_cats (name[i], mapset[i], &cats[i]) < 0)
				cats[i].ncats = -1;
		}
	}

	what (once->answer, terse->answer, colrow->answer, fs->answer, width, mwidth) ;

	R_close_driver();
	exit(0);
}
