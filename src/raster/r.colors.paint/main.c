/*
 *
 * r.colors.paint
 *
 */
#define MAIN
#include "gis.h"
#include "stdio.h"
#include "table.h"

char *old_name;
char *new_name;
char *outdev_name;
char *colortbl;

main(argc, argv)
int argc;
char **argv;
{
	char buf[512];
	char title[200];
	char *old_mapset;
	char mapname[64];
	long old_min, old_max;
	long new_min, new_max;
	int readtest, min, max;
	struct Categories cats;
	struct Categories new_cats;
	extern int stash_away();
	struct Option *opt1, *opt2, *opt3, *opt4 ;
	struct Flag *flag1 ;

	/** INITIALIZE GIS CALLS ************************************************/

	G_gisinit(argv[0]);

	opt1               = G_define_option() ;
	opt1->key          = "input" ;
	opt1->type         = TYPE_STRING ;
	opt1->description  = "Map for which colors will be selected" ;
	opt1->required     = YES ;
	opt1->gisprompt    = "old,cell,raster" ;

	opt2               = G_define_option() ;
	opt2->key          = "output" ;
	opt2->type         = TYPE_STRING ;
	opt2->description  = "Map that will contain the results" ;
	opt2->required     = NO ;
	opt2->gisprompt    = "new,cell,raster" ;

	opt3               = G_define_option() ;
	opt3->key          = "device" ;
	opt3->type         = TYPE_STRING ;
	opt3->description  = "Output device for which colors are created" ;
	opt3->required     = YES ;
	opt3->options      = "preview,shinko635,tek4695" ;

	opt4               = G_define_option() ;
	opt4->key          = "color_table" ;
	opt4->type         = TYPE_STRING ;
	opt4->description  = "Color table from which colors are created" ;
	opt4->required     = YES ;

	flag1              = G_define_flag() ;
	flag1->key         = 'r' ;
	flag1->description = "Do a rescale on the categories for optional new map" ;

	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);

	old_name = opt1->answer ;
	new_name = opt2->answer ;
	outdev_name = opt3->answer ;
	colortbl = opt4->answer ;

	/** MAKE SURE OLD_NAME CELL FILE IS AVAILABLE ******************************/

	old_mapset = G_find_cell2(old_name, "");
	if (old_mapset == NULL)
	{
		sprintf(buf,"Cellfile <%s> not available",old_name);
		G_fatal_error(buf);
	}

	/** READ COLORTABLE FILE (RELATED FILES: READFILE.C TABLE.H)  **************/

	readtest = readfile(outdev_name,colortbl);
	if (readtest == -1)
	{
		fprintf(stderr,"Problems reading file %s, sorry\n",
		    colortbl);
		exit(0);
	}


	/** DETERMINE RANGE OF CATEGORIES OF OLD MAP. **********************/

	if (!quick_range (old_name, old_mapset, &old_min, &old_max))
	{
		old_min = old_max = 0;
		if (!slow_range (old_name, old_mapset, &old_min, &old_max))
		{
			fprintf (stderr,"\n** unable to get the range, sorry\n");
			exit(0);
		}
	}

	/** TO RESCALE OR NOT TO RESCALE, THAT IS THE QUESTION ************/
	/** IF RESCALE IS YES **********************************************/

	if (flag1->answer)
	{
		strcpy (title,G_get_cell_title (old_name, old_mapset));
		G_strip (title);
		if (*title == 0) strcpy (title, old_name);
		strcat (title, " (rescaled)");

		new_min = 1;
		new_max = clist.max_colors;

		sprintf (buf, "Grescale '%s in %s' '%s' %ld %ld %ld %ld '%s'",
		    old_name, old_mapset, new_name,
		    old_min, old_max, new_min, new_max,
		    title);
		system (buf);
		strcpy (mapname,new_name);
		min = new_min;
		max = new_max;

	}

	/** IF RESCALE IS NO ************************************************/

	else
	{
		if (clist.max_colors != (old_max - old_min + 1))
		{
			fprintf (stderr,"\n** %d categories in map <%s>\n",
			    (old_max - old_min + 1), old_name);
			fprintf (stderr,"   is not equal to %d in color table <%s>\n",
			    clist.max_colors, clist.table_name);
			exit(0);
		}
		strcpy (mapname,old_name);
		min = old_min;
		max = old_max;
	}

	/** ASSIGN THE NEW COLORTABLE TO MAP ********************************/

	assign_color(mapname,old_mapset,min,max,&clist,outdev_name);

}



