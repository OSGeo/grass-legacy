/*  r.weight 	reclassify and  add or multiply  2 or more layer files 
*/

#include <stdio.h>
#include "gis.h"
#define NULL_DEV	"/dev/null"
#define MAXFILS 6

extern char tmp_names[MAXFILS+1][50];

main (argc, argv)
	int argc;
	char *argv[];
{
	char **ptr;
	char **ptr1 ;
	char **ptr2;
	struct
	{
	    struct Option *output ;
	    struct Option *action ;
	    struct Option *title ;
	    struct Option *color ;
	} parm;
	int sub();
	int test;
	int action = 0;
	char *tmpfile;
	FILE *tmpfp;
	int num, i, stat;
	char command[256];
	char title[1024];

	struct Range range;
	struct Colors colr;
	struct History hist;
	CELL min, max;

	G_gisinit (argv[0]);

	/* Define the different options */

	parm.output = G_define_option() ;
	parm.output->key        = "output";
	parm.output->type       = TYPE_STRING;
	parm.output->required   = YES;
	parm.output->multiple   = NO;
	parm.output->gisprompt  = "new,cell,raster" ;
	parm.output->description= "Name of the output layer";

	parm.action = G_define_option() ;
	parm.action->key        = "action";
	parm.action->type       = TYPE_STRING;
	parm.action->required   = NO;
	parm.action->answer     = "add";
	parm.action->options    = "add,mult";
	parm.action->multiple    = NO;
	parm.action->description= "Mathematical operation to calculate weights" ;

	parm.color = G_define_option() ;
	parm.color->key        = "color";
	parm.color->type       = TYPE_STRING;
	parm.color->required   = NO;
	parm.color->answer     = "grey";
	parm.color->options    = "grey,wave,ramp";
	parm.color->multiple    = NO;
	parm.color->description= "Color table for the new layer";

	parm.title = G_define_option() ;
	parm.title->key        = "title";
	parm.title->type       = TYPE_STRING;
	parm.title->required   = NO;
	parm.title->multiple    = NO;
	parm.title->description= "Title for the new layer";


	if (0 > G_parser(argc, argv))
		exit(-1);


	if (G_legal_filename(parm.output->answer) != 1)
	{
		char tmpbuf[100];
		sprintf (tmpbuf, "'%s' is not a legal file name", parm.output->answer);
		G_fatal_error(tmpbuf);
		exit(-1);
	}
	action = 0;
	if (parm.action->answer)
	{
		if (!strcmp (parm.action->answer, "add"))
			action = 1;
		else if (!strcmp (parm.action->answer, "mult"))
			action = 2;
	}

	tmpfile = G_tempfile();
	if ((tmpfp = fopen (tmpfile, "w")) == NULL)
		G_fatal_error ("Can't create temp file");

	main_loop (parm.output->answer, tmpfp, action, title);

	fprintf (tmpfp, "exit $?\n");
	fclose (tmpfp);


/* if we do sh -x, we can watch it run
 * except that the output is missing all the quotes,
 * which could mislead the user
 */
	sprintf (command, "sh %s", tmpfile);
	stat = G_system (command);
	unlink (tmpfile);

/* remove temp file, but DONT remove all tmp_names[i] as some could be user cellfiles */
	for (i = 1 ; (i <= MAXFILS) && (tmp_names[i] != NULL) ; i++)
	{
		if ((sscanf (tmp_names[i], "WEIGHT.%d", &num)) == 1 && num==i)
		{
			sprintf (command, "g.remove rast='%s' > /dev/null\n", tmp_names[i]);
			system(command);
		}
	}
	if (stat) exit(stat);

	G_read_range (parm.output->answer, G_mapset (), &range);
	G_get_range_min_max (&range, &min, &max);

	if (!strcmp (parm.color->answer, "ramp"))
		G_make_ramp_colors (&colr, min, max);
	else if (!strcmp (parm.color->answer, "wave"))
		G_make_wave_colors (&colr, min, max);
	else 
		G_make_grey_scale_colors (&colr, min, max);

	G_write_colors(parm.output->answer, G_mapset (), &colr) ;
	G_free_colors (&colr);
	G_short_history (parm.output->answer, "raster", &hist);
	G_write_history (parm.output->answer, &hist);
	if (!parm.title->answer)
	    parm.title->answer = title;
	G_put_cell_title (parm.output->answer, parm.title->answer);

	return (0);
}
