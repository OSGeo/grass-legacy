/*  %W% %G%  */
/*  Gweight 	reclassify and  add or multiply  2 or more layer files 
*/

#include "gis.h"
#define NULL_DEV	"/dev/null"

#define MAXFILS 6

#define WAVE	1
#define RAMP	2
#define GREY	3
#define RAND	4

static
struct Command_keys keys[] = {
    { "output", 1 },
    { "action", 2 },
    { "color", 3 },
    { "result", 1 },
    { "out", 1 },
    { "act", 2 },
    { "colr", 3 },
    { NULL, 0 }
};

#define ADD	1
#define MULT	2

static int stash_keys ();
static int my_help ();
static int Got_act = 0;
static int Got_result = 0;
static int Colortype = 0;
static char *Output;
extern  char tmp_names[MAXFILS+1][50];

main (argc, argv)
    int argc;
    char *argv[];
{
    char *tmpfile;
    FILE *tmpfp;
    int ret;
    int num, i;
    char command[256];

    G_gisinit (argv[0]);

    G_set_parse_command_usage (my_help);
    if ((ret = G_parse_command (argc, argv, keys, stash_keys)) < 0)
	my_help (argv[0], keys, 0), exit (ret);
    if (ret > 0)	/* help was requested */
	exit (1);

    if (!Got_result)
	my_help (argv[0], keys, 0), exit (-3);

    /* SET DEFAULTS */
    if (!Got_act)		
	Got_act = ADD;
    if (!Colortype)
	Colortype = GREY;

    tmpfile = G_tempfile();
    if ((tmpfp = fopen (tmpfile, "w")) == NULL)
	G_fatal_error ("Can't create temp file");

    main_loop (Output, tmpfp, Got_act);

    for (i = 1 ; (i <= MAXFILS) && (tmp_names[i] != NULL) ; i++)
    {
	/* DONT! remove all tmp_names[i] as some could be user cellfiles */
	if ((sscanf (tmp_names[i], "#%d", &num)) == 1)
	{
	    if (num == i)
		fprintf (tmpfp, "Gremove cell '%s' > %s\n", tmp_names[i], NULL_DEV);
	}
    }
    fclose (tmpfp);

/* if we do sh -x, we can watch it run
 * except that the output is missing all the quotes,
 * which could mislead the user
 */
    sprintf (command, "sh %s", tmpfile);
    system (command);
    unlink (tmpfile);

    {
	struct Range range;
	struct Colors colr;
	CELL min, max;

	G_read_range (Output, G_mapset (), &range);
	min = range.nmin ? range.nmin : range.pmin;
	max = range.pmax ? range.pmax : range.nmax;

	switch (Colortype) {
	    case RAMP:
		G_make_color_ramp (&colr, min, max);
		break;
	    case WAVE:
		G_make_color_wave (&colr, min, max);
		break;
	    case GREY:
	    default:
		G_make_grey_scale (&colr, min, max);
		break;
	}
	G_write_colors(Output, G_mapset (), &colr) ;
	G_free_colors (&colr);
    }


    return (0);
}

static int
stash_keys (position, key)
    int position;
    char *key;
{
    switch (position) {
	case 1:
	    if (strcmp (key, "-") == 0)
	    {
		fprintf (stderr, "No default for Output layer\n");
		return (-1);
	    }
	    Got_result = 1;
	    Output = G_store (key);
	    if (G_legal_filename (Output) != 1)
	    {
		char tmpbuf[100];

		sprintf (tmpbuf, "'%s' is not a legal file name", Output);
		G_fatal_error (tmpbuf);
	    }
	    break;
	case 2:
	    G_toucase (key);
	    if (strcmp (key, "ADD") == 0 || strcmp (key, "-") == 0)
		Got_act = ADD;
	    else 
		if (strcmp (key, "MULT") == 0  || strcmp (key, "MULTIPLY") == 0)
		    Got_act = MULT;
		else
		{
		    Got_act = 0;
		    return (-1);
		}
	    break;
	case 3:	/* color */
	    G_toucase (key);
	    if (strcmp (key, "GREY") == 0 || strcmp (key, "-") == 0)
		Colortype = GREY;
	    else if (strcmp (key, "WAVE") == 0)
		Colortype = WAVE;
	    else if (strcmp (key, "RAMP") == 0)
		Colortype = RAMP;
	    else
	    {
		fprintf (stderr, "Color type '%s' unknown\n");
		return (-1);
	    }
	    break;
	default:
	    return (-1);
	    break;
    }
    return (0);
}

static int 
my_help (prog, keys, dummy)
    char *prog;
    struct Command_keys *keys;
{
    G_parse_command_usage (prog, keys, USAGE_SHORT);
    fprintf (stderr, "\n");
    fprintf (stderr, "	Output layer must be specified.                   (no default)\n");
    fprintf (stderr, "	Action must be one of [ add | mult ]              (default: add)\n");
    fprintf (stderr, "	Color table for new layer. [ GREY | WAVE | RAMP ] (default: GREY)\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "	Input commands are read on stdin as in the following:\n");
    fprintf (stderr, "		Cell_layer1\n");
    fprintf (stderr, "		[Reclass rule 1a]\n");
    fprintf (stderr, "		[Reclass rule 1b]\n");
    fprintf (stderr, "		Cell_layer2\n");
    fprintf (stderr, "		[Reclass rule 2a]\n");
    fprintf (stderr, "		etc.\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "	Cell_layer:  cellfile  OR  \"cellfile in mapset\"\n");
    fprintf (stderr, "	Reclass rule:   e.g. 1 = 5  OR  100 thru 200 = 5\n");
}
