#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "rule.h"
#include "gis.h"
#include "glocale.h"

int main (int argc, char *argv[])
{
    struct Categories cats;
    struct FPRange range;
    DCELL min, max;
    int fp;
    char *title;
    char buf[1024];
    RULE *rules, *tail;
    int any;
    char *old_name, *old_mapset;
    char *new_name;
	struct GModule *module;
    struct
    {
	struct Option *input, *output, *title;
    } parm;

    G_gisinit (argv[0]);

	module = G_define_module();
    module->description =
		_("Creates a new map layer whose category values "
		"are based upon the user's reclassification of categories in an "
		"existing raster map layer.");

    parm.input = G_define_option();
    parm.input->key = "input";
    parm.input->required = YES;
    parm.input->type = TYPE_STRING;
	parm.input->gisprompt  = "old,cell,raster" ;
    parm.input->description =  _("Raster map to be reclassified");

    parm.output = G_define_option();
    parm.output->key = "output";
    parm.output->required = YES;
    parm.output->type = TYPE_STRING;
	parm.output->gisprompt  = "new,cell,raster" ;
    parm.output->description =  _("Name for the resulting raster map");

    parm.title = G_define_option();
    parm.title->key = "title";
    parm.title->required = NO;
    parm.title->type = TYPE_STRING;
    parm.title->description =  _("Title for the resulting raster map");

    if (G_parser(argc, argv))
	exit(1);
    old_name = parm.input->answer;
    new_name = parm.output->answer;
    title    = parm.title->answer;

    old_mapset = G_find_cell2 (old_name, "");
    if (old_mapset == NULL)
    {
	sprintf (buf, "%s - not found", old_name);
	G_fatal_error (buf);
	exit(1);
    }
    if (G_legal_filename(new_name) < 0)
    {
	sprintf (buf, "%s - illegal name", new_name);
	G_fatal_error (buf);
	exit(1);
    }
    if (strcmp(old_name,new_name)==0 && strcmp(old_mapset,G_mapset())== 0)
    {
	G_fatal_error ("input map can NOT be the same as output map");
	exit(1);
    }

    G_init_cats (0, "", &cats);
    fp = G_raster_map_is_fp(old_name, old_mapset);
    G_read_fp_range (old_name, old_mapset, &range);
    G_get_fp_range_min_max (&range, &min, &max);
    rules = tail = NULL;
    any = 0;

    if(isatty(0))
	{ 
	  fprintf (stdout, "Enter rule(s), \"end\" when done, \"help\" if you need it\n");
	  if (fp)
	    fprintf (stdout, "fp: Data range is %.25f to %.25f\n", (double)min, (double)max);
	  else
	    fprintf (stdout, "Data range is %ld to %ld\n", (long)min, (long)max);
	}

    while (input(buf))
    {
	switch (parse (buf, &rules, &tail, &cats))
	{
	case -1:
	    if (isatty(0))
	    {
		fprintf (stderr, "illegal reclass rule.");
		fprintf (stderr, " ignored\n");
	    }
	    else
	    {
		strcat (buf, " - invalid reclass rule");
		G_fatal_error (buf);
		exit(1);
	    }
	    break;

	case 0: break;

	default: any = 1; break;
	}
    }

    if (!any)
    {
	if (isatty(0))
	    fprintf (stderr, "no rules specified. %s not created\n", new_name);
	else
	    G_fatal_error ("no rules specified");
	exit(1);
    }

    reclass (old_name, old_mapset, new_name, rules, &cats, title);

    exit(0);
}
