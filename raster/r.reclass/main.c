#include <unistd.h>
#include <string.h>
#include "rule.h"

int main (int argc, char *argv[])
{
    struct Categories cats;
    char *title;
    char buf[1024];
    RULE *rules, *tail;
    int any;
    char *old_name, *old_mapset;
    char rname[256], rmapset[256]; /* for reclass check only */
    char *new_name;
	struct GModule *module;
    struct
    {
	struct Option *input, *output, *title;
    } parm;

    G_gisinit (argv[0]);

	module = G_define_module();
    module->description =
		"Creates a new map layer whose category values "
		"are based upon the user's reclassification of categories in an "
		"existing raster map layer.";

    parm.input = G_define_option();
    parm.input->key = "input";
    parm.input->required = YES;
    parm.input->type = TYPE_STRING;
	parm.input->gisprompt  = "old,cell,raster" ;
    parm.input->description =  "Raster map to be reclassified";

    parm.output = G_define_option();
    parm.output->key = "output";
    parm.output->required = YES;
    parm.output->type = TYPE_STRING;
	parm.output->gisprompt  = "new,cell,raster" ;
    parm.output->description =  "Name for the resulting raster map";

    parm.title = G_define_option();
    parm.title->key = "title";
    parm.title->required = NO;
    parm.title->type = TYPE_STRING;
    parm.title->description =  "Title for the resulting raster map";

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

    if (G_is_reclass (old_name, G_mapset(), rname, rmapset) > 0)
    {
      sprintf(buf, "%s is a reclass of another map. Exiting.\n Use r.mapcalc to generate a copy of input map %s to work with", old_name, old_name);
	  G_fatal_error(buf);
    }


    G_init_cats (0, "", &cats);
    rules = tail = NULL;
    any = 0;

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
