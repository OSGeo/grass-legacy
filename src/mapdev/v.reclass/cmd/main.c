#include <string.h>
#include <unistd.h>
#include "Vect.h"
#include "rule.h"
#include "local_proto.h"

static char rcsid[]="$Header$";

int 
main (int argc, char *argv[])
{
    struct Categories cats;
    char *title;
    char buf[1024];
    RULE *rules, *tail;
    int i,dissolve=0,max_att;
    int any;
    char *old_name, *old_mapset;
    char *new_name;
	struct GModule *module;
    struct
    {
	struct Option *input, *output, *title;
    } parm;
    struct Flag *d_flag;
    struct Option *typopt;
    struct Reclass new;

    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Creates a new map layer whose category values "
		"are based upon the user's reclassification of "
		"categories in an existing vector map layer.";

    d_flag = G_define_flag();
    d_flag->key              = 'd';
    d_flag->description      = "Dissolve common boundaries (default is no) ";

    parm.input = G_define_option();
    parm.input->key = "input";
    parm.input->required = YES;
    parm.input->type = TYPE_STRING;
	parm.input->gisprompt  = "old,dig,vector" ;
    parm.input->description =  "Vector map to be reclassified";

    parm.output = G_define_option();
    parm.output->key = "output";
    parm.output->required = YES;
    parm.output->type = TYPE_STRING;
	parm.output->gisprompt  = "new,dig,vector" ;
    parm.output->description =  "Name for the resulting vector map";

    typopt = G_define_option();
    typopt->key              = "type";
    typopt->type             =  TYPE_STRING;
    typopt->required         =  YES;
    typopt->options          =  "area,line,site";
    typopt->description      =  "Select area, line, or site ";

    parm.title = G_define_option();
    parm.title->key = "title";
    parm.title->required = NO;
    parm.title->type = TYPE_STRING;
    parm.title->description =  "Title for the resulting vector map";

    if (G_parser(argc, argv))
	exit(1);
    if (d_flag->answer) dissolve = 1;
    old_name = parm.input->answer;
    new_name = parm.output->answer;
    title    = parm.title->answer;

    old_mapset = G_find_file2 ("dig", old_name, "");
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
    G_strcpy( buf, "No Data");
    G_set_cat(0, buf, &cats);
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

    reclass (old_name, old_mapset, new_name, rules, &cats, title, &new);
    if (*typopt->answer == 'a')
       {
        max_att = rclas_area(old_name, new_name, &new, dissolve);
       if ( 0 > max_att)
          {
          fprintf(stderr," Error in area re-class processing\n");
          exit(1);
          }
       }
    else
       {
       max_att = rclas_line(old_name,new_name,&new);
       if ( 0 > max_att)
          {
          fprintf(stderr," Error in line/site re-class processing\n");
          exit(1);
          }
       }


    exit(0);
}
