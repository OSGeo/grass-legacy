#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
	char buf[512];
	FILE *fd;
	long old_min, old_max;
	long new_min, new_max;
	long new_delta, old_delta;
	long value, first, prev;
	long cat;
	float divisor ;
	char *old_name;
	char *new_name;
	char *mapset;
	struct GModule *module;
	struct
	    {
		struct Option *input, *from, *output, *to, *title;
	} parm;
	struct
	    {
		struct Flag *quiet;
	} flag;

	G_gisinit (argv[0]);

	module = G_define_module();
    module->description =
		"Rescales the range of category values "
		"in a raster map layer.";
				        
	/* Define the different options */

	parm.input = G_define_option() ;
	parm.input->key        = "input";
	parm.input->type       = TYPE_STRING;
	parm.input->required   = YES;
	parm.input->gisprompt  = "old,cell,raster" ;
	parm.input->description= "The name of the raster map to be rescaled" ;

	parm.from = G_define_option() ;
	parm.from->key        = "from";
	parm.from->key_desc   = "min,max";
	parm.from->type       = TYPE_INTEGER;
	parm.from->required   = NO;
	parm.from->description= "The input data range to be rescaled (default: full range of input map)";

	parm.output = G_define_option() ;
	parm.output->key        = "output";
	parm.output->type       = TYPE_STRING;
	parm.output->required   = YES;
	parm.output->gisprompt  = "new,cell,raster" ;
	parm.output->description= "The resulting raster map name";

	parm.to = G_define_option() ;
	parm.to->key        = "to";
	parm.to->key_desc   = "min,max";
	parm.to->type       = TYPE_INTEGER;
	parm.to->required   = NO;
	parm.to->description= "The output data range";
	parm.to->answer     = "1,255";

	parm.title = G_define_option() ;
	parm.title->key        = "title";
	parm.title->key_desc   = "\"phrase\"";
	parm.title->type       = TYPE_STRING;
	parm.title->required   = NO;
	parm.title->description= "Title for new raster map" ;

	flag.quiet = G_define_flag();
	flag.quiet->key = 'q';
	flag.quiet->description = "Quietly";

	if (G_parser(argc, argv))
		exit(-1);

	old_name = parm.input->answer;
	new_name = parm.output->answer;

	mapset = G_find_cell (old_name,"");
	if (mapset == NULL)
	{
		sprintf (buf, "%s - not found\n", old_name);
		G_fatal_error (buf);
		exit(1);
	}
	if (G_legal_filename (new_name) < 0)
	{
		sprintf (buf, "%s - illegal map name\n", new_name);
		G_fatal_error (buf);
		exit(1);
	}

	if (parm.from->answer)
	{
		sscanf (parm.from->answers[0], "%ld", &old_min);
		sscanf (parm.from->answers[1], "%ld", &old_max);

	}
	else
		get_range (old_name, mapset, &old_min, &old_max, flag.quiet->answer);
	if (old_min > old_max)
	{
		value = old_min; /* swap */
		old_min = old_max;
		old_max = value;
	}

	sscanf (parm.to->answers[0], "%ld", &new_min);
	sscanf (parm.to->answers[1], "%ld", &new_max);
	if (new_min > new_max)
	{
		value = new_min; /* swap */
		new_min = new_max;
		new_max = value;
	}
	
	if (!flag.quiet->answer)
		fprintf (stderr, "Rescale %s[%ld,%ld] to %s[%ld,%ld]\n",
		    old_name, old_min, old_max, new_name, new_min, new_max);

	sprintf (buf, "r.reclass input='%s' output='%s' title='", old_name, new_name);
	if (parm.title->answer)
		strcat (buf, parm.title->answer);
	else
	{
		strcat (buf, "rescale of ");
		strcat (buf, old_name);
	}
	strcat (buf, "'");

	fd = popen (buf, "w");
	old_delta = old_max - old_min ;
	new_delta = new_max - new_min ;
	divisor = (float)new_delta/(float)old_delta ;

	prev = new_min;
	first = old_min;
	for (cat = old_min; cat <= old_max; cat++)
	{
		value = (int)(divisor * (cat - old_min) + new_min + .5) ;
		if (value != prev)
		{
			fprintf (fd, "%ld thru %ld = %ld %ld", first, cat-1, prev, first);
			if (cat-1 != first)
				fprintf (fd, " thru %ld", cat-1);
			fprintf (fd, "\n");
			prev = value;
			first = cat;
		}
	}
	fprintf (fd, "%ld thru %ld = %ld %ld", first, cat-1, prev, first);
	if (cat-1 != first)
		fprintf (fd, " thru %ld", cat-1);
	fprintf (fd, "\n");

	pclose (fd);
	exit(0);
}
