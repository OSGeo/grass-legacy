/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
**
**  New parser installed, 12/90 David Stigberg
*/
#include    <stdio.h>
#include    "gis.h"
#include    "Vect.h"
/*#include    "dig_head.h" */


#define MAIN
#define  USAGE  "Vprune dig=input dig_out=output thresh=value[i]\n"

long ftell ();
double atof ();
double threshold;
int inches;


/*
#define DEBUG
*/
/*  command line args */
static	char  *dig_name = NULL ;
static	char  *out_name = NULL ;

static  int  load_args() ;

#ifdef OLDPARSE
struct Command_keys vars[] = {
	{ "dig_in", 1 },
	{ "dig_out", 2 },
	{ "threshold", 3 },
	{ "input", 1 },
	{ "output", 2 },
	{ "thresh", 3 },
	{ NULL,     0 }
};

#endif /*OLDPARSE*/

double dig_unit_conversion ();
static	int   snapped = 0 ;

main (argc, argv)
int argc;
char **argv;
{
	int   ret ;
	char *mapset;
	char errmsg[100];

	struct Option *old, *new, *thresh;
	struct Flag *inch_flag;

	G_gisinit(argv[0]);

	inch_flag = G_define_flag();
	inch_flag->key			= 'i';
	inch_flag->description	= "set threshold value in inches";

	old = G_define_option();
	old->key 			= "input";
	old->type 			= TYPE_STRING;
	old->required 		= YES;
	old->multiple 		= NO;
	old->description 	= "vector file to be pruned";

	new = G_define_option();
	new->key 			= "output";
	new->type 			= TYPE_STRING;
	new->required 		= YES;
	new->multiple 		= NO;
	new->description 	= "new pruned vector file";

	thresh = G_define_option();
	thresh->key 		= "thresh";
	thresh->key_desc 	= "value";
	thresh->type 		= TYPE_DOUBLE;
	thresh->required 	= YES;
	thresh->multiple 	= NO;
	thresh->description = "threshold value";

	threshold = 0.0;

	if (G_parser (argc, argv))
		exit(-1);

	/*unnecessary, but done to preserve old names*/
	dig_name = G_store(old->answer) ;
	out_name = G_store(new->answer) ;
	threshold = atof (thresh->answer) ;


	/*  check args and set flags  */

	if (dig_name == NULL  ||   out_name == NULL)
	{
		fprintf (stderr, "%s: Command line error.\n\n", argv[0]);
		G_usage();
		exit (-1);
	}
	if (!*dig_name  || !*out_name)
	{
		fprintf (stderr, "%s: Command line error: missing input or output name.\n\n", argv[0]);
		G_usage();
		exit (-1);
	}

	if (threshold == 0.0)
	{
		fprintf (stderr, "%s: Command line error: missing or improper threshold value.\n\n", argv[0]);
		G_usage();
		exit (-1);
	}

	inches = inch_flag->answer;

	/*DEBUG*/
	/*
	fprintf (stderr, "in = '%s' out = '%s'\n", dig_name, out_name);
	fprintf (stderr, "thresh = %lf  inches = %d\n", threshold, inches);
	exit(0);
	*/
#ifdef OLDPARSE
	ret = G_parse_command (argc, argv, vars, load_args) ;
	if (ret > 0)	/* Help was requested */
		exit (1);

	if (ret < 0  ||  dig_name == NULL  ||   out_name == NULL)
	{
		fprintf (stderr, "%s: Command line error.\n\n Usage: %s\n",
		    argv[0], USAGE);
		exit (-1);
	}

	if (threshold == 0.0)
	{
		fprintf (stderr, "%s: Command line error.\n\n Usage: %s\n",
		    argv[0], USAGE);
		exit (-1);
	}

#endif /*OLDPARSE*/

	/* Show advertising */
	printf("\n\n   Prune:   threshold = %lf\n\n", threshold) ;

	if ((mapset = G_find_vector2 (dig_name, "")) == NULL)
	{
		sprintf (errmsg, "Could not find DIG file %s\n", dig_name);
		G_fatal_error (errmsg);
		/*
		G_fatal_error ("Could not find DIG file %s\n", dig_name);
		*/
	}
	export (dig_name, mapset, out_name);
	exit (0);
}

#ifdef OLDPARSE
static
load_args (position, str)
int position;
char *str;
{
	switch(position)
	{
	case 1:
		dig_name = G_store(str) ;
		break ;
	case 2:
		out_name = G_store(str) ;
		break ;
	case 3:
		threshold = atof (str) ;
		if (str[strlen(str)-1] == 'i')
			inches = 1;
		else 
			inches = 0;
		break ;
	default:
		break;
	}	/*  switch  */

	return (0);
}
#endif /*OLDPARSE*/

#ifdef DEBUG
debugf (format, a, b, c, d, e, f, g, h, i, j, k, l)
char *format;
int a, b, c, d, e, f, g, h, i, j, k, l;
{
	fprintf (stderr, format, a, b, c, d, e, f, g, h, i, j, k, l);
}
#endif


struct Map_info Map;
/*ORIGINAL struct head Head; */
struct dig_head d_head;

export(dig_name, mapset, out_name)
char *dig_name, *mapset, *out_name;
{
	FILE *Out;
	FILE *In;
	char buf[1024];
	char errmsg[200];
	struct Map_info InMap, OutMap;

	if ( ! mapset)
	{
		G_fatal_error ("No mapset specified.\n");
	}

	/****if (NULL == (In = G_fopen_vector_old (dig_name, mapset)))
	{
	    fprintf (stderr, "Cannot open input file.\n");
	    exit (-1);
	}
	if (dig_init (In) < 0)
	{
	    fprintf (stderr, "Cannot initialize input vector file.\n");
	    exit (-1);
	}
	****/
	/*Out = G_fopen_vector_new (out_name); */
	/*dig_read_head_binary (In, &d_head);*/
	/*dig_write_head_binary (Out, &d_head); */

	if (0 > Vect_open_old (&InMap, dig_name, mapset))
	{
		sprintf(errmsg, "Not able to open vector file <%s>\n", dig_name) ;
		G_fatal_error (errmsg);
	}

	if (0 > Vect_open_new (&OutMap, out_name))
	{
		sprintf(errmsg, "Not able to open vector file <%s>\n", out_name) ;
		G_fatal_error (errmsg);
	}

	Vect_copy_head_data (&InMap.head, &OutMap.head);

	/* fix up threshold value to correspond to digit */
	if (inches)
	{
		threshold = threshold * dig_unit_conversion() * InMap.head.orig_scale;
	}
	/* in either case, take this resultant map value and div by 2 */
	threshold = threshold / 2.;

	doit (&InMap, &OutMap);

	/**
	fclose (Out);
	dig_fini (In);
	fclose (In);
	**/
	Vect_close (&OutMap);
	Vect_close (&InMap);

	fprintf (stderr, "\n\nCopying Attribute file\n");

	if (NULL == (In = G_fopen_old ("dig_att", dig_name, mapset)))
	{
		fprintf (stderr, "Cannot find attribute file.\n");
		exit (1);
	}
	Out = G_fopen_new ("dig_att", out_name);

	while (NULL != fgets (buf, sizeof (buf), In))
		fputs (buf, Out);

	fclose (Out);
	fclose (In);

	fprintf (stderr, "Done.\n");


	return(0) ;
}

doit (InMap, OutMap)
struct Map_info *InMap, *OutMap;
{
	struct line_pnts *Points;
	register int line, type;
	int binary;
	long offset;
	int diff;
	int left;
	int old, new;

	/*Points.alloc_points = 0; */

	/* Must always use this to create an initialized  line_pnts structure */
	Points = Vect_new_line_struct ();

	/*DEBUG*/ fprintf (stderr, "Resultant threshold = %lf\n", threshold);
	left = diff = 0;
	line = 0;
	while (1)
	{
		line++;
		if (line % 10 == 0)
			fprintf (stderr, "Pruning line %5d  pruned: %d  left; %d\r", line, diff, left);
		/*offset = ftell (in);*/

		if (0 > (type = Vect_read_next_line (InMap, Points)))
		{
			if (type == -1)
			{
				fprintf (stderr, "Out of memory on line %d\n", line);
				return (-1);
			}
			else /* EOF */
			{
				Vect_destroy_line_struct (Points);
				return (0);
			}
		}
		old = Points->n_points;
		Points->n_points = dig_prune (Points, threshold);
		diff += old - Points->n_points;
		left += Points->n_points;
		Vect_write_line (OutMap, type, Points);
	}
}

