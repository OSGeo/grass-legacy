/* %W% %G% */
/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/
#include    <stdio.h>
#include    "gis.h"
#include    "digit.h"
#include    "dig_head.h"


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

struct Command_keys vars[] = {
 { "dig_in", 1 },
 { "dig_out", 2 },
 { "threshold", 3 },
 { "input", 1 },
 { "output", 2 },
 { "thresh", 3 },
 { NULL,     0 }
};

double dig_unit_conversion ();
static	int   snapped = 0 ;

main (argc, argv)
    int argc;
    char **argv;
{
    int   ret ;
    char *mapset;


/*  check args and set flags  */
	
    threshold = 0.0;
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

/* Show advertising */
    G_gisinit(argv[0]);
    printf("\n\n   Prune:   threshold = %lf\n\n", threshold) ;

    if ((mapset = G_find_file2 ("dig", dig_name, "")) == NULL)
	G_fatal_error ("Could not find DIG file %s\n", dig_name);
    
    export (dig_name, mapset, out_name); 
    exit (0);
}

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

#ifdef DEBUG
debugf (format, a, b, c, d, e, f, g, h, i, j, k, l)
    char *format;
    int a, b, c, d, e, f, g, h, i, j, k, l;
{
    fprintf (stderr, format, a, b, c, d, e, f, g, h, i, j, k, l);
}
#endif


struct Map_info Map;
struct head Head;

export(dig_name, mapset, out_name)
    char *dig_name, *mapset, *out_name;
{
	FILE *Out;
	FILE *In;
	char buf[1024];

	if ( ! mapset)
	{
		G_fatal_error ("No mapset specified.\n");
	}

	if (NULL == (In = G_fopen_old ("dig", dig_name, mapset)))
	{
	    fprintf (stderr, "Cannot open input file.\n");
	    exit (-1);
	}

	Out = G_fopen_new ("dig", out_name);

	dig_read_head_binary (In, &Head);
	dig_write_head_binary (Out, &Head);

	/* fix up threshold value to correspond to digit */
	if (inches)
	{
	    threshold = threshold * dig_unit_conversion() * Head.orig_scale;
	}
	/* in either case, take this resultant map value and div by 2 */
	threshold = threshold / 2.;

	doit (In, Out);

	fclose (Out);
	fclose (In);

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

doit (in, out)
    FILE *in;
    FILE *out;
{
    struct line_pnts Points;
    register int line, type;
    int binary;
    long offset;
    int diff;
    int left;
    int old, new; 
    
    Points.alloc_points = 0;

/*DEBUG*/ fprintf (stderr, "Resultant threshold = %lf\n", threshold);
    left = diff = 0;
    line = 0;
    while (1)
    {
	line++;
	if (line % 10 == 0)
	    fprintf (stderr, "Pruning line %5d  pruned: %d  left; %d\r", line, diff, left);
	offset = ftell (in);
	if (0 > (type = dig__Read_line (&Points, in, offset)))
	{
	    if (type == -1)
	    {
		fprintf (stderr, "Out of memory on line %d\n", line);
		return (-1);
	    }
	    else 	/* EOF */
		return (0);
	}
	old = Points.n_points;
	Points.n_points = dig_prune (&Points, threshold);
	diff += old - Points.n_points;
	left += Points.n_points;
	dig__Write_line (out, type, &Points);
    }
}
