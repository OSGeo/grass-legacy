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
#define  USAGE  "Vmake.site dig=input site=output\n"

/*
#define DEBUG
*/
/*  command line args */
static	char  *dig_name = NULL ;
static	char  *site_name = NULL ;

static  int  load_args() ;

struct Command_keys vars[] = {
 { "dig", 1 },
 { "site", 2 },
 { "input", 1 },
 { "output", 2 },
 { NULL,     0 }
};

double dig_unit_conversion ();
static	int   snapped = 0 ;

main(argc, argv)
    int argc;
    char **argv;
{
    int   ret ;
    char *mapset;


/*  check args and set flags  */
	
    ret = G_parse_command (argc, argv, vars, load_args) ;
    if (ret > 0)	/* Help was requested */
         exit (1);

    if (ret < 0  ||  dig_name == NULL  ||   site_name == NULL)
    {
        fprintf (stderr, "%s: Command line error.\n\n Usage: %s\n",
		argv[0], USAGE);
        exit (-1);
    }

/* Show advertising */
    G_gisinit(argv[0]);
    printf("\n\n   Make.site:\n\n") ;

    if ((mapset = G_find_file2 ("dig", dig_name, "")) == NULL)
	G_fatal_error ("Could not find DIG file %s\n", dig_name);
    
    export (dig_name, mapset, site_name); 
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
		site_name = G_store(str) ;
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

export(dig_name, mapset, site_name)
    char *dig_name, *mapset, *site_name;
{
	FILE *out;

	if ( ! mapset)
	{
		G_fatal_error ("No mapset specified.\n");
	}

	dig_P_init (dig_name, mapset, &Map);

	out = G_fopen_sites_new (site_name);

	dig_read_head_binary (Map.digit, &Head);

	doit (&Map, out);

	fclose (out);
	dig_P_fini (&Map);

	return(0) ;
}

doit (map, out)
    struct Map_info *map;
    FILE *out;
{
    P_LINE *Line;
    P_ATT *Att;
    struct line_pnts Points;
    char desc[128];
    register int line, ret;
    int hits = 0;
    int labels = 0;
    int binary;
    
    Points.alloc_points = 0;

    /* make a quick pass through checking for labelled sites */
    for (line = 1 ; line <= map->n_lines ; line++)
    {
	Line =  &(map->Line[line]);
	if (Line->type != DOT)
	    continue;
	hits++;
	if (Line->att)
	    labels++;
    }
    if (hits)
    {
	binary = 0;
	if (!labels)
	{
	    binary = 1;
	    printf ( "Creating a BINARY (0/1) site file\n");
	}
	else
	{
	    printf ( "Creating site file with category information\n");
	    if (labels < hits)
	    {
		printf ("Note: %d sites were not labeled\n", hits - labels);
	    }
	}
    }
    else
    { printf ( "No SITES found in vector file\n");
	return (0);
    }
    for (line = 1 ; line <= map->n_lines ; line++)
    {
	Line =  &(map->Line[line]);
	if (Line->type != DOT)
	    continue;
	if (Line->att > 0)
	{
	    Att = &(map->Att[Line->att]);
	    sprintf (desc, "#%d", Att->cat);
	    if (binary)
		G_put_site (out, Att->x, Att->y, "");
	    else
		G_put_site (out, Att->x, Att->y, desc);
	}
	else
	{
	    if (0 > (ret = dig__Read_line (&Points, map->digit, Line->offset)))
	    {
		if (ret == -1)
		{
		    fprintf (stderr, "Out of memory on line %d\n", line);
		    return (-1);
		}
		else 	/* EOF */
		{
		    fprintf (stderr, "Premature EOF. Dig_Plus file probably bad\n");
		    return (0);
		}
	    }
	    /* mark it as label 0 (unlabelled) */
	    if (binary)
		G_put_site (out, Points.x[0], Points.y[0], "");
	    else
		G_put_site (out, Points.x[0], Points.y[0], "#0");
	}
    }
    
    printf ( "\nFound %d sites\n", hits);
}
