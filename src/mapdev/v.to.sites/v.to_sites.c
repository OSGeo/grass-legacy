/*
**  v.make_sites.c:  source for command line version
**
**  Written by Dave Gerdes  6/89
**
**  GRASS4.0 - converted for new parser - DKS 12/90
**
**  US Army Construction Engineering Research Lab
**
*/

#include    <stdio.h>
#include    "gis.h"
#include    "digit.h"
#include    "dig_head.h"


#define MAIN
#define  USAGE  "v.make.site input=dig file input output=site file output\n"

/*
#define DEBUG
*/
/*  command line args */
static	char  *dig_name = NULL ;
static	char  *site_name = NULL ;


double dig_unit_conversion ();
static	int   snapped = 0 ;

main(argc, argv)
    int argc;
    char **argv;
{
    int   ret ;
    char *mapset;
    char errmsg[100];

    struct Option *old, *new;

    G_gisinit(argv[0]);

/*  check args and set flags  */
	
    old = G_define_option();
    old->key	= "input";	
    old->type	= TYPE_STRING;	
    old->required	= YES;	
    old->multiple	= NO;	
    old->gisprompt	= "old,dig,vector";
    old->description	= "vector file to be converted to sites";	

    new = G_define_option();
    new->key	= "output";	
    new->type	= TYPE_STRING;	
    new->required	= YES;	
    new->multiple	= NO;	
    new->gisprompt	= "new,site_lists,sites";
    new->description	= "sites file to be made from vector file";	

    if (G_parser (argc, argv))
	exit(-1);

    dig_name = old->answer;
    site_name = new->answer;


    if (dig_name == NULL  ||   site_name == NULL)
    {
        fprintf (stderr, "%s: Command line error.\n\n", argv[0]);
	G_usage();
        exit (-1);
    }

    if (!*dig_name  ||  !*site_name )
    {
        fprintf (stderr, "%s: Command line error: missing input or output name.\n\n", argv[0]);
	G_usage();
        exit (-1);
    }

    /* Show advertising */ 
    printf("\n\n   %s:\n\n", G_program_name()) ;
    if ((mapset = G_find_vector2 (dig_name, "")) == NULL)
    {
	sprintf (errmsg, "Could not find vector file %s\n", dig_name);
	G_fatal_error (errmsg);
    } 

    export (dig_name, mapset, site_name); 
    exit (0);
}

#ifdef DEBUG
debugf (format, a, b, c, d, e, f, g, h, i, j, k, l)
    char *format;
    int a, b, c, d, e, f, g, h, i, j, k, l;
{
    fprintf (stderr, format, a, b, c, d, e, f, g, h, i, j, k, l);
}
#endif



export(dig_name, mapset, site_name)
    char *dig_name, *mapset, *site_name;
{
    FILE *out;
    int level;
    struct Map_info Map;

    if ( ! mapset)
    {
	G_fatal_error ("No mapset specified.\n");
    }

    /*
    dig_P_init (dig_name, mapset, &Map);
    */
    level = Vect_open_old (&Map, dig_name, mapset);
    if (level < 0)
	G_fatal_error ("Could not open vector file");
	
    if (level < 2)
	G_fatal_error ("Could not open file at Level 2 access, run v.support");

    out = G_fopen_sites_new (site_name);

    doit (&Map, out);

    fclose (out);

    Vect_close (&Map);

    return(0) ;
}

doit (map, out)
    struct Map_info *map;
    FILE *out;
{
    P_LINE *Line;
    P_ATT *Att;
    struct line_pnts *Points;
    char desc[128];
    register int line, ret;
    int hits = 0;
    int labels = 0;
    int binary;
    
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

    Points = Vect_new_line_struct ();

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
	    if (0 > (ret = Vect__Read_line (map, Points, Line->offset)))
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
		G_put_site (out, Points->x[0], Points->y[0], "");
	    else
		G_put_site (out, Points->x[0], Points->y[0], "#0");
	}
    }

    Vect_destroy_line_struct (Points);
    
    printf ( "\nFound %d sites\n", hits);

    return 0;
}
