/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
**
**  GRASS4.0: converted for new parser - 1/91 - dks
*/
#include    <stdio.h>
#include    "gis.h"
#include    "Vect.h"
#include    "dig_head.h"


#define MAIN

/* 
USAGE:  v.out.dlg input=vector file   output=dlg file  
*/

/*
#define DEBUG
*/

double dig_unit_conversion ();
static	int   snapped = 0 ;

main(argc, argv)
    int argc;
    char **argv;
{
    char *dig_name, *dlg_name;
    char *mapset;
    char errmsg[200];
    struct Option *old, *new;


/* Show advertising */
    G_gisinit(argv[0]) ;

    old = G_define_option();
    old->key			= "input";
    old->type			= TYPE_STRING;
    old->required		= YES;
    old->multiple		= NO;
    old->gisprompt		= "old,dig,vector";
    old->description		= "vector input file";
    
    new = G_define_option();
    new->key			= "output";
    new->type			= TYPE_STRING;
    new->required		= YES;
    new->multiple		= NO;
    new->gisprompt		= "new,dlg,dlg";
    new->description		= "DLG-3 Optional format output file";


    if (G_parser (argc, argv))
	exit (-1);

    dig_name = old->answer;
    dlg_name = new->answer;

    if (!*dig_name  || !*dlg_name )
    {
        fprintf (stderr, "%s: Command line error: missing input or output name.\n\n", argv[0]);
	G_usage();
        exit (-1);
    }

    printf("\n\n   Export.DLG:\n\n") ;

    if ((mapset = G_find_vector2 (dig_name, "")) == NULL)
    {
	sprintf ("Could not find Vector file <%s>\n", dig_name);
	G_fatal_error (errmsg);
    }
    
    export (dig_name, mapset, dlg_name); 
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


struct Map_info Map;

export(dig_name, mapset, dlg_name)
    char *dig_name, *mapset, *dlg_name;
{
	FILE *out;
	int att, line;
	char out_name[250];
	register int num, i;
	char *X, *Y;
	int n_points;
	int level;

	if ( ! mapset)
	{
		G_fatal_error ("No mapset specified.\n");
	}

	/*
	dig_P_init (dig_name, mapset, &Map);
	*/
	level = Vect_open_old (&Map, dig_name, mapset);
	if (level <= 0)
	    G_fatal_error ("Can't open vector file");

	if (level < 2 || !Map.all_areas || !Map.all_isles)
	{
	    fprintf (stderr, "\n\nYou must first run Support.vect (option 1) on this data.\n");
	    exit (1);
	}
	G__make_mapset_element("dlg") ;
	G__file_name(out_name, "dlg", dlg_name, G_mapset()) ;
	out = fopen (out_name, "w");

	/*
	dig_read_head_binary (Map.digit, &D_head);
	*/

	build_area_one (&Map);
	shuffle_dots (&Map);
	printf ("Writing Header information\n");
	write_dlg_head (&Map, &(Map.head), out);
	printf ("Writing Node information\n");
	write_dlg_nodes (&Map, out);
	printf ("Writing Area information\n");
	write_dlg_areas (&Map, out);
	printf ("Writing Line information\n");
	write_dlg_lines (&Map, out);

	fclose (out);
	/*
	dig_P_fini (&Map);
	*/
	Vect_close (&Map);
	printf ("Done.\n");

	return(0) ;
}
