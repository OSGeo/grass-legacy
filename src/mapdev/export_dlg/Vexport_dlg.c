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
#define  USAGE  "Vexport.dlg dig=input dlg=output\n"

/*
#define DEBUG
*/
/*  command line args */
static	char  *dig_name = NULL ;
static	char  *dlg_name = NULL ;

static  int  load_args() ;

struct Command_keys vars[] = {
 { "dig", 1 },
 { "dlg", 2 },
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

    if (ret < 0  ||  dig_name == NULL  ||   dlg_name == NULL)
    {
        fprintf (stderr, "%s: Command line error.\n\n Usage: %s\n",
		argv[0], USAGE);
        exit (-1);
    }

/* Show advertising */
    G_gisinit("Export DLG");
    printf("\n\n   Export.DLG:\n\n") ;

    if ((mapset = G_find_file2 ("dig", dig_name, "")) == NULL)
	G_fatal_error ("Could not find DIG file %s\n", dig_name);
    
    export (dig_name, mapset, dlg_name); 
    exit (0);
}

static
load_args (position, str)
    int position;
    char *str;
{
    double atof ();

    switch(position)
    {
	case 1:
		dig_name = G_store(str) ;
		break ;
	case 2:
		dlg_name = G_store(str) ;
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

export(dig_name, mapset, dlg_name)
    char *dig_name, *mapset, *dlg_name;
{
	FILE *out;
	int att, line;
	char out_name[250];
	register int num, i;
	char *X, *Y;
	int n_points;

	if ( ! mapset)
	{
		G_fatal_error ("No mapset specified.\n");
	}

	dig_P_init (dig_name, mapset, &Map);

	if (!Map.all_areas || !Map.all_isles)
	{
	    fprintf (stderr, "\n\nYou must first run Support.vect (option 1) on this data.\n");
	    exit (1);
	}
	G__make_mapset_element("dlg") ;
	G__file_name(out_name, "dlg", dlg_name, G_mapset()) ;
	out = fopen (out_name, "w");

	dig_read_head_binary (Map.digit, &Head);

	build_area_one (&Map);
	shuffle_dots (&Map);
	printf ("Writing Header information\n");
	write_dlg_head (&Map, &Head, out);
	printf ("Writing Node information\n");
	write_dlg_nodes (&Map, out);
	printf ("Writing Area information\n");
	write_dlg_areas (&Map, out);
	printf ("Writing Line information\n");
	write_dlg_lines (&Map, out);

	fclose (out);
	dig_P_fini (&Map);
	printf ("Done.\n");

	return(0) ;
}
