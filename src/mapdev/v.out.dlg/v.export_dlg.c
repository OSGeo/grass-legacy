/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
**
**  GRASS4.0: converted for new parser - 1/91 - dks
*/
#include    <stdio.h>
#include    "gis.h"
#include    "Vect.h"
#include    "local_proto.h"


#define MAIN

/* 
USAGE:  v.out.dlg input=vector file   output=dlg file  
*/

/*
#define DEBUG
*/

static	int   snapped = 0 ;

int main (int argc, char **argv)
{
    char *dig_name, *dlg_name;
    char *mapset;
    char errmsg[200];
	struct GModule *module;
    struct Option *old, *new;


/* Show advertising */
    G_gisinit(argv[0]) ;

	module = G_define_module();
	module->description =
		"Converts binary GRASS vector data to "
		"DLG-3 Optional vector data format.";

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

    fprintf (stdout,"\n\n   Export.DLG:\n\n") ;

    if ((mapset = G_find_vector2 (dig_name, "")) == NULL)
    {
	sprintf ("Could not find Vector file <%s>\n", dig_name);
	G_fatal_error (errmsg);
    }
    
    export (dig_name, mapset, dlg_name); 
    exit (0);
}


#ifdef DEBUG
#include <stdarg.h>
int debugf (char *format, ...)
{
    va_list a;
    va_start(format,a);
    vfprintf (stderr, format, a);
    va_end(a);

    return 0;
}
#endif


struct Map_info Map;

int export (char *dig_name, char *mapset, char *dlg_name)
{
	FILE *out;
	char out_name[250];
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
	fprintf (stdout,"Writing Header information\n");
	write_dlg_head (&Map, &(Map.head), out);
	fprintf (stdout,"Writing Node information\n");
	write_dlg_nodes (&Map, out);
	fprintf (stdout,"Writing Area information\n");
	write_dlg_areas (&Map, out);
	fprintf (stdout,"Writing Line information\n");
	write_dlg_lines (&Map, out);

	fclose (out);
	/*
	dig_P_fini (&Map);
	*/
	Vect_close (&Map);
	fprintf (stdout,"Done.\n");

	return(0) ;
}
