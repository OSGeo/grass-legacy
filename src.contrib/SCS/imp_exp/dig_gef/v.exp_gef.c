/* @(#)v.export_gef.c   1.1   03/08/91 GRASS4.0 */
/* @(#)Vexport_gef.c   1.0   02/90 GRASS3.1 */
/*
**  Written by  R.L.Glenn
**  USDA, Soil Conservation Service, CGIS Division
*/
#include    <stdio.h>
#include    "gis.h"
#include    "Vect.h"
#include    "export_gef.h"


#define MAIN
/*
USAGE:  v.out.scsgef input=vector file  output=gef file
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
    char *dig_name, *gef_name;
    char *mapset;
    char ipath[100], errmsg[200];
    struct Option *old, *new;

/* Show advertising */
    G_gisinit("Export SCS-GEF");

    old = G_define_option();
    old->key                    = "input";
    old->type                   = TYPE_STRING;
    old->required               = YES;
    old->multiple               = NO;
    old->gisprompt              = "old,dig, vector";
    old->description            = "vector input file";
			     
    new = G_define_option();
    new->key                    = "output";
    new->type                   = TYPE_STRING;
    new->required               = YES;
    new->multiple               = NO;
    new->gisprompt              = "old,gef,gef";
    new->description            = "SCSGEF format output file";
							  

    if (G_parser (argc, argv))
        exit (-1);

    dig_name = old->answer;
    gef_name = new->answer;

    if (!*dig_name  || !*gef_name )
        {
        fprintf (stderr, "%s: Command line error: missing input or output name.\n\n", argv[0]);
        G_usage();
        exit (-1);
        }


    printf("\n\n   Export.SCS-GEF:\n\n") ;

    if ((mapset = G_find_vector2 (dig_name, "")) == NULL)
	{
	sprintf ("Could not find Vector file <%s>\n", dig_name);
	G_fatal_error (errmsg);
	}
					  
           /*** Get projection info for input mapset ***/
    if (G_projection() != 3)
	{          /* check input projection parameters file */
	G__file_name (ipath, "", PROJECTION_FILE, mapset);
        while (access(ipath,0) != 0)
           {
           fprintf(stderr,"PROJ_INFO file not found  for mapset %s\n",mapset);
           fprintf(stderr,"Run the command :    m.setproj\n");
	   exit(0);
           }
        }

    export (dig_name, mapset, gef_name); 
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

struct dig_head gef_head;
struct Categories cats;

export(dig_name, mapset, gef_name)
    char *dig_name, *mapset, *gef_name;
{
	FILE *out;
	char out_name[250];
	register int num, i, vect_read;

	if ( ! mapset)
	   G_fatal_error ("No mapset specified.\n");

		/* Do initial read of input DIGIT file */
        if ((vect_read = Vect_open_old(&Map,dig_name, mapset)) < 0 )
            {
            G_fatal_error("Reading input file.") ;
            return(-1) ;
            }
        if (vect_read < 2)
            {
            G_fatal_error("You must run v.support on this file.") ;
            return(-1) ;
            }

        fprintf(stderr,"\nLoading <%s> vector information.\n",dig_name);

	G__make_mapset_element("gef") ;
	G__file_name(out_name, "gef", gef_name, G_mapset()) ;
	out = fopen (out_name, "w");

        get_head_info(&gef_head);
	printf ("Writing Header information\n");

        G_read_vector_cats (dig_name, mapset, &cats);
                    /* Initialize the Point structure, ONCE */
        Points = Vect_new_line_struct();

  	write_gef_head (&gef_head, out);
      	printf ("Writing Line information\n");
        write_gef_lines (&cats, &gef_head, out);  
        printf ("Writing Text information\n");
	write_gef_points (&cats, &gef_head, out);
         /* Write Feature information (if any) */
	write_gef_featrs (&cats, &gef_head, out);

        fprintf (out, "-end\n");
	fclose (out);
	Vect_close(&Map);
	printf ("Done.\n");

	return(0) ;
}

