/* @(#)v.export_dlg.c	1.2   03/08/91 GRASS4.0 */
/* @(#)Vexport_dlg.c	1.2   06/27/90 */
/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
**  modified by : RL Glenn, SCS, CGIS Division
*/
#include    <stdio.h>
#include    "gis.h"
#include    "Vect.h"
#include    "export_dlg.h"


#define MAIN
/*
USAGE:  v.out.dlg.scs input=vector file   output=dlg file
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
    char errmsg[200], ipath[100];
    struct Option *old, *new;
		 
/* Show advertising */
    G_gisinit("Export DLG");

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
    new->gisprompt              = "new,dlg,dlg";
    new->description            = "DLG-3 Optional format output file";


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
		
     if ((mapset = G_find_vector2 (dig_name, "")) == NULL)
         {
         sprintf ("Could not find Vector file <%s>\n", dig_name);
         G_fatal_error (errmsg);
         }

          /* Check for dig_cats file for this map */
     G__file_name (ipath, "dig_cats", dig_name, mapset);
     if (access(ipath,0) != 0)
         {
         fprintf(stderr,"Dig_cats file not found  for map %s\n",dig_name);
         exit(0);
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


    printf("\n\n   Export.DLG:\n\n") ;

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
	FILE *out, *tmp, *fcat;
	int att, line, vect_read;
	char out_name[250], cat_name[250], command[100], *tmp_name;
	register int num, i;
	char *X, *Y;
	int n_points;

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

	G__make_mapset_element("dlg") ;
	G__file_name(out_name, "dlg", dlg_name, G_mapset()) ;
	G__file_name(cat_name, "dig_cats", dig_name, G_mapset()) ;
	out = fopen (out_name, "w");
/*-->*/
	fcat = fopen (cat_name, "r");
	tmp_name = G_tempfile();
	tmp = fopen (tmp_name, "w");

	printf ("Writing Header information\n");
	write_dlg_head (&Map, &(Map.head), out);
  	build_area_one (&Map); 
	shuffle_dots (&Map);
        printf ("Writing Node information\n");
	write_dlg_nodes (&Map, out, tmp, fcat);
        printf ("Writing Area information\n");
	write_dlg_areas (&Map, out, tmp, fcat);
    	printf ("Writing Line information\n");
 	write_dlg_lines (&Map, out, tmp, fcat); 

	fclose (out);
	fclose (fcat);
	fclose (tmp);

	V1_close (&Map);

    	printf ("Writing Attribute information\n");
	/* sort on labels first */
 	sprintf(command,"sort %s > %s.att\n",tmp_name,out_name);
    	if (system( command) )
	{
		fprintf(stderr, "ERROR:  Could not create dlg attribute file: '%s.att' \n", out_name) ;
	}
/*-->*/

	printf ("Done.\n");

	return(0) ;
}
