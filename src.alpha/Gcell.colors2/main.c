#include "gis.h"
#include "specs.h"

/* main.c
 *
 * Main for program Gcell.colors
 *
 * specify and print options added by DBA Systems, Inc.
 *
 */

main (argc, argv)
int argc;
char *argv[];
{
    struct SpecList specs;
    static int overwrite = 1;
    int have_colors;
    struct Colors colr;
    struct Range range;
    CELL min, max;
    char name[100], *mapset;
    char *type;

    G_gisinit (argv[0]);

    if (argc > 1 && strcmp (argv[1],"-") == 0)
    {
	overwrite = 0;
	argc--;
	argv++;
    }
    if (argc != 3)
	usage();

    strcpy (name, argv[1]);
    mapset = G_find_cell2 (name, "");
    if (mapset == NULL)
    {
	fprintf (stderr, "ERROR: %s - cell file not found\n", name);
	exit(1);
    }

    have_colors = G_read_colors (name, mapset, &colr);
    /*if (have_colors >= 0)
	G_free_colors (&colr);*/

    if (have_colors > 0 && !overwrite)
	exit(0);

    G_read_range (name, mapset, &range);
    min = range.nmin ? range.nmin : range.pmin;
    max = range.pmax ? range.pmax : range.nmax;

    type = argv[2];

    /* 
     * here the predefined color-table color-types random, ramp, wave,
     * grey, aspect, rainbow, and ryb are created by GRASS library calls. 
     */
    if (strcmp (type, "random") == 0)
	G_make_random_colors (&colr, min, max);
    else if (strcmp (type, "ramp") == 0)
	G_make_color_ramp (&colr, min, max);
    else if (strcmp (type, "wave") == 0)
	G_make_color_wave (&colr, min, max);
    else if (strcmp (type, "grey") == 0)
	G_make_grey_scale (&colr, min, max);
    else if (strcmp (type, "aspect") == 0)
	G_make_aspect_colors (&colr, min, max);
    else if (strcmp (type, "rainbow") == 0)
	G_make_rainbow_colors (&colr, min, max);
    else if (strcmp (type, "ryg") == 0)
	G_make_red_yel_grn (&colr, min, max);
    else if (strcmp (type, "print") == 0)
       {
       print_lut(colr);
       exit(0);          
       }
    else if (strcmp (type, "specify") == 0)
       {
       G_init_colors(&colr);
       /* read and parse color-table specs from stdin */ 
       if (read_specs(&specs,min,max))
          exit(-1); 
       /* build color-table according to specs */
       if (build_lut(&specs,&colr)) 
          exit(-1);    
       }
    else
    {
	fprintf ("ERROR: %s - unknown color request\n", type);
	usage();
    }

    if (G_write_colors (name, mapset, &colr) >= 0)
	printf ("Color table for [%s] set to %s\n",argv[1], type);
    exit(0);
}
usage()
{
    fprintf (stderr, "Usage: %s [-] layer color-type\n\n", G_program_name());
    fprintf (stderr, "Where color-type is one of\n");
    fprintf (stderr, "  aspect    (aspect oriented grey colors)\n");
    fprintf (stderr, "  grey      (grey scale)\n");
    fprintf (stderr, "  print     (print cell file's color table)\n");
    fprintf (stderr, "  rainbow   (rainbow color table)\n");
    fprintf (stderr, "  ramp      (color ramp)\n");
    fprintf (stderr, "  ryg       (red through yellow to green colors)\n");
    fprintf (stderr, "  random    (random color table)\n");
    fprintf (stderr, "  specify   (create new color table by specifications)\n");
    fprintf (stderr, "  wave      (color wave)\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "Note: if the - option is specified, a color table is\n");
    fprintf (stderr, "generated only if no color table already exists\n");
    exit(1);
}
