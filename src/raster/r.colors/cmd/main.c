#include "gis.h"

/* main.c
 *
 * specify and print options added by DBA Systems, Inc.
 *
 */

main (argc, argv)
int argc;
char *argv[];
{
    int overwrite;
    int have_colors;
    struct Colors colors;
    struct Range range;
    CELL min, max;
    char *name, *mapset;
    char *type;
    struct Flag *flag1, *flag2;
    struct Option *opt1, *opt2;

    G_gisinit (argv[0]);

    opt1 = G_define_option();
    opt1->key         = "map";
    opt1->type        = TYPE_STRING;
    opt1->required    = YES;
    opt1->gisprompt  = "old,cell,raster" ;
    opt1->description = "raster map name";

    opt2 = G_define_option();
    opt2->key         = "color";
    opt2->key_desc    = "type";
    opt2->type        = TYPE_STRING;
    opt2->required    = YES;
    opt2->options     = "aspect,grey,grey.eq,gyr,rainbow,ramp,random,ryg,wave,rules";
    opt2->description = "type of color table";

    flag1 = G_define_flag();
    flag1->key = 'w';
    flag1->description = "Don't overwrite existing color table";

    flag2 = G_define_flag();
    flag2->key = 'q';
    flag2->description = "Quietly";

    if (G_parser(argc, argv) < 0)
    {
	more_usage();
	exit(1);
    }

    overwrite = ( ! flag1->answer);
    name = opt1->answer;
    type = opt2->answer;

    mapset = G_find_cell2 (name, "");
    if (mapset == NULL)
    {
	fprintf (stderr, "ERROR: %s - map not found\n", name);
	exit(1);
    }

    have_colors = G_read_colors (name, mapset, &colors);
    /*if (have_colors >= 0)
	G_free_colors (&colors);*/

    if (have_colors > 0 && !overwrite)
	exit(0);

    G_read_range (name, mapset, &range);
    G_get_range_min_max (&range, &min, &max);


    /* 
     * here the predefined color-table color-types random, ramp, wave,
     * grey, aspect, rainbow, and ryb are created by GRASS library calls. 
     */
    if (strcmp (type, "random") == 0)
	G_make_random_colors (&colors, min, max);
    else if (strcmp (type, "ramp") == 0)
	G_make_ramp_colors (&colors, min, max);
    else if (strcmp (type, "wave") == 0)
	G_make_wave_colors (&colors, min, max);
    else if (strcmp (type, "grey") == 0)
	G_make_grey_scale_colors (&colors, min, max);
    else if (strcmp (type, "grey.eq") == 0)
	eq_grey_colors (name, mapset, &colors, flag2->answer);
    else if (strcmp (type, "aspect") == 0)
	G_make_aspect_colors (&colors, min, max);
    else if (strcmp (type, "rainbow") == 0)
	G_make_rainbow_colors (&colors, min, max);
    else if (strcmp (type, "ryg") == 0)
	G_make_ryg_colors (&colors, min, max);
    else if (strcmp (type, "gyr") == 0)
	G_make_gyr_colors (&colors, min, max);
    else if (strcmp (type, "rules") == 0)
    {
       if (!read_color_rules(&colors, min, max))
          exit(-1); 
    }
    else
    {
	fprintf (stderr, "ERROR: %s - unknown color request\n", type);
	G_usage();
	more_usage();
	exit(1);
    }

    if (G_write_colors (name, mapset, &colors) >= 0 && !flag2->answer)
	printf ("Color table for [%s] set to %s\n", name, type);
    exit(0);
}
more_usage()
{
    fprintf (stderr, "\nWhere color type is one of:\n");
    fprintf (stderr, "  aspect    (aspect oriented grey colors)\n");
    fprintf (stderr, "  grey      (linear grey scale)\n");
    fprintf (stderr, "  grey.eq   (histogram equalized grey scale)\n");
    fprintf (stderr, "  gyr       (green through yellow to red colors)\n");
    fprintf (stderr, "  rainbow   (rainbow color table)\n");
    fprintf (stderr, "  ramp      (color ramp)\n");
    fprintf (stderr, "  ryg       (red through yellow to green colors)\n");
    fprintf (stderr, "  random    (random color table)\n");
    fprintf (stderr, "  wave      (color wave)\n");
    fprintf (stderr, "  rules     (create new color table by rules)\n");
    fprintf (stderr, "\n");
}
