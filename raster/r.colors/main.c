#include "gis.h"
#include "local_proto.h"

/* main.c
 *
 * specify and print options added by DBA Systems, Inc.
 * update 10/99 for GRASS 5
 */

int main (int argc, char *argv[])
{
    int overwrite;
    int have_colors;
    struct Colors colors;
    struct FPRange range;
    DCELL min, max;
    char *name, *mapset;
    char *type, *cmap, *cmapset;
    char errbuf[256];
    int fp;
	struct GModule *module;
    struct Flag *flag1, *flag2;
    struct Option *opt1, *opt2, *opt3;

    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Creates/Modifies the color table associated with "
		"a raster map layer.";

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
    opt2->required    = NO;
    opt2->options     = "aspect,grey,grey.eq,gyr,rainbow,ramp,random,ryg,wave,rules";
    opt2->description = "type of color table";

    opt3 = G_define_option();
    opt3->key         = "rast";
    opt3->type        = TYPE_STRING;
    opt3->required    = NO;
    opt3->gisprompt  = "old,cell,raster" ;
    opt3->description = "raster map name from which to copy color table";

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
    cmap = opt3->answer;

    if(!cmap && !type){
	sprintf(errbuf,
		"One of options \"color\" OR \"rast\" MUST be specified!");
	G_warning(errbuf);
	G_usage();
	exit(1);
    }

    if(cmap && type){
	sprintf(errbuf,
	    "Both options \"color\" AND \"rast\" specified - ignoring rast");
	G_warning(errbuf);
    }

    mapset = G_find_cell2 (name, "");
    if (mapset == NULL)
    {
	fprintf (stderr, "ERROR: %s - map not found\n", name);
	exit(1);
    }

    
    G_suppress_warnings (1);
    have_colors = G_read_colors (name, mapset, &colors);
    /*if (have_colors >= 0)
	G_free_colors (&colors);*/

    if (have_colors > 0 && !overwrite)
	exit(0);
    G_suppress_warnings (0);

    G_sleep_on_error (0);
    fp = G_raster_map_is_fp(name, G_mapset());
    G_read_fp_range (name, mapset, &range);
    G_get_fp_range_min_max (&range, &min, &max);
    G_sleep_on_error (1);

    if(type){
	/* 
	 * here the predefined color-table color-types random, ramp, wave,
	 * grey, aspect, rainbow, and ryb are created by GRASS library calls. 
	 */
	if (strcmp (type, "random") == 0)
	{
	    if(fp)
	       G_fatal_error
		  ("Can't make random color table for floating point map\n");
	    G_make_random_colors (&colors, (CELL) min, (CELL) max);
	}
	else if (strcmp (type, "ramp") == 0)
	    G_make_ramp_fp_colors (&colors, min, max);
	else if (strcmp (type, "wave") == 0)
	    G_make_wave_fp_colors (&colors, min, max);
	else if (strcmp (type, "grey") == 0)
	    G_make_grey_scale_fp_colors (&colors, min, max);
	else if (strcmp (type, "grey.eq") == 0)
	{
	    if(fp)
		G_fatal_error
		   ("Can't make grey.eq color table for floating point map\n");
	    eq_grey_colors (name, mapset, &colors, flag2->answer);
	}
	else if (strcmp (type, "aspect") == 0)
	    G_make_aspect_fp_colors (&colors, min, max);
	else if (strcmp (type, "rainbow") == 0)
	    G_make_rainbow_fp_colors (&colors, min, max);
	else if (strcmp (type, "ryg") == 0)
	    G_make_ryg_fp_colors (&colors, min, max);
	else if (strcmp (type, "gyr") == 0)
	    G_make_gyr_fp_colors (&colors, min, max);
	else if (strcmp (type, "rules") == 0)
	{
	    if (!read_color_rules(&colors, min, max, fp))
	      exit(-1); 
	}
	else
	{
	    fprintf (stderr, "ERROR: %s - unknown color request\n", type);
	    G_usage();
	    more_usage();
	    exit(1);
	}
	if(fp) G_mark_colors_as_fp(&colors);

	if (G_write_colors (name, mapset, &colors) >= 0 && !flag2->answer)
	    fprintf (stdout, "Color table for [%s] set to %s\n", name, type);
    }
    else{  /* use color from another map (cmap) */
	cmapset = G_find_cell2 (cmap, "");
	if (cmapset == NULL) {
	    fprintf (stderr, "ERROR: %s - map not found\n", name);
	    exit(1);
	}
	if(0 > G_read_colors (cmap, cmapset, &colors)){
	    sprintf(errbuf,"Unable to read color table for %s", cmap);
	    G_fatal_error(errbuf);
	}

	if(fp) G_mark_colors_as_fp(&colors);

	if (G_write_colors (name, mapset, &colors) >= 0 && !flag2->answer)
	    fprintf (stdout, "Color table for [%s] set to %s\n", name, cmap);
    }

    exit(0);
}

int more_usage (void)
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
    
    return 0;
}
