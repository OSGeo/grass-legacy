/*
 * r3.colors
 *
 * adapted from r.colors
 * Markus Neteler 1/2001
 * neteler@geog.uni-hannover.de
 *
 * GPL 2 or later
 *
 * TODO: The written color table is rather empty! Why?
 *   obviously problem with G3D_Map struct and name...
 */

#include "gis.h"
#include "G3d.h"
#include "local_proto.h"


static char *name, *cmap;
extern char * G_find_grid3 ();

int main (int argc, char *argv[])
{
    int overwrite;
    int have_colors, fp;
    struct Colors colors;
    struct FPRange range;
    G3D_Map *g3dname;
    DCELL min, max;
/*    G3D_Map *name, *cmap;*/
    char *mapset;
    char *type, *cmapset;
    char errbuf[256];
    struct GModule *module;
    struct Flag *flag1, *flag2;
    struct Option *opt1, *opt2, *opt3;
    G3D_Region region;

    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Creates/Modifies the color table associated with "
		"a 3dcell volume.";

    opt1 = G_define_option();
    opt1->key         = "grid3";
    opt1->type        = TYPE_STRING;
    opt1->required    = YES;
    opt1->gisprompt  = "old,grid3,3d-raster" ;
    opt1->description = "3dcell volume name";

    opt2 = G_define_option();
    opt2->key         = "color";
    opt2->key_desc    = "type";
    opt2->type        = TYPE_STRING;
    opt2->required    = NO;
    opt2->options     = "aspect,grey,grey.eq,gyr,rainbow,ramp,random,ryg,wave,rules";
    opt2->description = "type of color table";

    opt3 = G_define_option();
    opt3->key         = "3dcell";
    opt3->type        = TYPE_STRING;
    opt3->required    = NO;
    opt3->gisprompt  = "old,grid3,3d-raster" ;
    opt3->description = "3dcell volume name from which to copy color table";

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
    g3dname = G_store(opt1->answer);
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

   if ((mapset = G_find_grid3 (name, "")) == NULL)
   {
      G3d_fatalError("g3d file not found");
      exit (1);
    }

  /* Figure out the current region */
    G3d_getWindow(&region);

    G_suppress_warnings (1);
    have_colors = G3d_readColors (name, mapset, &colors);

    fp=1; /* always */
    if (have_colors > 0 && !overwrite)
	exit(0);
    G_suppress_warnings (0);

    G_sleep_on_error (0);
    G3d_readRange (g3dname, mapset, &range);
    G3d_range_min_max (g3dname, &min, &max);
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
		  ("Can't make random color table for 3d floating point map\n");
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

	if (G3d_writeColors (name, mapset, &colors) >= 0 && !flag2->answer)
	    fprintf (stdout, "Color table for [%s] set to %s\n", name, type);
    }
    else{  /* use color from another map (cmap) */
        if ((cmapset = G_find_grid3 (cmap, "")) == NULL)
        {
	    fprintf (stderr, "ERROR: %s - map not found\n", cmap);
	    exit(1);
	}
	if(0 > G3d_readColors (cmap, cmapset, &colors)){
	    sprintf(errbuf,"Unable to read color table for %s", cmap);
	    G_fatal_error(errbuf);
	}

	if(fp) G_mark_colors_as_fp(&colors);

	if (G3d_writeColors (name, mapset, &colors) >= 0 && !flag2->answer)
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
