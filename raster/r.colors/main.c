/****************************************************************************
 *
 * MODULE:       r.colors
 *
 * AUTHOR(S):    Michael Shapiro - CERL
 *               David Johnson
 *
 * PURPOSE:      Allows creation and/or modification of the color table
 *               for a raster map layer.
 *
 * COPYRIGHT:    (C) 2006 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 ***************************************************************************/

/* main.c
 *
 * specify and print options added by DBA Systems, Inc.
 * update 10/99 for GRASS 5
 */

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include "local_proto.h"

static char *rules_files(void)
{
	char path[GPATH_MAX];
	char **names;
	int count;
	char *list = NULL;
	int size = 0;
	int len = 0;
	int i;

	sprintf(path, "%s/etc/colors", G_gisbase());

	names = G__ls(path, &count);

	for (i = 0; i < count; i++)
	{
		char *name = names[i];
		int n = strlen(name);

		if (size < len + n + 2)
		{
			size = len + n + 200;
			list = G_realloc(list, size);
		}

		if (len > 0)
			list[len++] = ',';

		memcpy(&list[len], name, n + 1);
		len += n;

		G_free(name);
	}

	G_free(names);

	return list;
}

static void list_rules_files(void)
{
	char path[GPATH_MAX];
	char **names;
	int names_len;
	int i;

	sprintf(path, "%s/etc/colors", G_gisbase());

	names = G__ls(path, &names_len);

	for (i = 0; i < names_len; i++)
	{
		printf("%s\n", names[i]);
		G_free(names[i]);
	}

	G_free(names);
}

int main (int argc, char *argv[])
{
    int overwrite;
    int have_colors;
    struct Colors colors, colors_tmp;
    struct Cell_stats statf;
    int have_stats = 0;
    struct FPRange range;
    DCELL min, max;
    char *name, *mapset;
    char *type, *cmap, *cmapset;
    char *rules;
    int fp;
    struct GModule *module;
    struct Flag *flag1, *flag3, *g_flag, *e_flag;
    struct Option *opt1, *opt2, *opt3, *opt4;

    /* please, remove before GRASS 7 released */
    struct Flag *q_flag;

    G_gisinit (argv[0]);

	module = G_define_module();
	module->keywords = _("raster");
    module->description =
		_("Creates/Modifies the color table associated with "
		"a raster map layer.");

    opt1 = G_define_standard_option(G_OPT_R_MAP);

    opt2 = G_define_option();
    opt2->key         = "color";
    opt2->key_desc    = "type";
    opt2->type        = TYPE_STRING;
    opt2->required    = NO;
    opt2->options     = "aspect,grey,grey.eq,grey.log,byg,byr,gyr,rainbow,ramp,random,ryg,wave,rules";
    opt2->description = _("Type of color table");
    opt2->descriptions = "aspect;aspect oriented grey colors;"
		"grey;linear grey scale;"
		"grey.eq;histogram equalized grey scale;"
    		"grey.log;histogram logarithmic transformed grey scale;"
		"byg;blue through yellow to green colors;"
		"byr;blue through yellow to red colors;"
		"gyr;green through yellow to red colors;"
		"rainbow;rainbow color table;"
		"ramp;color ramp;"
		"ryg;red through yellow to green colors;"
		"random;random color table;"
		"wave;color wave;"
		"rules;create new color table by rules";

    opt3 = G_define_option();
    opt3->key         = "rast";
    opt3->type        = TYPE_STRING;
    opt3->required    = NO;
    opt3->gisprompt  = "old,cell,raster" ;
    opt3->description = _("Raster map name from which to copy color table");

    opt4 = G_define_option();
    opt4->key         = "rules";
    opt4->type        = TYPE_STRING;
    opt4->required    = NO;
    opt4->description = _("Name of predefined rules file");
    opt4->options     = rules_files();

    flag1 = G_define_flag();
    flag1->key = 'w';
    flag1->description = _("Keep existing color table");

    flag3 = G_define_flag();
    flag3->key = 'l';
    flag3->description = _("List rules");

    g_flag = G_define_flag() ;
    g_flag->key         = 'g' ;  
    g_flag->description = _("Logarithmic scaling") ;

    e_flag = G_define_flag() ;
    e_flag->key         = 'e' ;  
    e_flag->description = _("Histogram equalization") ;

    /* please, remove before GRASS 7 released */
    q_flag = G_define_flag() ;
    q_flag->key         = 'q' ;  
    q_flag->description = _("Run quietly") ;


    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    /* please, remove before GRASS 7 released */
    if(q_flag->answer) {
        G_putenv("GRASS_VERBOSE","0");
        G_warning(_("The '-q' flag is superseded and will be removed "
            "in future. Please use '--quiet' instead."));
    }


    if (flag3->answer)
    {
	    list_rules_files();
	    return 0;
    }

    overwrite = (!flag1->answer);
    name = opt1->answer;
    type = opt2->answer;
    cmap = opt3->answer;
    rules = opt4->answer;

    if (!name)
	G_fatal_error(_("No map specified"));

    if (!cmap && !type && !rules)
	G_fatal_error(_("One of options \"color\", \"rast\" OR \"rules\" MUST be specified!"));

    if (cmap && type)
	G_warning(_("Both options \"color\" AND \"rast\" specified - ignoring rast"));

    if (rules && type)
	G_warning(_("Both options \"color\" AND \"rules\" specified - ignoring rules"));

    if (rules && cmap)
	G_warning(_("Both options \"rast\" AND \"rules\" specified - ignoring rast"));

    if (cmap && g_flag->answer)
	G_warning(_("Both \"rast=\" AND \"-l\" specified - ignoring -l"));

    if (cmap && e_flag->answer)
	G_warning(_("Both \"rast=\" AND \"-e\" specified - ignoring -e"));

    mapset = G_find_cell2 (name, "");
    if (mapset == NULL)
	G_fatal_error(_("%s - map not found"), name);

    G_suppress_warnings (1);
    have_colors = G_read_colors (name, mapset, &colors);
    /*if (have_colors >= 0)
	G_free_colors (&colors);*/

    if (have_colors > 0 && !overwrite)
	exit(EXIT_SUCCESS);

    G_suppress_warnings (0);

    fp = G_raster_map_is_fp(name, mapset);
    G_read_fp_range (name, mapset, &range);
    G_get_fp_range_min_max (&range, &min, &max);

    if (type)
    {
	/* 
	 * here the predefined color-table color-types random, ramp, wave,
	 * grey, aspect, rainbow, and ryb are created by GRASS library calls. 
	 */
	if (strcmp (type, "random") == 0)
	{
	    if(fp)
	       G_fatal_error (_("Can't make random color table for floating point map"));
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
		G_fatal_error(_("Can't make grey.eq color table for floating point map"));
	    if (!have_stats)
		have_stats = get_stats (name, mapset, &statf);
	    G_make_histogram_eq_colors (&colors, &statf);
	}
	else if (strcmp (type, "grey.log") == 0)
	{
	    if(fp)
		G_fatal_error(_("Can't make logarithmic color table for floating point map"));
	    if (!have_stats)
		have_stats = get_stats (name, mapset, &statf);
	    G_make_histogram_log_colors (&colors, &statf, (CELL) min, (CELL) max);
	}
	else if (strcmp (type, "aspect") == 0)
	    G_make_aspect_fp_colors (&colors, min, max);
	else if (strcmp (type, "rainbow") == 0)
	    G_make_rainbow_fp_colors (&colors, min, max);
	else if (strcmp (type, "ryg") == 0)
	    G_make_ryg_fp_colors (&colors, min, max);
	else if (strcmp (type, "gyr") == 0)
	    G_make_gyr_fp_colors (&colors, min, max);
	else if (strcmp (type, "byr") == 0)
	    G_make_byr_fp_colors (&colors, min, max);
	else if (strcmp (type, "byg") == 0)
	    G_make_byg_fp_colors (&colors, min, max);
	else if (strcmp (type, "rules") == 0)
	{
	    if (!read_color_rules(stdin, &colors, min, max, fp))
	      exit(EXIT_FAILURE); 
	}
	else
	    G_fatal_error(_("%s - unknown color request"), type);

	if(fp) G_mark_colors_as_fp(&colors);

	if (e_flag->answer)
	{
	    if (!have_stats)
		have_stats = get_stats(name, mapset, &statf);
	    G_histogram_eq_colors(&colors_tmp, &colors, &statf);
	    colors = colors_tmp;
	}

	if (g_flag->answer)
	{
	    G_log_colors(&colors_tmp, &colors, 100);
	    colors = colors_tmp;
	}

	if(fp) G_mark_colors_as_fp(&colors);

	if (G_write_colors (name, mapset, &colors) >= 0 )
	    G_message(_("Color table for [%s] set to %s"), name, type);
    }
    else if (rules)
    {
	if (!G_make_fp_colors(&colors, rules, min, max))
	    G_fatal_error(_("Unable to load rules file %s"), rules);

	if(fp) G_mark_colors_as_fp(&colors);

	if (e_flag->answer)
	{
	    if (!have_stats)
		have_stats = get_stats(name, mapset, &statf);
	    G_histogram_eq_colors(&colors_tmp, &colors, &statf);
	    colors = colors_tmp;
	}

	if (g_flag->answer)
	{
	    G_log_colors(&colors_tmp, &colors, 100);
	    colors = colors_tmp;
	}

	if(fp) G_mark_colors_as_fp(&colors);

	if (G_write_colors (name, mapset, &colors) >= 0 )
	    G_message(_("Color table for [%s] set to %s"), name, rules);
    }
    else
    {  /* use color from another map (cmap) */
	cmapset = G_find_cell2 (cmap, "");
	if (cmapset == NULL)
	    G_fatal_error(_("%s - map not found"), cmap);

	if (0 > G_read_colors (cmap, cmapset, &colors))
	    G_fatal_error(_("Unable to read color table for %s"), cmap);

	if(fp) G_mark_colors_as_fp(&colors);

	if (G_write_colors (name, mapset, &colors) >= 0 )
	    G_message(_("Color table for [%s] set to %s"), name, cmap);
    }

    exit(EXIT_SUCCESS);
}
