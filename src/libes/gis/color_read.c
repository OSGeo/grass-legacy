/**********************************************************************
 *
 *  G_read_colors (name, mapset, colors)
 *      char *name                   name of map
 *      char *mapset                 mapset that map belongs to
 *      struct Colors *colors        structure to hold color info
 *
 *  Reads the color information associated with map layer "map"
 *  in mapset "mapset" into the structure "colors".
 *
 *  returns:    1  if successful
 *              0  if missing, but default colors generated
 *             -1  on fail
 *
 *  note:   If a secondary color file for map name "name" exists
 *          in the current project, that color file is read.  This
 *          allows the user to define their own color lookup tables
 *          for cell maps found in other mapsets.
 *
 *          Warning message is printed if the color file is
 *          missing or invalid.
 *********************************************************************/

#include "gis.h"

G_read_colors (name, mapset, colors)
    char *name ;
    char *mapset ;
    struct Colors *colors ;
{
    char buf[512];
    char *err;
    char xname[512], xmapset[512];
    struct Range range;
    CELL min, max;

    G_init_colors (colors);
    if (G__name_is_fully_qualified (name, xname, xmapset))
    {
	if (strcmp (xmapset, mapset) != 0)
	    return -1;
	name = xname;
    }

/* first look for secondary color table in current mapset */
    sprintf (buf,"colr2/%s", mapset);
    if (read_colors (buf, name, G_mapset(), colors) >= 0)
	return 1;

/* now look for the regular color table */
    switch (read_colors ("colr", name, mapset, colors))
    {
    case -2:
	    if (G_read_range (name, mapset, &range) >= 0)
	    {
		G_get_range_min_max (&range, &min, &max);
		G_make_rainbow_colors (colors, min, max);
		return 0;
	    }
	    err = "missing";
	    break;
    case -1:
	    err = "invalid";
	    break;
    default:
	    return 1;
    }

    sprintf(buf,"color support for [%s] in mapset [%s] %s", name, mapset, err);
    G_warning (buf);
    return -1;
}

static
read_colors (element, name, mapset, colors)
    char *element ;
    char *name ;
    char *mapset ;
    struct Colors *colors ;
{
    FILE *fd ;
    int stat;
    char buf[1024] ;

    if (!(fd = G_fopen_old (element, name, mapset)))
	return -2;

/*
 * first line in 4.0 color files is %
 * otherwise it is pre 4.0
 */
    if (fgets(buf,sizeof buf,fd) == NULL) 
    {
	fclose (fd);
	return -1;
    }
    fseek (fd, 0L, 0);

    G_strip (buf);
    if (*buf == '%') /* 4.0 format */
    {
	stat = read_new_colors (fd, colors);
	colors->version = 0; /* 4.0 format */
    }
    else
    {
	stat = read_old_colors (fd, colors);
	colors->version = -1; /* pre 4.0 format */
    }
    fclose (fd);
    return stat;
}

/* parse input lines with the following formats
 *   cat1:r:g:b cat2:r:g:b
 *   cat:r:g:b          (implies cat1==cat2)
 *
 * r:g:b can be just a single grey level
 *   cat1:x cat2:y
 *   cat:x
 *
 * optional lines are
 *    invert            invert color table
 *    shift:n           where n is the amount to shift the color table
 */
static
read_new_colors (fd, colors)
    FILE *fd;
    struct Colors *colors;
{
    long cat1, cat2;
    int r1,g1,b1;
    int r2,g2,b2;
    char buf[1024];
    char word1[256], word2[256];
    int n;
    int modular;
    int shift;

    G_init_colors (colors);

    if (fgets(buf,sizeof buf,fd) == NULL) 
	return -1;
    G_strip (buf);
    if(sscanf (buf+1, "%ld %ld", &cat1, &cat2) == 2)
	G_set_color_range ((CELL)cat1, (CELL)cat2, colors);
/*
   {
	if (cat1 < cat2)
	{
	    colors->cmin = cat1;
	    colors->cmax = cat2;
	}
	else
	{
	    colors->cmin = cat2;
	    colors->cmax = cat1;
	}
    }
*/

    modular = 0;
    while (fgets(buf, sizeof buf, fd))
    {
	*word1 = *word2 = 0;
	n = sscanf (buf, "%s %s", word1, word2);
	if (n < 1) continue;

	if (sscanf (word1, "shift:%d", &shift) == 1
	|| (strcmp (word1, "shift:") == 0 && sscanf (word2, "%d", &shift) == 1))
	{
	    G_shift_colors (shift, colors);
	    continue;
	}
	if (strcmp (word1, "invert") == 0)
	{
	    G_invert_colors (colors);
	    continue;
	}
	if (strcmp (word1, "%%") == 0)
	{
	    modular = !modular;
	    continue;
	}

	switch (sscanf (word1, "%ld:%d:%d:%d", &cat1, &r1, &g1, &b1))
	{
	    case 2: b1 = g1 = r1; break;
	    case 4: break;
	    default: continue;	/* other lines are ignored */
	}
	if (n == 2)
	{
	    switch (sscanf (word2, "%ld:%d:%d:%d", &cat2, &r2, &g2, &b2))
	    {
		case 2: b2 = g2 = r2; break;
		case 4: break;
		default: continue;	/* other lines are ignored */
	    }
	}
	else
	{
	    cat2 = cat1;
	    r2 = r1;
	    g2 = g1;
	    b2 = b1;
	}
	if (modular)
	    G_add_modular_color_rule ((CELL)cat1, r1, g1, b1,
				      (CELL)cat2, r2, g2, b2, colors);
	else
	    G_add_color_rule ((CELL)cat1, r1, g1, b1,
			      (CELL)cat2, r2, g2, b2, colors);
    }
    return 1;
}

static
read_old_colors (fd, colors)
    FILE *fd;
    struct Colors *colors ;
{
    char buf[256] ;
    long n ;
    long min;
    float red_f, grn_f, blu_f;
    int red, grn, blu;
    int old;
    int zero;

    G_init_colors (colors);
/*
 * first line in pre 3.0 color files is number of colors - ignore
 * otherwise it is #min first color, and the next line is for color 0
 */
    if (fgets(buf,sizeof buf,fd) == NULL) 
	return -1;

    G_strip (buf);
    if (*buf == '#') /* 3.0 format */
    {
	old = 0;
	if (sscanf (buf+1, "%ld", &min) != 1)	/* first color */
	    return -1;
	zero = 1;
    }
    else
    {
	old = 1;
	min = 0;
	zero = 0;
    }

    colors->cmin = min;
    n = min;
    while (fgets (buf, sizeof buf, fd))
    {
	if (old)
	{
	    if (sscanf (buf, "%f %f %f", &red_f, &grn_f, &blu_f) != 3)
		return -1;

	    red = 256 * red_f;
	    grn = 256 * grn_f;
	    blu = 256 * blu_f;
	}
	else
	{
	    switch (sscanf (buf, "%d %d %d", &red, &grn, &blu))
	    {
	    case 1: blu = grn = red; break;
	    case 2: blu = grn; break;
	    case 3: break;
	    default: return -1;
	    }
	}
	if (zero)
	{
	    G__insert_color_into_lookup ((CELL)0, red, grn, blu, &colors->fixed);
	    zero = 0;
	}
	else
	    G__insert_color_into_lookup ((CELL)n++, red, grn, blu, &colors->fixed);
    }
    colors->cmax = n-1;

    return 0 ;
}
