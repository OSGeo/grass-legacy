/**********************************************************************
 *  G_write_colors (name, mapset, colors)
 *      char *name                   name of map
 *      char *mapset                 mapset that map belongs to
 *      struct Colors *colors        structure holding color info
 *
 *  Writes the color information associated with map layer "map"
 *  in mapset "mapset" from the structure "colors".
 *
 *  returns:    1  if successful
 *             -1  on fail
 *
 * If the environment variable FORCE_GRASS3_COLORS is set (to anything at all)
 * then the output format is 3.0, even if the structure contains 4.0 rules.
 * This allows users to create 3.0 color files for export to sites which
 * don't yet have 4.0
 ***********************************************************************/
#include "gis.h"

G_write_colors (name, mapset, colors)
    char *name ;
    char *mapset ;
    struct Colors *colors ;
{
    char element[512];
    char xname[512], xmapset[512];
    FILE *fd;
    int stat;

    if (G__name_is_fully_qualified (name, xname, xmapset))
    {
	if (strcmp (xmapset, mapset) != 0)
	    return -1;
	name = xname;
    }
/*
 * if mapset is current mapset, remove colr2 file (created by pre 3.0 grass)
 *    and then write original color table
 * else write secondary color table
 */
    sprintf (element, "colr2/%s", mapset);
    if (strcmp (mapset, G_mapset()) == 0)
    {
	G_remove (element, name);	/* get rid of existing colr2, if any */
	strcpy (element, "colr");
    }
    if (!(fd = G_fopen_new (element, name)))
	return -1;

    stat = G__write_colors (fd, colors) ;
    fclose (fd);
    return stat;
}

G__write_colors (fd, colors)
    FILE *fd;
    struct Colors *colors;
{
    char *getenv();

    if (getenv("FORCE_GRASS3_COLORS"))
	return forced_write_old_colors (fd, colors);
    else if (colors->version < 0)
	return write_old_colors (fd, colors);
    else
	return write_new_colors (fd, colors);
}

static
write_new_colors (fd, colors)
    FILE *fd;
    struct Colors *colors;
{
    fprintf(fd, "%% %ld %ld\n", (long) colors->cmin, (long) colors->cmax);

    if (colors->shift)
	fprintf (fd, "shift:%d\n", colors->shift);
    if (colors->invert)
	fprintf (fd, "invert\n");

    if (colors->modular.rules)
    {
	fprintf (fd, "%s\n","%%");
	write_rules(fd, colors->modular.rules);
	fprintf (fd, "%s\n","%%");
    }
    if (colors->fixed.rules)
	write_rules(fd, colors->fixed.rules);

    return 1;
}

static
write_rules(fd, crules)
    FILE *fd;
    struct _Color_Rule_ *crules;
{
    struct _Color_Rule_ *rule;

/* find the end of the rules list */
    rule = crules;
    while (rule->next)
	rule = rule->next;

/* write out the rules in reverse order */
    for ( ; rule; rule = rule->prev)
    {
	fprintf (fd, "%ld:%d", (long)rule->low.cat, (int) rule->low.red);
	if (rule->low.red != rule->low.grn || rule->low.red != rule->low.blu)
	    fprintf (fd, ":%d:%d", rule->low.grn, rule->low.blu);
	if (rule->low.cat != rule->high.cat)
	{
	    fprintf (fd, " %ld:%d", (long)rule->high.cat, (int) rule->high.red);
	    if (rule->high.red != rule->high.grn || rule->high.red != rule->high.blu)
		fprintf (fd, ":%d:%d", rule->high.grn, rule->high.blu);
	}
	fprintf (fd, "\n");
    }
}

static
write_old_colors (fd, colors)
    FILE *fd;
    struct Colors *colors;
{
    int i,n;

    fprintf (fd, "#%ld first color\n", (long)colors->fixed.min) ;
    fprintf (fd, "%d %d %d\n",
	(int)colors->fixed.lookup.r0,
	(int)colors->fixed.lookup.g0,
	(int)colors->fixed.lookup.b0);

    n = colors->fixed.max - colors->fixed.min + 1;

    for (i=0; i < n; i++)  
    {
	fprintf ( fd, "%d", (int)colors->fixed.lookup.red[i]);
	if (colors->fixed.lookup.red[i] != colors->fixed.lookup.grn[i] 
	||  colors->fixed.lookup.red[i] != colors->fixed.lookup.blu[i])
	    fprintf ( fd, " %d %d",
		(int)colors->fixed.lookup.grn[i],
		(int)colors->fixed.lookup.blu[i]) ;
	fprintf (fd, "\n");
    }

    return 1;
}

static
forced_write_old_colors (fd, colors)
    FILE *fd;
    struct Colors *colors;
{
    int red,grn,blu;
    CELL cat;

    fprintf (fd, "#%ld first color\n", (long)colors->cmin) ;
    G_get_color ((CELL)0, &red, &grn, &blu, colors);
    fprintf (fd, "%d %d %d\n", red, grn, blu);

    for (cat = colors->cmin; cat <= colors->cmax; cat++)
    {
	G_get_color (cat, &red, &grn, &blu, colors);
	fprintf ( fd, "%d", red);
	if (red != grn || red != blu)
	    fprintf ( fd, " %d %d", grn, blu);
	fprintf (fd, "\n");
    }

    return 1;
}
