/**********************************************************************
 *
 *  G_read_colors (name, mapset, pcolr)
 *      char *name                   name of map
 *      char *mapset                 mapset that map belongs to
 *      struct Colors *pcolr         structure to hold color info
 *
 *  Reads the color information associated with map layer "map"
 *  in mapset "mapset" into the structure "pcolr".
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
 *********************************************************************
 *
 * G_init_colors (pcolr)
 *      struct Colors *pcolr         structure to hold color info
 *
 * Initializes the color structure for subsequent calls to G_set_color()
 *
 *********************************************************************
 *
 * G_set_color (n, red, grn, blu, pcolr)
 *      CELL n                      color number to set
 *      int red, grn, blu           color values (0-255)
 *      struct Colors *pcolr        structure with color info
 *
 * returns: -1 too many colors, 1 ok
 *********************************************************************
 *
 * G_get_color (n, red, grn, blu, pcolr)
 *      CELL n                      color number to get
 *      int *red, *grn, *blu        color values (0-255) for color n
 *      struct Colors *pcolr        structure with color info
 *********************************************************************
 *
 * G_free_colors (pcolr)
 *      struct Colors *pcolr         color structure to free
 *
 * Frees memory allocated for the color structure
 *********************************************************************
 *
 *  G_write_colors (name, mapset, pcolr)
 *      char *name                   name of map
 *      char *mapset                 mapset that map belongs to
 *      struct Colors *pcolr         structure holding color info
 *
 *  Writes the color information associated with map layer "map"
 *  in mapset "mapset" from the structure "pcolr".
 *
 *  returns:    1  if successful
 *             -1  on fail
 *
 ***********************************************************************/

#include "gis.h"

G_read_colors (name, mapset, pcolr)
    char *name ;
    char *mapset ;
    struct Colors *pcolr ;
{
    char buf[100];
    char *err;
    char rname[256], rmapset[256];
    struct Range range;

    if (G__name_in_mapset (name, rname, rmapset))
    {
	if (strcmp (rmapset, mapset) != 0)
	    return -1;
	name = rname;
    }

/* first look for secondary color table in current mapset */
    sprintf (buf,"colr2/%s", mapset);
    if (G__read_colors (buf, name, G_mapset(), pcolr) >= 0)
	    return 1;

/* now look for the regular color table */
    switch (G__read_colors ("colr", name, mapset, pcolr))
    {
    case -2:
	    if (G_read_range (name, mapset, &range) >= 0)
	    {
		G_init_colors (pcolr);
		G_make_rainbow_colors (pcolr,
			range.nmin ? range.nmin : range.pmin,
			range.pmax ? range.pmax : range.nmax);
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

G__read_colors (element, name, mapset, pcolr)
    char *element ;
    char *name ;
    char *mapset ;
    struct Colors *pcolr ;
{
    FILE *fd ;
    char buf[256] ;
    long n ;
    long min;
    float red_f, grn_f, blu_f;
    int red, grn, blu;
    int old;
    int zero;

    G_init_colors (pcolr);
    if (!(fd = G_fopen_old (element, name, mapset)))
	return -2;

/*
 * first line in pre 3.0 color files is number of colors - ignore
 * otherwise it is #min first color, and the next line is for color 0
 */
    if (fgets(buf,sizeof buf,fd) == NULL) 
    {
	fclose (fd);
	return -1;
    }
    G_strip (buf);
    if (*buf == '#') /* 3.0 format */
    {
	old = 0;
	if (sscanf (buf+1, "%ld", &min) != 1)	/* first color */
	{
	    fclose (fd);
	    return -1;
	}
	zero = 1;
    }
    else
    {
	old = 1;
	min = 0;
	zero = 0;
    }

    n = min;
    while (fgets (buf, sizeof buf, fd))
    {
	if (old)
	{
	    if (sscanf (buf, "%f %f %f", &red_f, &grn_f, &blu_f) != 3)
	    {
		fclose (fd);
		return -1;
	    }
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
	    default: fclose (fd); return -1;
	    }
	}
	if (zero)
	{
	    G_set_color ((CELL)0, red, grn, blu, pcolr);
	    zero = 0;
	}
	else
	    G_set_color ((CELL)n++, red, grn, blu, pcolr);
    }
    fclose (fd);

    return 0 ;
}

G_init_colors (pcolr)
    struct Colors *pcolr ;
{
    pcolr->min = 0;
    pcolr->max = -1;
    pcolr->red = NULL;
    pcolr->grn = NULL;
    pcolr->blu = NULL;
    pcolr->r0 = pcolr->g0 = pcolr->b0 = 255;
    pcolr->nalloc = 0;
}

G_free_colors (pcolr)
    struct Colors *pcolr ;
{
    if (pcolr->red)
	free (pcolr->red);
    if (pcolr->grn)
	free (pcolr->grn);
    if (pcolr->blu)
	free (pcolr->blu);
}

G_get_color (n, red, grn, blu, pcolr)
    CELL n;
    int *red, *grn, *blu;
    struct Colors *pcolr ;
{
    if (n == 0)
    {
	*red = pcolr->r0;
	*grn = pcolr->g0;
	*blu = pcolr->b0;
    }
    else if (pcolr->min <= n && n <= pcolr->max)
    {
	n -= pcolr->min;
	*red = pcolr->red[n];
	*grn = pcolr->grn[n];
	*blu = pcolr->blu[n];
    }
    else
	*red = *grn = *blu = 255; /* white */
}

G_get_color_array (cell, r, g, b, pcolr, n)
    CELL *cell;
    uchar *r, *g, *b;
    struct Colors *pcolr ;
    register int n;
{
    register CELL c;
    CELL min,max;
    uchar *red, *grn, *blu;

    red = pcolr->red;
    grn = pcolr->grn;
    blu = pcolr->blu;
    min = pcolr->min;
    max = pcolr->max;

    while (--n >= 0)
    {
	c = *cell++;
	if (c == 0)
	{
	    *r++ = pcolr->r0;
	    *g++ = pcolr->g0;
	    *b++ = pcolr->b0;
	}
	else if (min <= c && c <= max)
	{
	    c -= min;
	    *r++ = red[c];
	    *g++ = grn[c];
	    *b++ = blu[c];
	}
	else
	    *r++ = *g++ = *b++ = 255; /* white */
    }
}

G_set_color (n, red, grn, blu, pcolr)
    CELL n;
    int red, grn, blu;
    struct Colors *pcolr ;
{
    long nalloc;
    long i;
    long newlen, curlen, gap;

    if (red < 0) red = 0;
    if (red > 255) red = 255;
    if (grn < 0) grn = 0;
    if (grn > 255) grn = 255;
    if (blu < 0) blu = 0;
    if (blu > 255) blu = 255;

/* color number 0 is a special case */
    if (n == 0)
    {
	pcolr->r0 = red;
	pcolr->g0 = grn;
	pcolr->b0 = blu;
	return 1;
    }

/* first color? */
    if (pcolr->nalloc == 0)
    {
	pcolr->nalloc = 256;
	pcolr->red = (uchar *) G_malloc (pcolr->nalloc);
	pcolr->grn = (uchar *) G_malloc (pcolr->nalloc);
	pcolr->blu = (uchar *) G_malloc (pcolr->nalloc);
	pcolr->max = pcolr->min = n;
    }
/* extend the color table? */
    else if (n > pcolr->max)
    {
	curlen = pcolr->max - pcolr->min + 1;
	newlen = n - pcolr->min + 1;
	nalloc = newlen;
	if (nalloc != (int) nalloc)        /* check for int overflow */
	    return -1;

	if (nalloc > pcolr->nalloc)
	{
	    while (pcolr->nalloc < nalloc)
		pcolr->nalloc += 256;
	    nalloc = pcolr->nalloc;

	    pcolr->red = (uchar *) G_realloc (pcolr->red, (int)nalloc);
	    pcolr->grn = (uchar *) G_realloc (pcolr->grn, (int)nalloc);
	    pcolr->blu = (uchar *) G_realloc (pcolr->blu, (int)nalloc);
	}

	/* fill in gap with white */
	for (i = curlen; i < newlen; i++)
	{
	    pcolr->red[i] = 255;
	    pcolr->grn[i] = 255;
	    pcolr->blu[i] = 255;
	}
	pcolr->max = n;
    }
    else if (n < pcolr->min)
    {
	curlen = pcolr->max - pcolr->min + 1;
	newlen = pcolr->max - n + 1;
	gap    = newlen - curlen;
	nalloc = newlen;
	if (nalloc != (int) nalloc)        /* check for int overflow */
	    return -1;

	if (nalloc > pcolr->nalloc)
	{
	    while (pcolr->nalloc < nalloc)
		pcolr->nalloc += 256;
	    nalloc = pcolr->nalloc;

	    pcolr->red = (uchar *) G_realloc (pcolr->red, (int)nalloc);
	    pcolr->grn = (uchar *) G_realloc (pcolr->grn, (int)nalloc);
	    pcolr->blu = (uchar *) G_realloc (pcolr->blu, (int)nalloc);
	}

    /* shift the table to make room in front */
	for (i = 1; i <= curlen; i++)
	{
	    pcolr->red[newlen-i] = pcolr->red[curlen-i];
	    pcolr->grn[newlen-i] = pcolr->grn[curlen-i];
	    pcolr->blu[newlen-i] = pcolr->blu[curlen-i];
	}

    /* fill in gap with white */
	for (i=1; i < gap; i++)
	{
	    pcolr->red[i] = 255 ;
	    pcolr->grn[i] = 255 ;
	    pcolr->blu[i] = 255 ;
	}
	pcolr->min = n;
    }

/* set the color! */
    i = n - pcolr->min;
    pcolr->red[i] = red;
    pcolr->grn[i] = grn;
    pcolr->blu[i] = blu;

    return 1;
}

G_write_colors (name, mapset, pcolr)
    char *name ;
    char *mapset ;
    struct Colors *pcolr ;
{
    char element[256];
    char rname[256], rmapset[256];

    if (G__name_in_mapset (name, rname, rmapset))
    {
	if (strcmp (rmapset, mapset) != 0)
	    return -1;
	name = rname;
    }
/*
 * if mapset is the current mapset and there is no color table
 * write it to the original file. else write it to the secondary
 * color file
 */
/* This code is removed and replaced with the lines below
    strcpy (element, "colr");
    if (strcmp (mapset, G_mapset()) || G_find_file (element, name, G_mapset()))
	sprintf (element, "colr2/%s", mapset);
*/
/*
 * if mapset is current mapset, remove colr2 file (created by pre 3.0 grass)
 * and then write original color table
 * else write secondary color table
 */
    sprintf (element, "colr2/%s", mapset);
    if (strcmp (mapset, G_mapset()) == 0)
    {
	G_remove (element, name);	/* get rid of existing colr2, if any */
	strcpy (element, "colr");
    }

    return G__write_colors (name, element, pcolr) ;
}

G__write_colors (name, element, pcolr)
    char *name ;
    char *element ;
    struct Colors *pcolr ;
{
    FILE *fd ;
    int i ;
    int n;


    if (!(fd = G_fopen_new (element, name)))
	return -1;

    fprintf (fd, "#%ld first color\n", (long)pcolr->min) ;
    fprintf (fd, "%d %d %d\n", (int)pcolr->r0, (int)pcolr->g0, (int)pcolr->b0);

    n = pcolr->max - pcolr->min + 1;
    for (i=0; i < n; i++)  
    {
	fprintf ( fd, "%d", (int)pcolr->red[i]);
	if (pcolr->red[i] != pcolr->grn[i] || pcolr->red[i] != pcolr->blu[i])
	    fprintf ( fd, " %d %d", (int)pcolr->grn[i], (int)pcolr->blu[i]) ;
	fprintf (fd, "\n");
    }

    fclose (fd);

    return 1;
}
