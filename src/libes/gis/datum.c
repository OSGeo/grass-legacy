/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       coorcnv library
 * AUTHOR(S):    Andreas Lange - andreas.lange@rhein-main.de
 * PURPOSE: 	 provide functions for reading datum parameters from the
 *               location database.     
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include "gis.h"

static struct table
{
    char *name;   /* Name/acronym of map datum */
    char *descr;  /* description for map datum */
    char *ellps;  /* acronym for ellipsoid used with this datum */
    double dx;    /* delta x */
    double dy;    /* delta y */
    double dz;    /* delta z */
} *table;
static int size;
static int count = -1;

static int compare_table_names(const void *, const void *);
static void read_datum_table(void);

int 
G_get_datum_by_name(const char *name)
{
    int i;

    read_datum_table();

    for (i = 0; i < count; i++)
	if (G_strcasecmp(name, table[i].name) == 0)
	    return i;

    return -1;
}

/* this sets the datum shift parameters for datum pointed to by name */
int 
G_datum_shift(int n, double *dx, double *dy, double *dz)
{
    read_datum_table();

    if (n < 0 || n >= count) 
	return 0; 

    *dx = table[n].dx;
    *dy = table[n].dy;
    *dz = table[n].dz;
    return 1;
}

/* set the ellipsoid name and parameters for datum */
int 
G_datum_parameters(int n, char *ellps, double *dx, double *dy, double *dz)
{
    read_datum_table();

    if (n < 0 || n >= count) 
	return 0; 

    G_strcpy(ellps, table[n].ellps);
    *dx = table[n].dx;
    *dy = table[n].dy;
    *dz = table[n].dz;
    return 1;
}

char *
G_datum_name(int n)
{
    read_datum_table();

    if (n < 0 || n >= count) 
	return NULL; 

    return table[n].name;
}

char *
G_datum_description(int n)
{ 
    read_datum_table();
  
    if (n < 0 || n >= count)
	return NULL;

    return table[n].descr;
}

char *
G_datum_ellipsoid(int n)
{
    read_datum_table();

    if (n < 0 || n >= count)
	return NULL;

    return table[n].ellps;
}

static void
read_datum_table(void) 
{
    FILE *fd;
    char file[1024];
    char buf[1024];
    int line;

    if (count >= 0)
	return;

    count = 0;

    sprintf(file, "%s/etc/datum.table", G_gisbase());

    fd = fopen(file, "r");
    if (!fd)
    {
	G_warning("unable to open datum table file: %s", file);
	return;
    }

    for (line = 1; G_getl(buf, sizeof(buf), fd); line++)
    {
	char name[100], descr[100], ellps[100];
	double dx, dy, dz;
	struct table *t;

	G_strip(buf);
	if (*buf == '\0' || *buf == '#')
	    continue;

	if (count >= size)
	{
	    size += 50;
	    table = G_realloc(table, size * sizeof(struct table));
	}

	t = &table[count];

	if (sscanf(buf, "%s \"%99[^\"]\" %s dx=%lf dy=%lf dz=%lf",
		   name, descr, ellps, &t->dx, &t->dy, &t->dz) != 6)
	{
	    G_warning("error in datum table file, line %d", line);
	    continue;
	}

	t->name  = G_store (name);
	t->descr = G_store (descr);
	t->ellps = G_store (ellps);

	count++;
    }
 
    qsort(table, count, sizeof(struct table), compare_table_names);
}

static int
compare_table_names(const void *aa, const void *bb)
{
    const struct table *a = aa;
    const struct table *b = bb;

    return G_strcasecmp(a->name, b->name);
}
