/* 
 * $Id$
 *
 * plot() - vector reading
 */

#include <string.h>
#include "gis.h"
#include "Vect.h"

static
struct list
{
    double size;
    int index;
    CELL cat;
} *list;

static
struct cats
{
    CELL cat;
    int n_areas;
} *cats;

static int nareas ;
FILE *mif, *mid;

extern struct Cell_head window;
extern int linecolor;


plot (name, mapset, Points, filename, colors, Mapcats)
    char *name, *mapset, *filename;
    struct line_pnts *Points;
    struct Colors *colors;
    struct Categories *Mapcats;

{
    int i, j, ncolor, natt;
    struct Map_info Map;
    double *x, *y, px, py;
    int line, ncats;
    int sp;
    char midname[128], mifname[128];
    i = Vect_open_old (&Map, name, mapset);

    if (i < 2)
	G_fatal_error ("Failed opening vector file");

    strcpy(midname,filename);
    strcpy(mifname,filename);

    mif = G_fopen_new("mapinfo",strcat(mifname, ".mif"));
    mid = G_fopen_new("mapinfo",strcat(midname, ".mid"));

    nareas = sort_areas(&Map, Points);
    
    ncats = fillcats(&Map, nareas);
    j = 0;
    wmifheader();
    for (i = 0;i < ncats; i++)
    {

	wmifregion(cats[i].n_areas);
	if (Mapcats->num > 0)
		wmifcat(cats[i].cat, G_get_cat(cats[i].cat, Mapcats));
	else
		wmifcat(cats[i].cat, "");

	while (cats[i].cat == list[j].cat)
	{
		writepoly(&Map, list[j++].index, Points);
	}
	if (colors->version > -9) wmifcolor(colors, cats[i].cat);
    }
    fclose(mif); fclose(mid);
    Vect_close(&Map);
}

wmifcolor(colors, cat)
	struct Colors *colors;
	CELL cat;
{
	int r, g, b;
	int mifcolor;
	r = 0; g = 0; b = 0;
	G_get_color(cat, &r, &g, &b, colors);
	mifcolor = r * 65536 + 256 * g + b;
	fprintf(mif, "Brush(2, %d , %d)\n", mifcolor, mifcolor);
}

writepoly(Map, index, Points)
    struct Map_info *Map;
    struct line_pnts *Points;

{
	int area,j;

	switch (Vect_get_area_points(Map, index, Points))
	{
	case -1:
	    Vect_close (Map);
	    fprintf (stderr, "\nERROR: vector file - can't read\n");
	    return -1;
	    break;
	case -2:
	    printf ("Done\n");
	    Vect_close (Map);
	    return  0;
	    break;
	}
	
	wmifnop(Points->n_points);
	for(j=0; j < Points->n_points; j++)
	{
		wmifcoords(*(Points->x+j),*(Points->y+j));
	}
	for (j = 0; j < Map->Area[index].n_isles; j++)
		writeisle(Map, Map->Area[index].isles[j], Points);
}

writeisle(Map, index, Points)
    struct Map_info *Map;
    struct line_pnts *Points;

{
	int j;

	switch (Vect_get_isle_points(Map, index, Points))
	{
	case -1:
	    Vect_close (Map);
	    fprintf (stderr, "\nERROR: vector file - can't read\n");
	    return -1;
	    break;
	case -2:
	    printf ("Done\n");
	    Vect_close (Map);
	    return  0;
	    break;
	}
	
	wmifnop(Points->n_points);
	for(j=0; j < Points->n_points; j++)
	{
		wmifcoords(*(Points->x+j),*(Points->y+j));
	}
	return 1;
}



wmifheader()
{
	fprintf(mif,"Version 300\n");
	fprintf(mif,"Charset \"WindowsLatin1\"\n");
	fprintf(mif,"Delimiter \"|\"\n");
	fprintf(mif,"CoordSys Earth Projection 20, 109, \"m\", \n");
	fprintf(mif,"5.387638889, 52.156160556, 0.9999079, 155000, 463000\n");
	fprintf(mif,"Bounds (-99845000, -99537000)(100155000, 100463000)\n");
	fprintf(mif,"Columns 2\n");
	fprintf(mif,"id integer\n");
	fprintf(mif,"category char(128)\n");
	fprintf(mif,"Data\n\n");
}

wmifcat(cat, s)
{
	fprintf(mid,"%d|%s\n",(int) cat, s);
}

wmifregion(nareas)
{
	fprintf(mif,"Region %d\n",nareas);
}

wmifnop(n)
int n;
{
	fprintf(mif,"%d\n", n);
}

wmifcoords(x,y)
double x,y;
{
	fprintf(mif,"%10.1lf %10.1lf\n", x, y);
}



sort_areas (Map, Points)
    struct Map_info *Map;
    struct line_pnts *Points;
{
    int i,index;
    CELL cat;
    int compare();
    double G_area_of_polygon();

    G_begin_polygon_area_calculations();

/* first count valid areas */
    for (nareas = 0, index = 1; index <= Map->n_areas; index++)
    {
	if (area_ok(Map, index, &cat))
	    nareas++;
    }
    if (nareas == 0) return 0;

/* allocate list to hold valid area info */
    list = (struct list *) G_calloc (nareas, sizeof (struct list));

/* store area size,cat,index in list */
    for (i = 0, index = 1; index <= Map->n_areas; index++)
    {
	if (area_ok(Map, index, &cat))
	{
	    list[i].index = index;
	    list[i].cat = cat;
	    if(Vect_get_area_points (Map, index, Points) <= 0)
	    {
		fprintf (stderr, "*** Get area [%d] failed ***\n", index);
		return -1;
	    }
	    list[i].size = G_area_of_polygon (Points->x, Points->y, Points->n_points);
	    i++;
	}
    }

/* sort the list by size */
    qsort (list, nareas, sizeof(struct list), compare);
    return nareas;
}

static
compare (a, b)
    struct list *a, *b;
{
    return (a->cat - b->cat);
}

static
area_ok(Map, index, cat)
    struct Map_info *Map;
    CELL *cat;
{
    int att;

    if (!AREA_ALIVE(&Map->Area[index]))
	return 0;
    att = Map->Area[index].att;
    if (att == 0)
	*cat = 0; /* unlabeled */
    else
	*cat = Map->Att[att].cat;

    return 1;
}



fillcats(Map, nareas)
	struct Map_info *Map;
{
    int i, j, count;
    CELL oldcat;
    j = 0; count = 0; 
    oldcat = list[0].cat;
    cats = (struct cats *) G_calloc (Map->n_atts, sizeof (struct cats));
    for (i = 0; i < nareas; i++)
    {
	if (list[i].cat != oldcat) 
	{
		cats[j].cat = oldcat;
		cats[j++].n_areas = count;
		count = 0;
		oldcat = list[i].cat;
	}
	count += Map->Area[list[i].index].n_isles;
	count++;
    }
    cats[j].cat = oldcat;
    cats[j++].n_areas = count;
    return j;
}
	
	
