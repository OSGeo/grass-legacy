#include "gis.h"
#include "Vect.h"

static int *area_list, nareas;;
static int *line_list, nlines;
static struct Map_info *Map;
static int start = -1;

static int cmp_areas1(a,b)
	int *a, *b;
{
	return V2_area_att (Map, *a) - V2_area_att(Map, *b);
}

static int cmp_areas2(a,n)
	int *a, n;
{
	return *a - V2_area_att(Map, area_list[n]);
}

static int cmp_lines1(a,b)
	int *a, *b;
{
	return V2_line_att (Map, *a) - V2_line_att(Map, *b);
}

static int cmp_lines2(a,n)
	int *a, n;
{
	return *a - V2_line_att(Map, line_list[n]);
}

build_lookup_tables (M)
	struct Map_info *M;
{
	int i;

	Map = M;

	nareas = V2_num_areas(M);
	if (nareas > 0)
	{
		area_list = (int *)G_calloc (nareas, sizeof(int));
		for (i=0; i < nareas; i++)
			area_list[i] = i+1;
		qsort (area_list, nareas, sizeof(*area_list), cmp_areas1);
	}

	nlines = V2_num_lines(M);
	if (nlines > 0)
	{
		line_list = (int *)G_calloc (nlines, sizeof(int));
		for (i=0; i < nlines; i++)
			line_list[i] = i+1;
		qsort (line_list, nlines, sizeof(*line_list), cmp_lines1);
	}
}

int *
find_area (cat, count, M)
	int cat;
	int *count;
	struct Map_info *M;
{
	Map = M;
	if (nareas <= 0)
	{
		*count = 0;
		return NULL;
	}
	*count = bin_search (&cat, nareas, &start, cmp_areas2);
	if (*count) return area_list + start;
	else return NULL;
}
int *
find_line (cat, count, M)
	int cat;
	int *count;
	struct Map_info *M;
{
	Map = M;
	if (nlines <= 0)
	{
		*count = 0;
		return NULL;
	}
	*count = bin_search (&cat, nlines, &start, cmp_lines2);
	if (*count) return line_list + start;
	else return NULL;
}
