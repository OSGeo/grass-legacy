#include "gis.h"

struct EdgeList                  /* keep track of edge cells */
{
    struct CatEdgeList
    {
	CELL cat;                /* category number */
	int *row, *col;          /* arrays of pixels indexes */
	int ncells;              /* count of edges cells with this cat */
	int nalloc;              /* lenght of allocation for row,col */
    } *catlist;                  /* array of cat:edgelists */
    int ncats;                   /* number of cats */
    int nalloc;                  /* length of allocation for catlist */
    int count;                   /* total number of edge cells */
};

struct Map
{
    char *name;                  /* raster map name */
    char *mapset;                /* raster map mapset */
    char *fullname;              /* raster map fully qualified name */
    struct Categories labels;    /* category labels */
    struct EdgeList edges;       /* edge cells */
};

struct Parms
{
    struct Map map1, map2;       /* two raster maps to analyze */
    int verbose,                 /* boolean: verbose mode      */
	labels;                  /* boolean: report includes cat labels */
    char *fs;                    /* report field separator     */
};
