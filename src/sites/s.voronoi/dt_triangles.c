/* Andrea Aime on 23/04/2001:
   write_triangle reads Fortune's alghoritm (FA) output and writes the
   corresponding GRASS vector file. Since FA output triplets that are the
   three site index that form a triangle, every couple of point is
   represented two times, so a method to avoid duplication of lines is
   needed (otherwise v.spag -i is needed on the output).
   From the theory of Delaunay triangulation we know that a triangulation
   has 3n - 3 - h edges, where h is the number of vertices on the
   convex hull of sites. So we'll use this information to store an array of
   3n-3-3 couple of points to detect collisions (since there are at least 3 points
   on the convex hull).

   Markus Neteler:
   added category support, added area size support   
*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "site.h"
#include "linkm.h"

extern struct Cell_head window;


#define ALLOC_NEIGHBOUR 5   /* each site has at least 3 neighbours on Delaunay triang */
#define ALLOC_SITE 300      /* number of sites to alloc and increment for realloc */
struct DSite {
    double x,y;             /* site coordinates */
} DSite;

struct DLine{               /* represents a line from s1, s2. If that couple is present */
    int s1, s2;             /* in the data structure the line has already been written to output */
} DLine;

struct DSite *ds;           /* site coordinates */
struct DLine *dl;           /* lines already written to output */
int numWrittenLines;        /* number of lines alreay written to output */
struct line_pnts *Points;

void init_lines_array(int);
int write_new_line(int, int, struct Map_info*, int);
double get_triangle_area(int, int, int);
void write_line_att(int, int, FILE*, int);

void dt_write_triangles (struct Map_info *Map, FILE *fd_site, FILE *f_att, char
 vname[512], char *tmp_vert, int lineType, int verbose, int cat_area)
{
    int i;
    int numRead;
    int numSites;
    int s1, s2, s3;
    SITE_XYZ *sites;
    FILE *ptr;
    char buf[128];
    int catnum;                 /* category number */
    double f_area;
    struct Categories cats;
    char vlabel[80];
    int currLine, currArea, numAreas;
    double baricenter_x, baricenter_y;

    G_init_cats((CELL) 0, vname, &cats);

    catnum = 0;

    /* Read sites and prepare data structures */
    sites = G_alloc_site_xyz(ALLOC_SITE);
    numSites = 0;
    while((numRead = G_readsites_xyz(fd_site, SITE_COL_NUL, 0,
                                     ALLOC_SITE, &window, sites)) > 0)
    {
        ds = (struct DSite*) G_realloc(ds, (numSites + numRead) * sizeof(DSite));
        for(i = numSites; i < numSites + numRead; i ++)
        {
            ds[i].x = sites[i - numSites].x;
            ds[i].y = sites[i - numSites].y;
        }
        numSites = numSites + numRead;
    }
    G_free_site_xyz(sites);

    /* read triangle borders and write output */
    if ((ptr = fopen (tmp_vert, "r")) == NULL)
        G_fatal_error ("Cannot open temporary file");

    Points = Vect_new_line_struct ();

    if ((lineType == 2) && cat_area)
    {
       G_begin_polygon_area_calculations();
       fprintf(stderr, "Storing area information as category label\n");
    }
    else
       fprintf(stderr, "Storing triangle number as category label\n");

    /* write out triangles and count triangle number */
    init_lines_array(numSites);
    currArea = 0;
    currLine = 0;
    while (fgets (buf, 128, ptr) != NULL)
    {
        int new1, new2, new3;

        sscanf(buf, "%d %d %d", &s1, &s2, &s3);
        new1 = write_new_line(s1, s2, Map, lineType);
        new2 = write_new_line(s2, s3, Map, lineType);
        new3 = write_new_line(s3, s1, Map, lineType);

        if(lineType == 1)
        {
            if(new1) write_line_att(s1, s2, f_att, ++currLine);
            if(new2) write_line_att(s2, s3, f_att, ++currLine);
            if(new3) write_line_att(s3, s1, f_att, ++currLine);
        }
        else
        {
            currArea++;
            baricenter_x = (ds[s1].x + ds[s2].x + ds[s3].x) / 3;
            baricenter_y = (ds[s1].y + ds[s2].y + ds[s3].y) / 3;

            /* seen in v.alabel/main.c: */
            write_att (f_att, 'A', baricenter_x, baricenter_y, currArea);

            /* write cats */
            if (cat_area) /* we want the area stored instead of triangle number */
            {
                /* note: check for -l flag is already done in main() */

                /* Calculate polygon area. Note, no islands to be expected since
                   we have triangles - MN  */

                f_area = get_triangle_area(s1, s2, s3);
                sprintf(vlabel, "%f", f_area); /* write area size */
            }
            else
                sprintf(vlabel, "%i", currArea);  /* write area number instead of area */

            /* set vector cat for this area */
            if(G_set_cat(currArea, vlabel, &cats) < 0)
                G_fatal_error("Can't set vector cats.");
        }

    } /* while */

    Vect_destroy_line_struct(Points);

    fprintf(stderr, "Writing vector cats\n");
    if(G_write_vector_cats(vname, &cats) < 0)
       G_fatal_error("Can't write dig_cats file.");
    G_free_cats(&cats);

    G_free(ds);
    G_free(dl);
}

double segment_length(int s1, int s2)
{
    return sqrt((ds[s1].x - ds[s2].x) * (ds[s1].x - ds[s2].x) +
               (ds[s1].y - ds[s2].y) * (ds[s1].y - ds[s2].y));
}

/* Calculate triangle area using Heron's formula */
double get_triangle_area(int s1, int s2, int s3)
{
    double l1, l2, l3, s;
    l1 = segment_length(s1, s2);
    l2 = segment_length(s2, s3);
    l3 = segment_length(s3, s1);
    s = (l1 + l2 + l3) / 2;

    return sqrt(s * (s - l1) * (s - l2) * (s - l3)) ;
}

void write_line_att(int s1, int s2, FILE* f_att, int catNum)
{
    double baricenter_x, baricenter_y;

    baricenter_x = (ds[s1].x + ds[s2].x) / 2;
    baricenter_y = (ds[s1].y + ds[s2].y) / 2;

    write_att (f_att, 'L', baricenter_x, baricenter_y, catNum);
}

/* No more than 3n-3-3 sites in the triangulation */
void init_lines_array(int numSites)
{
	dl = (struct DLine*) G_malloc((3 * numSites - 6) * sizeof(struct DLine));
  numWrittenLines = 0;
}

int write_new_line(int sa, int sb, struct Map_info *Map, int lineType)
{
    int tmp;
    int i;
    int isNew;
    double tmpx[2], tmpy[2];

    /* sort sites */
    if(sa > sb)
    {
        tmp = sb;
        sb = sa;
        sa = tmp;
    }

    /* new line? Scan dl array. */
    /* I only need s1==sa, s2==sb because I know that sa<sb */
    for(i = 0; i < numWrittenLines; i++)
        if(dl[i].s1 == sa && dl[i].s2 == sb)
            break;
	
    isNew = (i == numWrittenLines) ? TRUE : FALSE;

    if(isNew == TRUE) /* new Line */
    {
        /* store the information in the written lines array */
        dl[numWrittenLines].s1 = sa;
        dl[numWrittenLines].s2 = sb;
        numWrittenLines++;

        /* write line to output */
        tmpx[0] = ds[sa].x; tmpx[1] = ds[sb].x;
        tmpy[0] = ds[sa].y; tmpy[1] = ds[sb].y;
        if (0 > Vect_copy_xy_to_pnts (Points, tmpx, tmpy, 2))
            G_fatal_error ("Out of memory");
        Vect_write_line (Map, lineType, Points);
    }

    return isNew == TRUE;
}
