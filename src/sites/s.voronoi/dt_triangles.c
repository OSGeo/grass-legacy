/* Andrea Aime on 23/04/2001:
   write_triangle reads Fortune's alghoritm (FA) output and writes the corresponding
   GRASS vector file. Since FA output triplets that are the three site index that
   form a triangle, every couple of point is represented two times, so a method to
   avoid duplication of lines is needed (otherwise v.spag -i is needed on the output).
   From the theory of Delaunay triangulation we know that a triangulation has
   3n - 3 - h edges, where h is the number of vertices forming the convex hull of
   sites. So we'll use this information to store an array of 3n-3 couple of points
   to detect collisions (there are at least 3 points on the convex hull, but...).
*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "site.h"

extern struct Cell_head window;


#define ALLOC_NEIGHBOUR 5   /* each site has at least 3 neighbours on Delaunay triang */
#define ALLOC_SITE 300      /* number of sites to alloc and increment for realloc */
struct DSite {
    double x,y;             /* site coordinates */
} DSite;
struct DLine{                    /* represents a line from s1, s2. If that couple is present */
    int s1, s2;             /* in the data structure the line has already been written to output */
    struct DLine *next;
} DLine;

struct DSite *ds;           /* site coordinates */
struct DLine *dl;           /* lines already written to output */
int numLines;               /* number of lines alreay written to output */
struct line_pnts *Points;

void write_new_line(int, int, struct Map_info*, int);

void write_triangles (struct Map_info *Map, FILE *fd_site,
                      char *tmp_vert, int lineType, int verbose)
{
    int i;
    int numRead;
    int numSites;
    int s1, s2, s3;
    SITE_XYZ *sites;
    double tmpx[2], tmpy[2];
    FILE *ptr;
    char buf[128];

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
    dl = NULL;
    numLines = 0;
    while (fgets (buf, 128, ptr) != NULL)
    {
        sscanf(buf, "%d %d %d", &s1, &s2, &s3);
        write_new_line(s1, s2, Map, lineType);
        write_new_line(s2, s3, Map, lineType);
        write_new_line(s3, s1, Map, lineType);
    }
    Vect_destroy_line_struct(Points);
    G_free(ds);
    G_free(dl);
}


void write_new_line(int sa, int sb, struct Map_info *Map, int lineType)
{
    int tmp;
    struct DLine *currLine, *prevLine, *newLine;
    int isNew;
    static int lastFound;
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
    isNew = TRUE;
    for(currLine = dl, prevLine = dl;
        currLine != NULL;
        prevLine = currLine, currLine = currLine->next)
    {
        if(currLine->s1 == sa && currLine->s2 == sb)
        {
            /* The duplicate has been found, remove site */
            if(prevLine == dl)
                dl = currLine->next;
            else
                prevLine->next = currLine->next;

            free(currLine);
            isNew = FALSE;
            break;
        }
    }

    if(isNew == TRUE)
    {
        /* create another newLine structure and append it to the list */
        newLine = (struct DLine*) G_malloc(sizeof(struct DLine));
        newLine->s1 = sa;
        newLine->s2 = sb;
        newLine->next = NULL;

        if(prevLine == dl)
        {
            newLine->next = dl;
            dl = newLine;
        } else {
            prevLine->next = newLine;
        }

        tmpx[0] = ds[sa].x; tmpx[1] = ds[sb].x;
        tmpy[0] = ds[sa].y; tmpy[1] = ds[sb].y;
        if (0 > Vect_copy_xy_to_pnts (Points, tmpx, tmpy, 2))
            G_fatal_error ("Out of memory");
        Vect_write_line (Map, lineType, Points);
    }
}
