/* cell_site.h */
#ifndef CELL_SITE_H
#define CELL_SITE_H

#include <stdio.h>
#include "gis.h"

/* Holds the row and column indices, and the site's datum */
typedef struct cell_site {
    int    row, col;
    double datum;
} Cell_Site;

typedef struct site_counts {
    int count;   /* sites in Cell_Site *, or -1 for error */
    int discard; /* sites outside the region              */
} Site_Counts;

/* Takes a sites file pointer, the double attribute index, and the region
 * to use for filtering and looking up row/col indices.  Returns an array
 * of Cell_Site objects sorted in row major order, and then by the datum
 * of the site.  The return value of the function holds the number of sites
 * in the array and the number of sites discarded because they were outside
 * the region. If an error is encountered, a negative value is returned
 * for the count, whereas if no sites are in the region, zero will be 
 * returned.  In both cases the Cell_Site * will be NULL.
 */
Site_Counts
CS_Make_Index (FILE *              /*  in: sites file object           */
               ,int                /*  in: attribute field number      */
               ,struct Cell_head * /*  in: region for filter & row/col */
               ,Cell_Site **       /* out: array of Cell_Site          */
        );

#  ifdef DEBUG
/* dump the contents of the array to stdout */
void CS_dump (const Cell_Site *cs_array, int count);
#  endif
#endif
