/* cell_site.c */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "gis.h"
#include "site.h"
#include "cell_site.h"
#include "qisort.h"

#if defined(USE_QSORT) || defined(USE_QISORT)
/* need compare function */
static int cmp_cell_sites (const void *v1, const void *v2)
{
    const Cell_Site *cs1 = v1;
    const Cell_Site *cs2 = v2;

    if      (cs1->row   < cs2->row)   return -1;
    else if (cs1->row   > cs2->row)   return  1;
    else if (cs1->col   < cs2->col)   return -1;
    else if (cs1->col   > cs2->col)   return  1;
    else if (cs1->datum < cs2->datum) return -1;
    else if (cs1->datum > cs2->datum) return  1;
    else                              return  0;
}

#else
/* use shell sort - marginally faster than qsort for large sets */
#define less(A,B) ((A).row < (B).row \
                || ((A).row == (B).row && (A).col < (B).col) \
                || ((A).row == (B).row && (A).col == (B).col && (A).datum < (B).datum))

static void 
shsort(Cell_Site *array, size_t nmemb)
{
    /* This shell sort slightly modified from a version posted to
     * comp.programming and comp.lang.c by "pete <pfiland@mindspring.com>"
     */
    size_t h;
    Cell_Site v, *i, *j, *k, *after;

    after = array + nmemb;
    h = nmemb > ((size_t)-1) / 3 ? ((size_t)-1) / 3 : nmemb;
    while (h) {
        for (i = h + array; i != after; ++i) {
            k = i - h;
            if (less(*i, *k)) {
                j = i;
                v = *j;
                do {
                    *j = *k;
                     j =  k;
                    k -= h;
                } while (j >= h + array && less(v, *k));
                *j = v;
            }
        }
        h = h != 2 ? 3 * h / 7 : 1;
    }
}

#endif

Site_Counts
CS_Make_Index (FILE *ifp                 /*  in: sites file object           */
               ,int attrib               /*  in: attribute field number      */
               ,struct Cell_head *window /*  in: region for filter & row/col */
               ,Cell_Site **out          /* out: array of Cell_Site          */
        )
{
    Cell_Site  cs_work;
    Cell_Site *cs_array = NULL;
    char      *tempname = NULL;
    int        ndims = 0;
    int        ndbls = 0;
    int        nstrs = 0;
    int        cell_type = 0;
    Site      *theSite = NULL;
    FILE      *idx_file = NULL;
    Site_Counts counter = {0,0};
    
    assert (ifp != NULL);
    assert (attrib >= 0);
    assert (window != NULL);
    assert (out != NULL);

    if (G_site_describe (ifp, &ndims, &cell_type, &nstrs, &ndbls) != 0)
        goto Handle_Errors;

    if (attrib >= ndbls) {
        G_warning ("%s:%d: Decimal attribute %d does not exist", 
                __FILE__, __LINE__, attrib+1);
        goto Handle_Errors;
    }
    
    theSite = G_site_new_struct (cell_type, ndims, nstrs, ndbls);
    if (theSite == NULL) {
        G_warning ("%s:%d: G_site_new_struct (%d, %d, %d, %d) failed",
                __FILE__, __LINE__, cell_type, ndims, nstrs, ndbls);
        goto Handle_Errors;
    }

    tempname = G_tempfile();
    if (tempname == NULL) {
        G_warning ("%s:%d: G_tempfile() failed", __FILE__, __LINE__);
        goto Handle_Errors;
    }
    
    idx_file = fopen (tempname, "wb+");
    if (idx_file == NULL) {
        G_warning ("%s:%d: Couldn't open temporary file '%s' for writing",
                __FILE__, __LINE__, tempname);
        goto Handle_Errors;
    }
    
    while (G_site_get (ifp, theSite) == 0) {
        if (!G_site_in_region (theSite, window)) {
            counter.discard++;
            continue;
        }
        cs_work.row   = (int) G_northing_to_row (theSite->north, window);
        cs_work.col   = (int) G_easting_to_col (theSite->east, window);
        cs_work.datum = theSite->dbl_att[attrib];
        if (fwrite (&cs_work, sizeof cs_work, 1, idx_file) != 1) {
            G_warning ("%s:%d: Error writing to temporary file '%s'",
                    __FILE__, __LINE__, tempname);
            goto Handle_Errors;
        }
        counter.count++;
    }

    G_site_free_struct (theSite); theSite = NULL;
    
    if (counter.count > 0) {
        int i;
        cs_array = G_malloc (counter.count * sizeof *cs_array);
        rewind (idx_file);
        for (i = 0; i < counter.count; i++) {
            if (fread (&cs_array[i], sizeof *cs_array, 1, idx_file) != 1) {
                G_warning ("%s:%d: Error reading from temporary file '%s'",
                        __FILE__, __LINE__, tempname);
                goto Handle_Errors;
            }
        }
#if defined(USE_QSORT)
        qsort (cs_array, counter.count, sizeof *cs_array, cmp_cell_sites);
#elif defined(USE_QISORT)
        qisort (cs_array, counter.count, sizeof *cs_array, cmp_cell_sites);
#else
        shsort (cs_array, (size_t)counter.count);
#endif
#ifdef DEBUG
        CS_dump (cs_array, counter.count);
#endif
    }
    fclose (idx_file); idx_file = NULL; remove (tempname);
    G_free (tempname); tempname = NULL;

    *out = cs_array;
    return counter;
    
Handle_Errors:
    if (idx_file != NULL) {fclose (idx_file); remove (tempname);}
    if (theSite!= NULL)   G_site_free_struct (theSite);
    if (cs_array != NULL) G_free (cs_array);
    if (tempname != NULL) G_free (tempname); 
    counter.count = -1;
    return counter;
}

#  ifdef DEBUG
#include <float.h>
/* dump the contents of the array to stdout */
void CS_dump (const Cell_Site *cs_array, int count)
{
    int i;
    
    if (cs_array == NULL || count <= 0) return;
    printf ("DEBUG: CS_dump (%p, %d)\n", (void *)cs_array, count); 
    for (i = 0; i < count; i++)
        printf ("[%3d](%3d, %3d) = %1.*E\n", i, cs_array[i].row,
                cs_array[i].col, DBL_DIG, cs_array[i].datum);
    printf ("END\n");
}
#  endif
