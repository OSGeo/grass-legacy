#include <limits.h>
#include <float.h>
#include <math.h>
#include <unistd.h>
#include "gis.h"
#include "local_proto.h"

void set_min(struct RASTER_MAP_PTR *from, int col, struct RASTER_MAP_PTR *to);
void set_max(struct RASTER_MAP_PTR *from, int col, struct RASTER_MAP_PTR *to);

/* get_stats() Find out the number of cells total, number of nulls
 * the min value, the max value and the create the null replacement
 * value.
 */
void get_stats (struct rr_state *theState)
{
    int nrows, ncols, row, col;

    theState->fd_old = G_open_cell_old (theState->inraster, theState->mapset);
    if (theState->fd_old < 0)
        G_fatal_error("%s: unable to open raster file <%s>",
                G_program_name(), theState->inraster);

    theState->buf.type = 
        G_raster_map_type (theState->inraster, theState->mapset);
    theState->buf.data.v = G_allocate_raster_buf (theState->buf.type);

    theState->nulls.type   = theState->buf.type;
    theState->min.type    = theState->buf.type;
    theState->max.type    = theState->buf.type;
    theState->nulls.data.v = (void *) G_malloc (G_raster_size (theState->nulls.type));
    theState->min.data.v  = (void *) G_malloc (G_raster_size (theState->min.type));
    theState->max.data.v  = (void *) G_malloc (G_raster_size (theState->max.type));
    
    nrows = G_window_rows();
    ncols = G_window_cols();

    theState->nCells = nrows * ncols;
    theState->nNulls = 0;
    set_min (NULL, 0, &theState->min);
    set_max (NULL, 0, &theState->max);

    if (theState->verbose)
        fprintf(stderr, "Collecting Stats ... ");
    for (row = 0; row < nrows; row++)
    {
        if (G_get_raster_row(theState->fd_old, theState->buf.data.v, 
                            row, theState->buf.type) < 0) {
            G_fatal_error ("%s: Failed to read entire raster", 
                    G_program_name());
        }

        for (col = 0; col < ncols; col++)
        {
            if (is_null_value(theState->buf, col)) {
                theState->nNulls++;
            }
            else
            {
                set_min(&theState->buf, col, &theState->min);
                set_max(&theState->buf, col, &theState->max);
            }
        } /* for (col ... ) */
        
        if (theState->verbose)
            G_percent(row, nrows, 2);
        
    } /* for (row ... ) */
        
    if (theState->verbose)
        G_percent(1, 1, 1);

    /* rewind the in raster file descriptor for later use */
    lseek(theState->fd_old, 0, SEEK_SET);

    /* Set the NULL value replacement */
    switch (theState->nulls.type)
    {
        case CELL_TYPE:
            *theState->nulls.data.c = *theState->min.data.c - 1; break;
        case FCELL_TYPE:
            *theState->nulls.data.f = floor(*theState->min.data.f - 1); break;
        case DCELL_TYPE:
            *theState->nulls.data.d = floor(*theState->min.data.d - 1); break;
        default: /* Huh? */
            G_fatal_error ("%s: Programmer error in get_stats/switch",
                    G_program_name());
    } /* switch() */
} /* get_stats() */


void set_min(struct RASTER_MAP_PTR *from, int col, struct RASTER_MAP_PTR *to)
{
    if (from == NULL) {
        switch (to->type)
        {
            case CELL_TYPE:
                *to->data.c = INT_MAX; break;
            case FCELL_TYPE:
                *to->data.f = FLT_MAX; break;
            case DCELL_TYPE:
                *to->data.d = DBL_MAX; break;
        }
    }
    else
    {
        switch (to->type)
        {
            case CELL_TYPE:
                *to->data.c = (*to->data.c < from->data.c[col]) ?
                        *to->data.c : from->data.c[col]; break;
            case FCELL_TYPE:
                *to->data.f = (*to->data.f < from->data.f[col]) ?
                        *to->data.f : from->data.f[col]; break;
            case DCELL_TYPE:
                *to->data.d = (*to->data.d < from->data.d[col]) ?
                        *to->data.d : from->data.d[col]; break;
        }
    }
}


void set_max(struct RASTER_MAP_PTR *from, int col, struct RASTER_MAP_PTR *to)
{
    if (from == NULL) {
        switch (to->type)
        {
            case CELL_TYPE:
                *to->data.c = INT_MIN; break;
            case FCELL_TYPE:
                *to->data.f = FLT_MIN; break;
            case DCELL_TYPE:
                *to->data.d = DBL_MIN; break;
        }
    }
    else
    {
        switch (to->type)
        {
            case CELL_TYPE:
                *to->data.c = (*to->data.c > from->data.c[col]) ?
                        *to->data.c : from->data.c[col]; break;
            case FCELL_TYPE:
                *to->data.f = (*to->data.f > from->data.f[col]) ?
                        *to->data.f : from->data.f[col]; break;
            case DCELL_TYPE:
                *to->data.d = (*to->data.d > from->data.d[col]) ?
                        *to->data.d : from->data.d[col]; break;
        }
    }
}

/* vim: set softtabstop=4 shiftwidth=4 expandtab: */
