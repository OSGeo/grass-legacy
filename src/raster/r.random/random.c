#include "gis.h"
#include "site.h"
#include "local_proto.h"

void cpvalue (struct RASTER_MAP_PTR *from, int fcol, 
        struct RASTER_MAP_PTR *to, int tcol);
double cell_as_dbl(struct RASTER_MAP_PTR *buf, int col);
void set_to_null (struct RASTER_MAP_PTR *buf, int col);

int execute_random (struct rr_state *theState)
{
    long nt;
    long nc;
    struct Cell_head window;
    int nrows, ncols, row, col;
    int infd, outfd;
    FILE *sitefd;
    char msg[256];
    Site *mysite;
    Site_head site_info;

    G_get_window (&window);

    nrows = G_window_rows();
    ncols = G_window_cols();

    /* open the data files, input raster should be set-up already */
    infd = theState->fd_old;
    if (infd < 0)
    {
        sprintf (msg, "%s: unable to open raster file [%s]", G_program_name(),
                theState->inraster);
        G_fatal_error (msg);
        exit(1);
    }
    if (theState->outraster != NULL)
    {
        outfd = G_open_raster_new (theState->outraster, theState->buf.type);
        if (outfd < 0)
        {
            sprintf (msg, "%s: unable to create raster file [%s]", 
                G_program_name(), theState->outraster);
            G_fatal_error (msg);
            exit(1);
        }
        theState->fd_new = outfd;
    }
    if (theState->outsites)
    {
        mysite=G_site_new_struct(-1,2,0,1);
        sitefd = G_sites_open_new (theState->outsites);
        if (sitefd == NULL)
        {
            sprintf (msg, "%s: unable to create site file [%s]",
                G_program_name() , theState->outsites);
            G_fatal_error (msg);
            exit(1);
        }
        else
        {
            theState->fsites = sitefd;
            site_info.form=NULL;
            site_info.labels=NULL;
            site_info.time=NULL;
            site_info.stime=NULL;
            site_info.name=(char *)G_malloc(80*sizeof(char));
            site_info.desc=(char *)G_malloc(80*sizeof(char));
            if (site_info.desc==NULL || site_info.name==NULL)
              G_fatal_error("memory allocation error");
            sprintf (site_info.desc, 
                     "Random sites from [%s in %s]", 
                     theState->inraster, theState->mapset);
            sprintf (site_info.name, "%s", theState->outsites);
            G_site_put_head (sitefd, &site_info);
        }
    }

    if (theState->verbose)
    {
        fprintf (stderr, "Writing ");
        if (theState->outraster)
                fprintf (stderr, "raster file [%s] ", theState->outraster);
        if (theState->outsites && theState->outraster)
                fprintf (stderr, "and ");
        if (theState->outsites)
                fprintf (stderr, "site file [%s] ", theState->outsites);
        fprintf (stderr, "... ");
        G_percent (0, theState->nRand, 2);
    }

    init_rand();
    nc = (theState->use_nulls) ? theState->nCells : 
            theState->nCells - theState->nNulls ;
    nt = theState->nRand;
    for (row = 0; row < nrows && nt ; row++)
    {
        if (G_get_raster_row (infd, theState->buf.data.v, row, theState->buf.type) < 0)
        {
            sprintf (msg, "%s: can't read raster file [%s]",
                G_program_name(), theState->inraster);
            G_fatal_error (msg);
            exit(1);
        }

        for (col = 0; col < ncols && nt ; col++)
        {
            if (!theState->use_nulls && is_null_value(theState->buf, col))
                continue;

            if (make_rand() % nc < nt)
            {
                nt--;
                if (is_null_value(theState->buf, col))
                        cpvalue(&theState->nulls, 0, &theState->buf, col);

                if (theState->outsites)
                {
                    mysite->north = window.north - (row + .5) * window.ns_res;
                    mysite->east  = window.west  + (col + .5) * window.ew_res;
                    mysite->dbl_att[0] = cell_as_dbl(&theState->buf, col);
                    G_site_put (sitefd, mysite);
                }
                G_percent ((theState->nRand - nt), theState->nRand, 2);
            }
            else
                set_to_null(&theState->buf, col);

            nc-- ;
        }

        while (col < ncols)
                set_to_null(&theState->buf, col++);

        if (theState->outraster)
            G_put_raster_row(outfd, theState->buf.data.v, theState->buf.type);
    }

    /* Catch any remaining rows in the window*/
    if (theState->outraster && row < nrows)
    {
        for ( col = 0 ; col < ncols; col++)
            set_to_null(&theState->buf, col);
        
        for ( ; row < nrows ; row++)
            G_put_raster_row(outfd, theState->buf.data.v, theState->buf.type);
    }
            
    if (nt > 0)
        G_warning("%s: Only created %d random sites",
                G_program_name(), theState->nRand - nt);

    /* close files */
    G_close_cell(infd);
    if (theState->outsites)
        fclose (sitefd);
    if (theState->outraster)
        G_close_cell(outfd);

    return 0;
} /* execute_random() */


void cpvalue (struct RASTER_MAP_PTR *from, int fcol, 
        struct RASTER_MAP_PTR *to, int tcol)
{
    switch (from->type)
    {
        case CELL_TYPE:
            to->data.c[tcol] = from->data.c[fcol]; break;
        case FCELL_TYPE:
            to->data.f[tcol] = from->data.f[fcol]; break;
        case DCELL_TYPE:
            to->data.d[tcol] = from->data.d[fcol]; break;
    }
}

double cell_as_dbl(struct RASTER_MAP_PTR *buf, int col)
{
    switch(buf->type)
    {
        case CELL_TYPE:
            return (double) buf->data.c[col];
        case FCELL_TYPE:
            return (double) buf->data.f[col];
        case DCELL_TYPE:
            return (double) buf->data.d[col];
    }
}


void set_to_null (struct RASTER_MAP_PTR *buf, int col)
{
    switch(buf->type)
    {
        case CELL_TYPE:
            G_set_c_null_value(&(buf->data.c[col]), 1); break;
        case FCELL_TYPE:
            G_set_f_null_value(&(buf->data.f[col]), 1); break;
        case DCELL_TYPE:
            G_set_d_null_value(&(buf->data.d[col]), 1); break;
    }
}

int
is_null_value(buf, col)
    struct  RASTER_MAP_PTR buf;
    int     col;
{
    switch(buf.type)
    {
        case CELL_TYPE:
            return G_is_c_null_value(&buf.data.c[col]);
            break;
        case FCELL_TYPE:
            return G_is_f_null_value(&buf.data.f[col]);
            break;
        case DCELL_TYPE:
            return G_is_d_null_value(&buf.data.d[col]);
            break;
    }

    return -1;
}


/* vim: set softtabstop=4 shiftwidth=4 expandtab: */
