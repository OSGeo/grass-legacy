#include <stdlib.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
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
    char msg[256];
    struct Map_info Out;
    struct field_info *fi;
    dbTable *table;
    dbColumn *column;
    dbString sql;
    dbDriver *driver;
    struct line_pnts *Points;
    struct line_cats *Cats;
    int cat;

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
        if (theState->z_geometry)
	  Vect_open_new (&Out, theState->outsites, 1);
	else
	  Vect_open_new (&Out, theState->outsites, 0);
        Vect_hist_command ( &Out );

        fi = Vect_default_field_info ( &Out, 1, NULL, GV_1TABLE );

        driver = db_start_driver_open_database ( fi->driver, Vect_subst_var(fi->database,&Out) );
        if ( !driver )
            G_fatal_error ( "Cannot open database %s with driver %s", 
                             Vect_subst_var(fi->database,&Out), fi->driver );
        
        Vect_map_add_dblink ( &Out, 1, NULL, fi->table, "cat", fi->database, fi->driver);

        table = db_alloc_table ( 2 );
        db_set_table_name ( table, fi->table );
        
        column = db_get_table_column (table, 0);
        db_set_column_name ( column,  "cat" );
        db_set_column_sqltype ( column, DB_SQL_TYPE_INTEGER ); 

        column = db_get_table_column (table, 1);
        db_set_column_name ( column,  "value" );
        db_set_column_sqltype ( column, DB_SQL_TYPE_DOUBLE_PRECISION ); 

        if ( db_create_table ( driver, table ) != DB_OK )
            G_warning ( "Cannot create new table" );

        db_begin_transaction ( driver );
        
        Points = Vect_new_line_struct ();
        Cats = Vect_new_cats_struct ();
        db_init_string (&sql);
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
    cat = 1;
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
                    double x, y, val;
                    char buf[500];

                    Vect_reset_line ( Points );
                    Vect_reset_cats ( Cats );
                    
                    x = window.west  + (col + .5) * window.ew_res;
                    y = window.north - (row + .5) * window.ns_res;
                    
                    val = cell_as_dbl(&theState->buf, col);

                    if (theState->z_geometry)
		      Vect_append_point ( Points, x, y, val );
		    else
		      Vect_append_point ( Points, x, y, 0.0 );
                    Vect_cat_set (Cats, 1, cat);

                    Vect_write_line ( &Out, GV_POINT, Points, Cats );

                    sprintf (buf, "insert into %s values ( %d, %f )", fi->table, cat, val );
                    db_set_string ( &sql, buf );
                    
                    if (db_execute_immediate (driver, &sql) != DB_OK )
                        G_fatal_error ( "Cannot insert new row: %s", db_get_string ( &sql )  );

                    cat++;
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
        G_warning("%s: Only created %ld random sites",
                G_program_name(), theState->nRand - nt);

    /* close files */
    G_close_cell(infd);
    if (theState->outsites) {
        db_commit_transaction ( driver );
        db_close_database_shutdown_driver ( driver );
        Vect_build (&Out, stderr);
        Vect_close (&Out);
    }
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
