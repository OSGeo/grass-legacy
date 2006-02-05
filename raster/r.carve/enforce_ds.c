/* Written by Bill Brown, UIUC GIS Laboratory */
#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "glocale.h"
#include "enforce.h"


#ifndef MAX
#  define MIN(a,b)      ((a<b) ? a : b)
#  define MAX(a,b)      ((a>b) ? a : b)
#endif


#define USE_LOWEST


/* function prototypes */
static void traverse_line_flat(Point2 *pgpts, const int pt, const int npts);
static void traverse_line_noflat(Point2 *pgpts, const double depth, 
                const int pt, const int npts);
static void set_min_point(void *buf, int col, int row,
                double elev, double depth, RASTER_MAP_TYPE rtype);
static double lowest_cell_near_point(void *data, RASTER_MAP_TYPE rtype,
                double px, double py, double rad);


/******************************************************************
 * Returns 0 on success, -1 on error, 1 on warning, writing message
 * to errbuf for -1 or 1 */

int enforce_downstream(int infd, int outfd, char *outvect, 
                   struct Map_info *Map, struct Map_info *outMap,
                   RASTER_MAP_TYPE rtype, double width, double depth, 
                   int noflat, int quiet)
{
    struct Cell_head wind;
    int i, j, retval = 0;
    int vtype, line, nlines;
    int nrows, ncols;
    PointGrp pg;
    PointGrp pgxy;   /* copy any points in region to this one */
    Point2 pt, ptxy, *pgpts, *pgxypts;
    struct line_pnts *points;
    struct line_cats *cats;
#if 0
    CELL *inrast = NULL, *outrast = NULL;
#endif
    void *rbuf = NULL;
    void *tmpbuf = NULL;
    struct BM *bm;

    /* width used here is actually distance to center of stream */
    width /= 2.0; 

    G_get_window(&wind);

    points = Vect_new_line_struct();
    cats   = Vect_new_cats_struct();
    Vect_set_constraint_region(Map, wind.north, wind.south, wind.east,
				wind.west, wind.top, wind.bottom);

    ncols = G_window_cols();
    nrows = G_window_rows();

    bm = BM_create(ncols, nrows);

    /* allocate and clear memory for entire raster */
    rbuf = G_malloc(nrows * ncols * G_raster_size(rtype));
    memset(rbuf, 0, nrows * ncols * G_raster_size(rtype));

    if (!quiet)
        G_message(_("Reading raster file... "));

    /* first read whole elevation file into buf */
    tmpbuf = rbuf;
    for (i = 0; i < nrows; i++) {
        if (!quiet)
            G_percent(i + 1, nrows, 10);

        G_get_raster_row(infd, tmpbuf, i, rtype);
        tmpbuf = G_incr_void_ptr(tmpbuf, G_raster_size(rtype) * ncols);
    }

#if 0
    /* leave for now - may want to eliminate above code eventually and
     * somehow figure out how to write random, so would need to do this
     * row by row at that point */
    inrast  = G_allocate_raster_buf(rtype);
    outrast = G_allocate_raster_buf(rtype);
#endif

    if (!quiet)
        fprintf(stderr, _("Processing lines..."));

    nlines = Vect_get_num_lines(Map);
    for (line = 1; line < nlines; line++) {
        int row, col, inorder = 0;
        int do_warn = 0, first_in = -1;
        int npts = 0;
        int prevrow = -1;
        double totdist = 0.;

        vtype = Vect_read_line(Map, points, cats, line);
        if (!(vtype & GV_LINE))
            continue;

        pg_init(&pg);
        pg_init(&pgxy);

        /* zero out the bitmap */
        for (i = 0; i < nrows; i++)
            for (j = 0; j < ncols; j++)
                BM_set(bm, j, i, 0);

        if (!quiet)
            G_percent(line, nlines, 10);

        for (i = 0; i < points->n_points; i++) {
            row = G_northing_to_row(points->y[i], &wind);
            col = G_easting_to_col(points->x[i], &wind);

            /* rough clipping */
            if (row < 0 || row > nrows - 1 ||
                col < 0 || col > ncols - 1)
            {
                if (first_in != -1)
                    do_warn = 1;

                continue;
	    }

            if (first_in < 0)
                first_in = i;

            ptxy[0] = points->x[i];
            ptxy[1] = points->y[i];

#ifdef USE_LOWEST
	    pt[1] = lowest_cell_near_point(rbuf, rtype,
                                ptxy[0], ptxy[1], width);

            /* returns 0. for NULL cell - kludge */
            if (pt[1] == 0.)
                continue;
#else
            if (row != prevrow)
                G_get_raster_row(infd, inrast, row, rtype);

            if (G_is_null_value(&inrast[col], rtype))
                continue;

            pt[1] = inrast[col];
            prevrow = row;
#endif
            /* get distance from this point to previous point */
            if (i)
                totdist += G_distance(points->x[i-1], points->y[i-1],
                                      points->x[i],   points->y[i]);

            pt[0] = totdist;
            pg_addpt(&pg, pt);
            pg_addpt(&pgxy, ptxy);
            npts++;
        }

        if (do_warn) {
            G_warning(_("Vect runs out of region and re-enters - "
                        "this case is not yet implemented!"));
            retval = 1;
        }

        /* TODO: check for NULL */
        /* now check to see if points go downslope(inorder) or upslope */
        if ((inorder = (pg_y_from_x(&pg, 0.0) > pg_y_from_x(&pg, totdist))))
        {
            pgpts   = pg_getpoints(&pg);
            pgxypts = pg_getpoints(&pgxy);
        } else {
            /* pgpts is now high to low */
            pgpts = pg_getpoints_reversed(&pg);

            for (i = 0; i < npts; i++)
                pgpts[i][0] = totdist - pgpts[i][0];

            pgxypts = pg_getpoints_reversed(&pgxy);  
        }

        for (i = 0; i < (npts - 1); i++) {
            if (noflat)
            {
                /* make sure there are no flat segments in line */
                if (pgpts[i+1][1] < pgpts[i][1]) 
                    continue;

                traverse_line_noflat(pgpts, depth, i, npts);
            } else {
                /* ok to have flat segments in line */
                if (pgpts[i+1][1] <= pgpts[i][1]) 
                    continue;

                traverse_line_flat(pgpts, i, npts);
            }
        }

        /* points are now in order and adjusted to run high to low with
         * no dips or mounds - if site output file given, write it now */
        if (outvect)
            write_xyz_points(outMap, pgxypts, pgpts, npts, depth);

        /* Now for each segment in the line, use distance from segment 
         * to find beginning row from northernmost point, beginning
         * col from easternmost, ending row & col, then loop through 
         * bounding box and use distance from segment to emboss
         * new elevations */
	{
        int row1, row2, col1, col2;
        int prevcol;
        double cellx, celly, cy;

        /* kludge - fix for lat_lon */
        int rowoff = width / wind.ns_res;
        int coloff = width / wind.ew_res;

        /* get prevrow and prevcol for iteration 0 of following loop */
        prevrow = G_northing_to_row(pgxypts[0][1], &wind);
        prevcol = G_easting_to_col(pgxypts[0][0], &wind);

        for (i = 1; i < (npts - 1); i++) {
            int c, r;

            row = G_northing_to_row(pgxypts[i][1], &wind);
            col = G_easting_to_col(pgxypts[i][0], &wind);

            /* get bounding box of line points */
            row1 = MAX(0, MIN(row, prevrow) - rowoff);
            row2 = MIN(nrows - 1, MAX(row, prevrow) + rowoff);
            col1 = MAX(0, MIN(col, prevcol) - coloff);
            col2 = MIN(ncols - 1, MAX(col, prevcol) + coloff);

            for (r = row1; r < row2; r++) {
                cy = G_row_to_northing(r + 0.5, &wind);

                for (c = col1; c < col2; c++) {
#if 0
                    int status;
#endif

                    cellx = G_col_to_easting(c + 0.5, &wind);
                    celly = cy;  /* gets written over in distance2... */

                    /* Thought about not going past endpoints (use 
                     * status to check) but then pieces end up missing 
                     * from outside corners - if it goes past ends, 
                     * should probably do some interp or will get flats.
                     * Here we use a bitmap and only change cells once 
                     * on the way down */

                    if (dig_distance2_point_to_line(cellx, celly, 0,
                           pgxypts[i-1][0], pgxypts[i-1][1], 0,
                           pgxypts[i][0], pgxypts[i][1], 0,
                           0, &cellx, &celly, NULL, NULL, NULL))
                    {
                        if (!BM_get(bm, c, r))
                        {
                            double dist, elev;

                            BM_set(bm, c, r, 1);

                            dist = G_distance(pgxypts[i][0], pgxypts[i][1],
                                              cellx, celly);

                            elev = LINTERP(pgpts[i][1], pgpts[i-1][1],
                                        (dist / (pgpts[i][0] - pgpts[i-1][0])));

                            /* TODO - may want to use a function for the 
                             * cross section of stream */
                            set_min_point(rbuf, c, r, elev, depth, rtype);
                        }
                    }
                }
            }

            prevrow = row;
            prevcol = col;
        }
        }

        G_debug(3, "inorder:%d  %.3lf %.3lf", inorder,
                pg_y_from_x(&pg, 0.0), pg_y_from_x(&pg, totdist));
    }

    /* write output raster file */
    if (!quiet)
        fprintf(stderr, _("\nWriting raster file... "));

    tmpbuf = rbuf;
    for (i = 0; i < nrows; i++) {
        if (!quiet)
            G_percent(i, nrows, 10);

        G_put_raster_row(outfd, tmpbuf, rtype);
        tmpbuf = G_incr_void_ptr(tmpbuf, G_raster_size(rtype) * ncols);
    }

    Vect_destroy_line_struct(points);
    Vect_destroy_cats_struct(cats);
    BM_destroy(bm);
#if 0
    G_free(inrast);
    G_free(outrast);
#endif
    G_free(rbuf);

    if (!quiet)
        G_message(_("done."));

    return retval;
}


static void traverse_line_flat(Point2 *pgpts, const int pt, const int npts)
{
    int j, k;

    for (j = (pt + 2); j < npts; j++)
       if (pgpts[j][1] <= pgpts[pt][1])
           break; 

    if (j == npts)
        /* if we got to the end, level it out */
        for (j = (pt + 1); j < npts; j++)
            pgpts[j][1] = pgpts[pt][1];
    else
        /* linear interp between point i and the next <= */
        for (k = (pt + 1); k < j; k++)
            pgpts[k][1] = LINTERP(pgpts[j][1], pgpts[pt][1],
                                 (pgpts[j][0] - pgpts[k][0]) /
                                 (pgpts[j][0] - pgpts[pt][0]));
}


static void traverse_line_noflat(Point2 *pgpts, const double depth, 
                                 const int pt, const int npts)
{
    int j, k;

    for (j = (pt + 2); j < npts; j++)
        if (pgpts[j][1] < pgpts[pt][1])
            break; 

    if (j == npts)
    {
        /* if we got to the end, lower end by depth OR .01 & INTERP */
        --j;
        pgpts[j][1] = pgpts[pt][1] - (depth > 0 ? depth : .01);

        for (k = (pt + 1); k < j; k++)
            pgpts[k][1] = LINTERP(pgpts[j][1], pgpts[pt][1],
                                 (pgpts[j][0] - pgpts[k][0]) /
                                 (pgpts[j][0] - pgpts[pt][0]));
    } else {
        /* linear interp between point pt and the next < */
        for (k = (pt + 1); k < j; k++)
            pgpts[k][1] = LINTERP(pgpts[j][1], pgpts[pt][1],
                                 (pgpts[j][0] - pgpts[k][0]) / 
                                 (pgpts[j][0] - pgpts[pt][0]));
    }
}


static void set_min_point(void *data, int col, int row,
                double elev, double depth, RASTER_MAP_TYPE rtype)
{
    switch (rtype) {
    case CELL_TYPE:
    {
        CELL *cbuf = data;

        G_incr_void_ptr(cbuf, row * G_window_cols());
        cbuf[col] = MIN(cbuf[col], elev) - (int)depth;
        G_debug(3, "val=%d", cbuf[col]);
    }
    break;
    case FCELL_TYPE:
    {
        FCELL *fbuf = data;

        G_incr_void_ptr(fbuf, row * G_window_cols() * G_raster_size(rtype));
        fbuf[col] = MIN(fbuf[col], elev) - depth;
        G_debug(3, "val=%.2lf", fbuf[col]);
    }
    break;
    case DCELL_TYPE:
    {
        DCELL *dbuf = data;

        G_incr_void_ptr(dbuf, row * G_window_cols() * G_raster_size(rtype));
        dbuf[col] = MIN(dbuf[col], elev) - depth;
        G_debug(3, "val=%.2lf", dbuf[col]);
    }
    break;
    }
}


/* returns the lowest value cell within radius rad of px, py */
static double lowest_cell_near_point(void *data, RASTER_MAP_TYPE rtype,
            double px, double py, double rad)
{
    int r, row, col, row1, row2, col1, col2, rowoff, coloff;
    double min = 0.;
    struct Cell_head wind;

    G_get_window(&wind);

    /* kludge - fix for lat_lon */
    rowoff = rad / wind.ns_res;
    coloff = rad / wind.ew_res;

    row = G_northing_to_row(py, &wind);
    col = G_easting_to_col(px, &wind);

    row1 = MAX(0, row - rowoff);
    row2 = MIN(G_window_rows() - 1, row + rowoff);
    col1 = MAX(0, col - coloff);
    col2 = MIN(G_window_cols() - 1, col + coloff);

    G_debug(3, "row:%d col:%d row1:%d col1:%d row2:%d col2:%d",
               row, col, row1, col1, row2, col2);

    switch (rtype) {
    case CELL_TYPE:
    {
        CELL *cbuf = data;

        G_incr_void_ptr(cbuf, row1 * G_window_cols());
        if (G_is_c_null_value(&cbuf[col1]))
            min = 0.;
        else
            min = cbuf[col1];
    }
    break;
    case FCELL_TYPE:
    {
        FCELL *fbuf = data;

        G_incr_void_ptr(fbuf, row1 * G_window_cols() * G_raster_size(rtype));
        if (G_is_f_null_value(&fbuf[col1]))
            min = 0.;
        else
            min = fbuf[col1];
    }
    break;
    case DCELL_TYPE:
    {
        DCELL *dbuf = data;

        G_incr_void_ptr(dbuf, row1 * G_window_cols() * G_raster_size(rtype));
        if (G_is_d_null_value(&dbuf[col1]))
            min = 0.;
        else
            min = dbuf[col1];
    }
    break;
    }

    if (min == 0.)
        return min;

    for (r = row1; r < row2; r++) {
        double cy = G_row_to_northing(r + 0.5, &wind);
        int c;

        for (c = col1; c < col2; c++) {
            double cellx = G_col_to_easting(c + 0.5, &wind);
            double celly = cy;  /* gets written over in distance2... */

            if (G_distance(px, py, cellx, celly) <= SQR(rad))
            {
                switch (rtype) {
                case CELL_TYPE:
                {
                    CELL *cbuf = data;

                    G_incr_void_ptr(cbuf, r * G_window_cols());
                    if (cbuf[c] < min)
                        min = cbuf[c];
                }
		break;
                case FCELL_TYPE:
                {
                    FCELL *fbuf = data;

                    G_incr_void_ptr(fbuf, r * G_window_cols() * G_raster_size(rtype));
                    if (fbuf[c] < min)
                        min = fbuf[c];
                }
                break;
                case DCELL_TYPE:
                {
                    DCELL *dbuf = data;

                    G_incr_void_ptr(dbuf, r * G_window_cols() * G_raster_size(rtype));
                    if (dbuf[c] < min)
                        min = dbuf[c];
                }
                break;
                }
            }
        }
    }

    G_debug(3, "min=%.2lf", min);

    return min;
}
