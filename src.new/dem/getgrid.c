/* %W% %G% */

#include <sys/file.h>
#include "usgs.h"

static double g_row, g_col, cur_row;
static int skip_row, skip_col, elev;
static int col_id, row_id;
static int d_rows, d_cols;
static double xdelta, ydelta;

getgrid()
{
    int i, j, r;
    long offset;
    double P_height;

    xdelta = x_res / 2.0;
    ydelta = y_res / 2.0;
    for (i=0, g_col = west; (i < total_cols) && (g_col <= east); i++) {
#ifdef DEBUG
        fprintf(stderr, "col %-3d:", i);
#endif
        if (!get_profile())
            return 0;
        if ((P_col < west) || (P_col > east)) {
            fprintf(stderr, " skipping col %d\n", i);
            for (j=0; j < d_rows; j++)      /* profile no in window */
                buffer += get_int(&elev);   /* skip all of it */
            continue;
        }
        if (!skip_columns())      /* if dem smaller than window */
            continue;
    
        P_height = P_row + (d_rows-1) * y_res;
        /* find south edge of data */
        if (south > P_height) {
            for (j=0; j < d_rows; j++)   /* profile doesn't reach */
                buffer += get_int(&elev);   /* bottom, skip it */
            continue;
        }
        /* buffer += get_int(&elev); */         /* elev. at P_row */
        for (cur_row = P_row; south > cur_row; cur_row += y_res)
            buffer += get_int(&elev);       /* skip part of it */

        /* loop thru and extract elevation values according
         * to resolution of cell header file
         */
        g_row = south;
        skip_row = 0;
        if (!skip_rows())
            continue;
        bzero((char*)profile_buf, profile_size);
        for (r = skip_row; (cur_row <= P_height) && (cur_row <= north);
                cur_row += y_res) {
            buffer += get_int(&elev);
            elev = bas_elev + (elev * z_res);
            profile_buf[r++] = elev;
        }
        /* write profile to temporary file */
        offset = (skip_col * cellhd.rows) * sizeof(CELL);
        (void)lseek(fd, offset, 0);
        if (write(fd, (char*)profile_buf,  profile_size) != profile_size) {
            G_warning("getgrid: error while writing to cell file");
            return(0);
        }
        /* read rest of elevations in profile */
        for ( ; cur_row <= P_height; cur_row += y_res)
            buffer += get_int(&elev);
        g_col += cellhd.ew_res;
        skip_col++;
    }
    return (1);
}


skip_columns()
{
    while (g_col < P_col) {
        if ((g_col += cellhd.ew_res) > east)
            return 0;
        skip_col++;
    }
    return 1;
}


skip_rows()
{
    while (g_row < P_row) {
        if ((g_row += cellhd.ns_res) > north)
            return 0;
        skip_row++;
    }
    return 1;
}


get_profile()
{
    double junk;

    if (!get_buf())        /* each profile is on a new record */
        return 0;
    buffer += get_int(&row_id);
    buffer += get_int(&col_id);
    buffer += get_int(&d_rows);
    buffer += get_int(&d_cols);
    buffer += get_double(&P_col);
    buffer += get_double(&P_row);
    buffer += get_double(&bas_elev);
    buffer += get_double(&junk);
    buffer += get_double(&junk);
#ifdef DEBUG
    fprintf(stderr, "  rows %-3d  cols %d  P_col %g  P_row %g\n",
            d_rows, d_cols, P_col, P_row);
#endif
    return 1;
}

/*** end getgrid.c ***/
