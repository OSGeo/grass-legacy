#include <stdio.h>
#include "usgs.h"

get_hdr()
{
    int i;
    int accuracy;
    double ddummy, angle;

    record_pos = 0;
    buffer = buf_start;
    if (!(filestat = get_buf())) return(0);
    G_strncpy(name, buffer, 50);
    name[50] = '\0';

    buffer += 144;
    record_pos += 144;
    buffer += get_int(&DEM);
    buffer += get_int(&pattern);
    buffer += get_int(&ref_sys);
    buffer += get_int(&ref_zone);

    for (i=0; i < 15; i++)
        buffer += get_double(&ddummy);

    buffer += get_int(&xy_unit);
    buffer += get_int(&z_unit);
    buffer += get_int(&sides);

    for (i=0; i < sides; i++) {
        buffer += get_double(&tpeast[i]);
        buffer += get_double(&tpnorth[i]);
    }
    buffer += get_double(&min_elev);
    buffer += get_double(&max_elev);
    buffer += get_double(&angle);      /* element 12 */

    /* accuracy code */
    buffer += get_int(&accuracy);

    buffer += get_float(&x_res);
    buffer += get_float(&y_res);
    buffer += get_float(&z_res);
    if (!x_res)
        x_res = y_res;
    if (!y_res)
        y_res = x_res;

    buffer += get_int(&P_rows);
    buffer += get_int(&P_cols);

    file_north = -9999999.0;
    file_south = 9999999.0;
    file_east = -9999999.0;
    file_west = 9999999.0;
    for (i = 0; i < sides; i++) {
        if (tpnorth[i] > file_north)
            file_north = tpnorth[i];
        if (tpnorth[i] < file_south)
            file_south = tpnorth[i];
        if (tpeast[i] > file_east)
            file_east = tpeast[i];
        if (tpeast[i] < file_west)
            file_west = tpeast[i];
    }
#ifdef DEBUG
    fprintf(stderr, "\nname = (%s)\n", name);
    fprintf(stderr, "DEM %d  pattern %d  ref_sys %d  ref_zone %d\n",
            DEM, pattern, ref_sys, ref_zone);
    fprintf(stderr, "plan units: %s  ", (xy_unit == 0 ? "radians" :
            (xy_unit == 1 ? "feet" : (xy_unit == 2 ?
            "meters" : "arc-seconds"))));
    fprintf(stderr, "elev units: %s\n", z_unit == 1 ? "feet":"meters");
    fprintf(stderr, "# sides  %d\n", sides);
    fprintf(stderr, "min_elev %g  max_elev %g  angle %g\n",
            min_elev, max_elev, angle);
    fprintf(stderr, "accuracy %d\n", accuracy);
    fprintf(stderr, "x_res %g  y_res %g  z_res %g\n",
            x_res, y_res, z_res);
    fprintf(stderr, "P_cols %d\n",  P_cols);
    fprintf(stderr, "file_north %lf  file_south %lf\n",
            file_north, file_south);
    fprintf(stderr, "file_west %lf  file_east %lf\n",
            file_west, file_east);
#endif
    return (1);
}


hdr_list(file)
FILE *file;
{
    int i;

    for (i=0; i < 80; i++)
        fprintf(file, "-");
    fprintf(file, "\n");
    fprintf(file, "%s\n", name);
    fprintf(file, "min elevation: %f  max elevation: %f\n",
        min_elev, max_elev);
    fprintf(file, "ns_res: %f  ew_res: %f\n", x_res, y_res);
    fprintf(file, "# of columns in file = %d\n", P_cols);
    (void)fflush(file);
}


window_list(file)
FILE *file;
{
        int             i;

        fprintf(file,"Current Window Settings-----------------------------\n");
        fprintf(file,"rows:       %d\n",cellhd.rows);
        fprintf(file,"cols:       %d\n",cellhd.cols);
        fprintf(file,"north:      %lf\n",cellhd.north);
        fprintf(file,"south:      %lf\n",cellhd.south);
        fprintf(file,"east:       %lf\n",cellhd.east);
        fprintf(file,"west:       %lf\n",cellhd.west);
        fprintf(file,"ns_res:     %lf\n",cellhd.ns_res);
        fprintf(file,"ew_res:     %lf\n",cellhd.ew_res);
        fprintf(file,"\n");
}
