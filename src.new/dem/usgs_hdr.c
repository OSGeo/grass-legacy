/* %W% %G% */

#include "usgs.h"

static char name[51];
double file_north, file_south, file_east, file_west;
static double max_elev, min_elev;
extern int total_cols;

get_hdr()
{
    int i, sides, z_unit, xy_unit, P_rows;
    int DEM, pattern, ref_sys, accuracy;
    double ddummy, angle, lng[4], lat[4];

	if (fread(buffer, 1, 144, tapefile) != 144)
		return 0;
    bcopy(buffer, name, 50);
    name[50] = '\0';

    if (!get_buf())
        return (0);
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
        buffer += get_double(&lng[i]);
        buffer += get_double(&lat[i]);
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
    buffer += get_int(&total_cols);

    file_north = -9999999.0;
    file_south = 9999999.0;
    file_east = -9999999.0;
    file_west = 9999999.0;
    for (i = 0; i < sides; i++) {
        if (lat[i] > file_north)
            file_north = lat[i];
        if (lat[i] < file_south)
            file_south = lat[i];
        if (lng[i] > file_east)
            file_east = lng[i];
        if (lng[i] < file_west)
            file_west = lng[i];
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
    fprintf(stderr, "total_cols %d\n",  total_cols);
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
    fprintf(file, "# of columns in file = %d\n", total_cols);
    (void)fflush(file);
}

