#include <stdio.h>
#include "gis.h"
#include "usgs.h"

get_hdr()
{
    int i, i1, i2, i3, i4, i5, i6;
    double ddummy, angle;
    if(count>1){ if(!next_record()) return 0;}
    record_pos = 0;

    if (filestat <= 80) return -1; /* mod shapiro */
    G_strncpy(name, buffer, 40);
    name[40] = '\0';
    buffer += 144;
    record_pos += 144;

    if(sscanf(name, "%d     %d     %d     %d     %d     %d", 
	     &i1, &i2, &i3, &i4, &i5, &i6)==6)
    {
	/* printf("WARNING");*/
         return -3;
    }

    buffer += get_int(&DEM);
    buffer += get_int(&pattern);
    buffer += get_int(&ref_sys);
    buffer += get_int(&ref_zone);

    for (i=0; i < 15; i++)
        buffer += get_double(&ddummy);
/***************initialization:***********/
    xy_unit = 999;
    z_unit  = 999;
    sides   = 999;

    min_elev = 9999.;
    max_elev = 9999.;
    angle    = 9999.;

    x_res = 9999.;
    y_res = 9999.;
    z_res = 9999.;

    P_rows = 9999;
    P_cols = 9999;

    file_north = -9999999.0;
    file_south = 9999999.0;
    file_east = -9999999.0;
    file_west = 9999999.0;
/*****************************************/
    buffer += get_int(&xy_unit);
    buffer += get_int(&z_unit);
    buffer += get_int(&sides);

    if(xy_unit == 999)fail_return(name);
    if(z_unit  == 999)fail_return(name);
    if(sides   == 999)fail_return(name);

    if(sides>4) sides = 4; 

    for (i=0; i < sides; i++) {

    tpeast[i] = 9999.;
    tpnorth[i] = 9999.;

        buffer += get_double(&tpeast[i]);
        buffer += get_double(&tpnorth[i]);

    if(tpeast[i] == 9999.)fail_return(name);
    if(tpnorth[i] == 9999.)fail_return(name);

    }

    buffer += get_double(&min_elev);
    buffer += get_double(&max_elev);
    buffer += get_double(&angle);      /* element 12 */

    if(min_elev == 9999.)fail_return(name);
    if(max_elev == 9999.)fail_return(name);
    if(angle    == 9999.)fail_return(name);

    /* accuracy code */
    C_record = 999;
    buffer += get_int(&C_record); /* accuracy */

    if(!((C_record==0)||(C_record==1))) fail_return(name);

    buffer += get_float(&x_res);
    buffer += get_float(&y_res);
    buffer += get_float(&z_res);

    if(x_res == 9999.) fail_return(name);
    if(y_res == 9999.) fail_return(name);
    if(z_res == 9999.) fail_return(name);

    if (!x_res)
        x_res = y_res;
    if (!y_res)
        y_res = x_res;

    buffer += get_int(&P_rows);
    buffer += get_int(&P_cols);

    if(P_rows == 9999)fail_return(name);
    if(P_cols == 9999)fail_return(name);

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
    if(file_north == -9999999.0) fail_return(name);
    if(file_south == 9999999.0) fail_return(name);
    if(file_east == -9999999.0) fail_return(name);
    if(file_west == 9999999.0) fail_return(name);
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
    fprintf(stderr, "accuracy %d\n", C_record);
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
    fprintf(file, "File # %d\n", count);
    fprintf(file, "%s\n", name);
    fprintf(file, "min elevation: %f  max elevation: %f\n",
        min_elev, max_elev);
    fprintf(file, "ns_res: %f  ew_res: %f\n", x_res, y_res);
    fprintf(file, "# of columns in file = %d\n", P_cols);
    fprintf(file, "C-record: %d\n", C_record);
    (void)fflush(file);
}


window_list(file)
FILE *file;
{
   char buf[500];

        fprintf(file,"Current Region Settings-----------------------------\n");
        fprintf(file,"rows:       %d\n",cellhd.rows);
        fprintf(file,"cols:       %d\n",cellhd.cols);
	G_format_northing(cellhd.north, buf, G_projection());
        fprintf(file,"north:      %s\n",buf);
	G_format_northing(cellhd.south, buf, G_projection());
        fprintf(file,"south:      %s\n",buf);
	G_format_easting ( cellhd.east, buf, G_projection());
        fprintf(file,"east:       %s\n",buf);
	G_format_easting ( cellhd.west, buf, G_projection());
        fprintf(file,"west:       %s\n",buf);
        fprintf(file,"ns_res:     %lf\n",cellhd.ns_res);
        fprintf(file,"ew_res:     %lf\n",cellhd.ew_res);
        fprintf(file,"\n");
}


fail_return(name)
  char * name;
{

    return -2;
}
