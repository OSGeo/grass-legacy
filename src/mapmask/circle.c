/* %W% %G% */
#include "mapmask.h"

circle(radius,x_center,y_center)
    int radius,x_center,y_center;
{
    int row[1000],col[1000];
    int i,j,r,number;

/* note ns_res and ew_res have been made equal */
    x_center = (x_center - window.west) / window.ns_res;
    y_center = (window.north - y_center) / window.ns_res;
    radius   = radius / window.ns_res;

    arcdef(radius,row,col,&number);

    fprintf(stderr,"\n\n\n\n\nFilling Circle\n\n");

    topbtm_fill(col,row,0,number,x_center,y_center,radius);
    center_fill((number-1),number,col,x_center,y_center);
    center_fill(0,number,col,x_center,y_center);
    topbtm_fill(col,row,(number-1),number,x_center,y_center,radius);
}
