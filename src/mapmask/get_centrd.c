/* %W% %G% */
#include "mapmask.h"

get_centroid(x_center,y_center,radius,sides)
    int *x_center,*y_center,*radius,*sides;
{
    V_clear();

    V_line(1, "           E                                                                  E");
    V_line(2, "           N                                                                  N");
    V_line(20,"           E                                                                  E");
    V_line(21,"           N                                                                  N");
    V_const(&window.west,'d',1,1,10);
    V_const(&window.north,'d',2,0,10);
    V_const(&window.east,'d',1,67,10);
    V_const(&window.north,'d',2,66,10);
    V_const(&window.west,'d',20,1,10);
    V_const(&window.south,'d',21,0,10);
    V_const(&window.east,'d',20,67,10);
    V_const(&window.south,'d',21,66,10);

    V_line(3, "           The coordinate boundaries of the current window");
    V_line(4, "               are shown in the corners of your screen.");
    V_line(6, "           You have chosen to use a regular polygon to define");
    V_line(7, "                        your area of interest.");
    V_line(10,"           Please enter the coordinates for the centroid:");
    V_line(11,"                Easting:");
    V_line(12,"                Northing:");
    V_line(14,"           Please enter the corner to center distance in meters:");
    V_line(15,"                Distance:");
    V_line(17,"           Please enter the number of sides:");
    V_line(18,"                Sides:");

    *x_center = 0;
    *y_center = 0;
    *radius   = 0;
    *sides    = 0;
    V_ques(x_center,'i',11,26,10);
    V_ques(y_center,'i',12,26,10);
    V_ques(radius,'i',15,27,5);
    V_ques(sides,'i',18,24,2);

    V_call();
    V_clear();
}
