/* %W% %G% */
#include "mapmask.h"

front_end(sides,incr)
    int *sides,incr;
{
    int x_center,y_center,radius;
    int bad,option;
    char messg[60];
    char ttyname[20];
    FILE *process,*popen();
    extern int *x,*y;

    V_clear();

    sprintf(messg,"       defining your #%d area of interest.",incr+1);
    V_line(4, "     There are three options you can choose in");
    V_line(5,messg);
    V_line(7, "     0  exit");
    V_line(9, "     1  circle with user specified center and radius");
    V_line(11,"     2  regular polygon with user specified centroid,");
    V_line(12,"	       distance to corner, and number of sides");
    V_line(14,"     3  a polygon with user specified corner coordinates");
    V_line(19,"     Which option do you want? (0-3)");
    option = 0;
    V_ques(&option,'i',19,38,1);

    V_call();
    V_clear();

    switch(option)
    {
    case 0:
	    fprintf(stderr,"Exiting at user request\n");
	    exit(-1);
	    break;

    case 1:
	    do
	    {
		get_circle(&x_center,&y_center,&radius);
		bad = 0;

		if(x_center > window.east  ||
		   x_center < window.west  ||
		   y_center > window.north ||
		   y_center < window.south)
		{
		    fprintf(stderr,"\n\n\n\n\n\n\n\n\n");
		    fprintf(stderr,"	 CENTER OF CIRCLE OUT OF BOUNDS\n");
		    fprintf(stderr," center Easting:  %7d ",x_center);
		    fprintf(stderr," center Northing: %7d\n",y_center);
		    fprintf(stderr,"CENTER OF CIRCLE MUST BE WITHIN INSTALLATION BOUNDARIES\n");
		    sleep(5);
		    G_clear_screen();
		    bad = 1;
		}
		if(radius > (int)(710 * window.ns_res))
		{
		    fprintf(stderr,"\n\n\n\n\n\n\n\n\n");
		    fprintf(stderr,"MAX RADIUS = %d\n",
				    (int)(710 * window.ns_res));
		    sleep(5);
		    G_clear_screen();
		    bad = 1;
		}
		if(radius == 0)
		{
		    fprintf(stderr,"\n\n\n\n\n\n\n\n\n");
		    fprintf(stderr,"Radius must be greater than 0\n");
		    sleep(5);
		    G_clear_screen();
		    bad = 1;
		}
	    } while(bad == 1);

	    circle(radius,x_center,y_center);
	    *sides = -1; 
	    break;

    case 2:
	    get_centroid(&x_center,&y_center,&radius,sides);
	    if(*sides != 0)
		centroid(radius,*sides,x_center,y_center);
	    break;

    case 3:
	    get_coords(sides);
	    break;

    default:
	    fprintf(stderr,"\n\n\n\n\nINVALID OPTION\n");
	    return(-1); 
	    break;
    }
    return(1);
}
