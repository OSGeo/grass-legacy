#include "gis.h"

double
linterp (zmin, contour, zmax)
    int  contour, zmin, zmax;
{
    if (zmin == zmax)	/* div by zero.  should never get here */
	return (double) zmin;
    return ((double)(contour - zmin)) / ((double)(zmax - zmin));
}

main(argc,argv) char *argv[];
{
    int x,y,z;
    int row,col;
    double a,b,c,d;
    double xoffset, yoffset;
    double real_x, real_y;
    struct Cell_head window;

    G_gisinit(argv[0]);
    G_get_window (&window);
    xoffset = window.ew_res/2;
    yoffset = -window.ns_res/2;
    if (argc != 4)
	usage (argv[0]);
    if (sscanf (argv[1], "%d", &x) != 1)
	usage (argv[0]);
    if (sscanf (argv[2], "%d", &y) != 1)
	usage (argv[0]);
    if (sscanf (argv[3], "%d", &z) != 1)
	usage (argv[0]);

    row = 3;
    col = 2;
    real_x = window.west + (window.ew_res*col + xoffset);
    real_y = window.north -  (window.ns_res*row) + yoffset;
    a = real_x + window.ew_res;
    b = real_y - linterp(x, z, y) * window.ns_res;

    row = 3;
    col++;
    real_x = window.west + (window.ew_res*col + xoffset);
    real_y = window.north -  (window.ns_res*row) + yoffset;
    c = real_x ;
    d = real_y - linterp(x, z, y) * window.ns_res;
    if (a != c) printf ("Y differs\n");
    if (b != d) printf ("X differs\n");
}


usage (me) char *me;
{
    fprintf (stderr, "Usage: %s x y z\n", me);
    exit(1);
}
