
/*
**  Written by Dave Gerdes  Summer 1990
**  US Army Construction Engineering Research Lab
*/

#include "gis.h"
#include <gl.h>
#include <device.h>
#include <math.h>
#include "Vect.h"

/*#define MAIN*/
#include "externs.h"

long Main_Win;
/*double atof();*/
char *getenv();

int corners[8][3];


Dcell (elev, name1, name2, name3, vname)
    char *elev;
    char *name1;
    char *name2;
    char *name3;
    char *vname;
{
    char *elev_map;
    char *name1_map;
    char *name2_map;
    char *name3_map;
    char *vect_map;
    int i;
    /*struct Cell_head wind;*/
    char buff[128];
    FILEDESC cellfile1 = NULL;
    FILEDESC cellfile2 = NULL;
    FILEDESC cellfile3 = NULL;
    FILEDESC elev_cell = NULL;
    CELL *xarray;
    struct Range Range;
    long *color_array;
    int row, col;
    int t, b, l, r;
    char *p;
    int j, k;
    int dev;
    short val;
    /*float Z_Span, Z_Span2;*/

    float xfrom, yfrom, zfrom, xto, yto, zto;


    i = j = k = 0;

    keepaspect (1, 1);


/******************************************************************************/
    G_get_set_window (&wind) ; /* ok */

    /* get Z range */
    elev_map = G_find_file2 ("cell", elev, "");
    G_read_range (elev, elev_map, &Range);
    if (!(Z_Min_real = Range.nmin))
	Z_Min_real = Range.pmin;
    if (!(Z_Max_real = Range.pmax))
	Z_Max_real = Range.nmax;
    if (!Z_Min_real && !Z_Max_real)
    {
	fprintf (stderr, "Please update the stats (support) for file %s\n", elev);
	Z_Min_real = -2000; Z_Max_real = 2000;
    }

    /* allow env overide */
    if ((p = getenv ("ZMAX")) != NULL)
	Z_Max_real = atof (p);
    if ((p = getenv ("ZMIN")) != NULL)
	Z_Min_real = atof (p);


    Z_Mid_real = (Z_Max_real + Z_Min_real) / 2.;

    Z_Min = Z_Min_real;
    Z_Max = Z_Max_real;

    /*
    **  have to shrink down the world space
    **  because of Z-buffer accuracy limitations
    */
    {
	X_Min = 0;
	X_Max = wind.east - wind.west;
	Y_Min = 0;
	Y_Max = wind.north - wind.south;

	Z_Min = 0;
	Z_Max = Z_Max_real - Z_Min_real;
	Zoff = Z_Min_real;	/* offset to add to get real Z */
    }

    Z_Span_real = Z_Span = Z_Max_real - Z_Min_real;


    X_Mid = (X_Max + X_Min) / 2.;
    Y_Mid = (Y_Max + Y_Min) / 2.;
    Z_Mid = (Z_Max + Z_Min) / 2.;

    X_Res = wind.ew_res;
    Y_Res = wind.ns_res;

/*DEBUG*/  fprintf (stderr, "x (%d,%d)  y (%d,%d)  z (%f,%f)\n", X_Min, X_Max,Y_Min, Y_Max,Z_Min, Z_Max);

    if (vname == NULL)
	Vect_file = 0;
    else
    {
	vect_map = G_find_file2 ("dig", vname, "");
	Vect_file = 1;
	Vect_z = Z_Min;
	/*
	if (NULL == (Vect_fp = G_fopen_old ("dig", vname, vect_map)))
	    Vect_file = 0;
	*/
	if (0 >= Vect_open_old (&Map, vname, vect_map))
	    Vect_file = 0;
	else
	{
	    Points = Vect_new_line_struct ();
	    Vect_set_constraint_region (&Map, wind.north, wind.south, wind.east, wind.west);
	}
    }


/******************************************************************************/
/*   define the corners of the object space for calculating clipping planes   */
/******************************************************************************/
    corners[0][0] = X_Min; corners[0][1] = Y_Min; corners[0][2] = Z_Min;
    corners[1][0] = X_Min; corners[1][1] = Y_Max; corners[1][2] = Z_Min;
    corners[2][0] = X_Max; corners[2][1] = Y_Min; corners[2][2] = Z_Min;
    corners[3][0] = X_Max; corners[3][1] = Y_Max; corners[3][2] = Z_Min;
    corners[4][0] = X_Min; corners[4][1] = Y_Min; corners[4][2] = Z_Max;
    corners[5][0] = X_Min; corners[5][1] = Y_Max; corners[5][2] = Z_Max;
    corners[6][0] = X_Max; corners[6][1] = Y_Min; corners[6][2] = Z_Max;
    corners[7][0] = X_Max; corners[7][1] = Y_Max; corners[7][2] = Z_Max;
/******************************************************************************/
/******************************************************************************/

    foreground ();
    {
	int x1, x2, y1, y2;
	int height, width;

	if (p = getenv ("GRASS_HEIGHT"))
	    height = atoi (p);
	else height = 700;
	if (p = getenv ("GRASS_WIDTH"))
	    width = atoi (p);
	else width=700;
	x1 = 10; x2 = 10 + width;
	y2 = 1000; y1 = 1000 - height;

	/*prefposition (10, 700, 300, 1000);*/
	prefposition (x1, x2, y1, y2);
	Main_Win = winopen ("3D SGI");
	winconstraints ();
	winconstraints ();
    }

    concave (1);

    RGBmode ();
    doublebuffer ();
    gconfig ();

    getviewport (&left, &right, &bottom, &top);

    /*buff_init ();*/


    getorigin (&X_Size, &Y_Size); /* WILL be reused as Wind.size */
    xcenter = ((left + right) >> 1 ) + X_Size;
    ycenter = ((top + bottom) >> 1 ) + Y_Size;

    cpack (0);
    clear ();
    swapbuffers ();

    zbuffer (1);
    lsetdepth (0, 0x7fffff);
    zclear ();
    zfunction (ZF_LEQUAL);


    Y_Size = wind.rows;
    X_Size = wind.cols;

    initialize ();


/******************************************************************************/




/* Make sure maps are available */
    if ((elev_cell = G_open_cell_old(elev, elev_map)) == -1) 
    {
	sprintf(buff,"Not able to open cellfile for [%s]", elev);
	G_fatal_error(buff);
    }


#ifdef OLD
    name1_map = G_find_file2 ("cell", name1, "");
    if ((cellfile1 = G_open_cell_old(name1, name1_map)) == -1) 
    {
	sprintf(buff,"Not able to open cellfile for [%s]", name1);
	G_fatal_error(buff);
    }
    if (Three_map)
    {
	name2_map = G_find_file2 ("cell", name2, "");
	if ((cellfile2 = G_open_cell_old(name2, name2_map)) == -1) 
	{
	    sprintf(buff,"Not able to open cellfile for [%s]", name2);
	    G_fatal_error(buff);
	}
	name3_map = G_find_file2 ("cell", name3, "");
	if ((cellfile3 = G_open_cell_old(name3, name3_map)) == -1) 
	{
	    sprintf(buff,"Not able to open cellfile for [%s]", name3);
	    G_fatal_error(buff);
	}
    }
    else
    {
	G_read_colors (name1, name1_map, &Pcolor);
    }

/* Allocate space for cell buffer */
    xarray = (int *)G_malloc (X_Size * sizeof (int));
    color_array = (long *)G_malloc (X_Size * sizeof (long));



/* alllocate buffers */
    visual = (int *)G_malloc (X_Size * Y_Size * sizeof (int));
#endif

    elev_buf = (int *)G_malloc (X_Size * Y_Size * sizeof (int));

/******************************************************************************/


    fprintf (stderr, "Initial load of data:  ");

    for (row = 0; row < Y_Size ; row++) 
    {
	int row_off;

	row_off = row * X_Size;

	G_get_map_row(elev_cell, &(elev_buf[row_off]), row) ; 

#ifdef OLD
	if (Three_map)
	{
	    G_get_map_row (cellfile1, xarray, row); 
	    for (i = 0 ; i < X_Size ; i++)
		color_array[i] = xarray[i] & 0xff;

	    G_get_map_row (cellfile2, xarray, row); 
	    for (i = 0 ; i < X_Size ; i++)
		color_array[i] |= (xarray[i] & 0xff) << 8;
	
	    G_get_map_row (cellfile3, xarray, row); 

	    /* finish the composite color array */
	    for (i = 0 ; i < X_Size ; i++)
		visual[row_off+i] = color_array[i] | ((xarray[i]&0xff)<<16);
	}
	else
	{
	    /*G_get_map_row (cellfile1, &(visual[row*X_Size]), row); */
	    G_get_map_row (cellfile1, xarray, row); 
	    /* shift values down (or up) to start at 1 */
	    /* note will need to subtract 1 to index color arrays */
	    for (i = 0 ; i < X_Size ; i++)
	    {
		/*visual[row_off+i] = !xarray[i] ? 0 : xarray[i]-Pcolor.min+1;*/
		visual[row_off+i] = xarray[i];
	    }
	}
#endif
    }
    G_close_cell(elev_cell);

    if (Three_map)
	_newcell (name1, name2, name3);
    else
	_newcell (name1, NULL, NULL);

#ifdef OLD
    G_close_cell(cellfile1);
    if (Three_map)
    {
	G_close_cell(cellfile2);
	G_close_cell(cellfile3);
    }
    free (xarray);
    free (color_array);
#endif

    fprintf (stderr, "Done.\n");

/******************************************************************************/


    initialize2 ();

    /*
    cpack (0);
    clear ();
    zclear ();
    cpack (0xcfcfcfcf);
    display_lines (X_Mod, Y_Mod, shading);
    swapbuffers ();
    */


    {
	Actuator *a;
	static int first=1;

	redraw_ok = 1;
	while (1)
	{
	    extern Actuator *filetypein;	/* in script.c */

	    a = pnl_dopanel ();
	    if (Main_Win == pnl_userredraw ())
	    {
		redraw_ok = 1;
		reshapeviewport ();
		redraw_event ();
	    }

	    serve_actuator (a);

	    if (first && AUTO_FILE != NULL)
	    {
		 first = 0;
		 strcpy (PNL_ACCESS(Typein, filetypein, str), AUTO_FILE);
		 pnl_beginreadscript(PNL_ACCESS(Typein, filetypein, str));
	    }
	}
    }
}


static float
distance (from, to)
    float from[3], to[3];
{
    float x, y, z;
    x = from[X] - to[X];
    y = from[Y] - to[Y];
    z = from[Z] - to[Z];
    return (float) sqrt (x*x + y*y + z*z);
}

update_projection ()
{
    float tmp, farclip, nearclip;
    int cnt;
    float x, y, z;
    int i;

    cnt = 0;

    x = FROM_TO[FROM][X];
    y = FROM_TO[FROM][Y];
    z = FROM_TO[FROM][Z];
    /*
    ** determine where eye is w/ respect to data:
    **   completely out side of all cutting planes
    **   w/in 1 plane, w/in 2 planes, or entirely w/in parallelpiped
    **
    **  if 0, then nearclip == shortest distance
    **  if 1, ??   so use nearest/2  (really dist from ave 2 nearest)
    **  if 2, ??   so use nearest/4 (really dist from ave 4 nearest)
    **  if 3, then nearclip == SMALL
    */
    if (corners[0][X] < x && x < corners[2][X])  cnt++;
    if (corners[0][Y] < y && y < corners[1][Y])  cnt++;
    if (corners[0][Z] < z && z < corners[4][Z])  cnt++;
    farclip = 0.;
    nearclip = 999999999999.;
    for (i = 0 ; i < 8 ; i++)
    {
	if ((tmp = distance (FROM_TO[FROM], corners[i])) > farclip)
	    farclip = tmp;
	if ((tmp = distance (FROM_TO[FROM], corners[i])) < nearclip)
	    nearclip = tmp;
    }
    nearclip = nearclip / (1 << (cnt+2));
    if (nearclip < 20. || cnt == 3)
	nearclip = 30.;
	
    farclip *= 3;   /* fudge it */

  {
    /*perspective (persp, 1., .1, 20000.);ORIG*/
    /*perspective (persp, 1., 20., 20000.);*/
    /*printf ("%f %f\n", nearclip, farclip);*/
    perspective (persp, 1., nearclip, farclip);
    /*  Original
    lookat(FROM_TO[FROM][X],FROM_TO[FROM][Y],FROM_TO[FROM][Z],
           FROM_TO[TO][X],FROM_TO[TO][Y],FROM_TO[TO][Z], (int) Alook->val-900);
    */
    lookat(FROM_TO[FROM][X],FROM_TO[FROM][Y],FROM_TO[FROM][Z],
           FROM_TO[TO][X],FROM_TO[TO][Y],FROM_TO[TO][Z], (int) Alook->val-900 + Atwist->val);
  }
}

#define RANGE_FACTOR 1.5

initialize ()
{
    linesmooth (1);
    install_panels ();

    XBase = X_Min - (X_Max - X_Mid) * RANGE_FACTOR;
    YBase = Y_Min - (Y_Max - Y_Mid) * RANGE_FACTOR;
    ZBase = Z_Min;

    XRange = X_Max + (X_Max - X_Mid) * RANGE_FACTOR - XBase;
    YRange = Y_Max + (Y_Max - Y_Mid) * RANGE_FACTOR - YBase;
    /*ZRange = ZRange_real = Z_Max + (Z_Max - Z_Min) + ZBase; ORIG */
    /*ZRange = ZRange_real = Z_Max + (Z_Max - Z_Min); */
    _update_zbounds ();


    UNIT_FROM_TO[FROM][X] = 0.;
    UNIT_FROM_TO[FROM][Y] = 0.;
    UNIT_FROM_TO[FROM][Z] = 0.;
    UNIT_FROM_TO[FROM][W] = 1.;
    UNIT_FROM_TO[TO][X] = 1000.;
    UNIT_FROM_TO[TO][Y] = 0.;
    UNIT_FROM_TO[TO][Z] = 0.;
    UNIT_FROM_TO[TO][W] = 1.;

    persp = 400;


    /* for transform_fromto () */
    P_pushmatrix ();

}

initialize2 ()
{
    fast_res = X_Mod = Y_Mod = XMOD;
    slow_res = X_Modr = Y_Modr = 2.;
    Range = 7;		/* index into Range_val array */
    update_fast_res();
    update_slow_res();
    update_range ();


    Display_type = D_POLY;
    Apoly->val = 1;
    Apoly->dirtycnt = 2;
    pnl_fixact (Apoly);
    Agrid->val = 0;
    Agrid->dirtycnt = 2;
    pnl_fixact (Agrid);
    Agridc->val = 0;
    Agridc->dirtycnt = 2;
    pnl_fixact (Agridc);
    Agpoly->val = 0;
    Agpoly->dirtycnt = 2;
    pnl_fixact (Agpoly);

    shading = 1;
    Ashading->val = shading;
    Ashading->dirtycnt = 2;
    pnl_fixact (Ashading);

    Z_exag = 1.;			/* (* 10 == 1.0) */
    Aexag->val = 0.1;
    Aexag->dirtycnt = 2;
    pnl_fixact (Aexag);

    if (Vect_file)
    {
    Avectz->val = 0.1;
    Avectz->dirtycnt = 2;
    pnl_fixact (Avectz);
    }

    Afringe->val = Fringe_on = 1;
    Afringe->dirtycnt = 2;
    pnl_fixact (Afringe);

    /*
    Arange1->val = 0;
    Arange1->dirtycnt = 2;
    pnl_fixact (Arange);
    */


    /* generate starting point for viewpoint */
    FROM_TO[FROM][X] = X_Mid;
    FROM_TO[FROM][Y] = Y_Min - (Y_Max - Y_Min);
    FROM_TO[FROM][Z] = Z_Max + (Z_Max - Z_Min);
    FROM_TO[FROM][W] = 1.;
    FROM_TO[TO][X] = X_Mid;
    FROM_TO[TO][Y] = Y_Mid;
    FROM_TO[TO][Z] = Z_Mid;
    FROM_TO[TO][W] = 1.;
    
    /* Init Actuators */
    Alook->val = 900.0;
    pnl_fixact (Alook);

    Atwist->val = 0.0;
    pnl_fixact (Atwist);

    Aincl->val = 300.0;
    pnl_fixact (Aincl);

    PNL_ACCESS (Point, Axy, x) = .5;
    PNL_ACCESS (Point, Axy, y) = 0.;
    pnl_fixact (Axy);

    Aheight->val = .5;
    pnl_fixact (Aheight);

    Apersp->val = 400.;
    pnl_fixact (Apersp);


    _update_persp ();
    _update_height ();
    _update_xy ();
    update_view_dir ();


    /* initialize Object Actuators */
}

redraw_event ()
{
	int x, y;
	getviewport (&left, &right, &bottom, &top);

	getorigin (&x, &y);
	xcenter = ((left + right) >> 1 ) + x;
	ycenter = ((top + bottom) >> 1 ) + y;

	cpack (0);
	clear ();
	swapbuffers ();
}

leave ()
{
    gexit ();
    exit (0);
}
