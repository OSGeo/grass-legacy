/*
**  Written by Dave Gerdes  Summer 1990
**  US Army Construction Engineering Research Lab
*/

/*
** Enhancements made winter 1991 - 1992 by Bill Brown
** US Army Construction Engineering Research Lab
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/
/*
#include "gis.h"
#include <gl.h>
#include "Vect.h"
*/
#include <device.h>
#include <math.h>


/*#define MAIN*/
#include "externs.h"

long Main_Win;
extern float Range_val[];

/*double atof();*/
char *getenv();
extern void set_font();
extern void calculate_model();
extern void _update_lightpos ();
extern build_trigtable();
extern latlon_to_sphere();
extern flat_vertex();
extern flat_norm_vertex();
extern v_flat_vertex();
extern vd_flat_vertex();
void remap_pnl_colors();
void do_fast_display();
void redraw_event();

int corners[8][3];


Dcell (elev, name1, name2, name3, vname, sname, scolname, v3name)
    char *elev;
    char *name1;
    char *name2;
    char *name3;
    char *vname;
    char *sname;
    char *scolname;
    char *v3name;
{
    char *elev_map;
    char *name1_map;
    char *name2_map;
    char *name3_map;
    char *vect_map;
    char *site_map;
    char *sitecol_map;
    char *view_map;
    int i;
    /*struct Cell_head wind;*/
    char buff[128];
    FILEDESC cellfile1 = NULL;
    FILEDESC cellfile2 = NULL;
    FILEDESC cellfile3 = NULL;
    FILEDESC elev_cell = NULL;
    CELL *xarray;
    struct Range range;
    long *color_array;
    int row, col;
    int *int_buf;
    long size;
    int t, b, l, r;
    char *p;
    int j, k;
    int dev;
    short val;
    /*float Z_Span, Z_Span2;*/

    float xfrom, yfrom, zfrom, xto, yto, zto;

    XYscale = 1.0;

    if (name2 == NULL || name3 == NULL)
	Three_map = 0;
    else
	Three_map = 1;
	
    G_get_set_window (&wind) ; /* ok */
    if(NULL == (elev_map = G_find_file2 ("cell", elev, ""))){
	sprintf(buff,"Not able to find cellfile for [%s]", elev);
	G_fatal_error(buff);
    }

    X_Res = wind.ew_res;
    Y_Res = wind.ns_res;

    /*
    **  have to shrink down the world space
    **  because of Z-buffer accuracy limitations
    */
    X_Min_real = wind.west;
    X_Max_real = wind.east;
    Y_Min_real = wind.south;
    Y_Max_real = wind.north;
    
    /* scale largest dimension to 1000 */

    if((wind.east - wind.west) > (wind.north - wind.south))
	XYscale = 1000.0 / (wind.east - wind.west);
    else
	XYscale = 1000.0 / (wind.north - wind.south);

    X_Res *= XYscale;
    Y_Res *= XYscale;

    {
	X_Min = 0;
	X_Max = XYscale * (wind.east - wind.west);
	Y_Min = 0;
	Y_Max = XYscale * (wind.north - wind.south);

    }


    X_Mid = (X_Max + X_Min) / 2.;
    Y_Mid = (Y_Max + Y_Min) / 2.;
    
/******************************/
    if (vname == NULL)
	Vect_file = 0;
    else
    {
	if (NULL == (vect_map = G_find_file2 ("dig", vname, "")))
	{
	    fprintf (stderr,  "Could not find file '%s'", vname);
	    Vect_file = 0;
	}
	else
	{
	    Vect_file = 1;
	    Vect_z = Z_Min;

	    if (0 >= Vect_open_old (&Map, vname, vect_map))
		Vect_file = 0;
	    else
	    {
		Points = Vect_new_line_struct ();
		Vect_set_constraint_region (&Map, wind.north, wind.south, wind.east, wind.west);
	    }
	}
    }
/******************************/
    Map_Sitecolor = 0;
    if(scolname){
	sitecol_map = G_find_file2 ("cell", scolname, "");
	if (sitecol_map == NULL){
	    fprintf (stderr,  "Could not find file '%s'", scolname);
	    Map_Sitecolor = 0;
	}
	else{
	    G_read_colors (scolname, sitecol_map, &Scolor);
	    Map_Sitecolor = 1;
	}
    }
    
/******************************/
    if (sname == NULL)
	 Site_file = 0;
    else
    {
	Site_file = 1;
	site_map = G_find_file2 ("site_lists", sname, "");
	if (site_map == NULL){
	    fprintf (stderr,  "Could not find file '%s'", sname);
	    Site_file = 0;
	}
	else{
	    Sitefd = G_fopen_sites_old (sname, site_map);
	    if (Sitefd == NULL){
		fprintf (stderr, "can't open sites file [%s]", sname);
		Site_file = 0;
	    }
	}
    }	

/******************************/
    if(View_file)
    {
	view_map = G_find_file2 ("3d.view", v3name, "");
	if (view_map == NULL){
	    fprintf (stderr,  "Could not find file '%s'", v3name);
	    View_file = 0;
	}
    }


    i = j = k = 0;


/******************************************************************************/

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
/*                          no longer used                                    */
/******************************************************************************/

    foreground ();


    {
	int x1, x2, y1, y2;
	int height, width;

	if (p = getenv ("SG3D_WIDTH"))
	    width = atoi(p) < getgdesc(GD_XPMAX) ? atoi(p):getgdesc(GD_XPMAX);
	else width = getgdesc(GD_XPMAX) - 420; /*420 = width of widest panel*/
	if (p = getenv ("SG3D_HEIGHT"))
	    height = atoi(p) < getgdesc(GD_YPMAX) ? atoi(p):getgdesc(GD_YPMAX);
	else height = (int)(width * .8);
	x1 = 10; x2 = 10 + width;
	y2 = 1000; y1 = 1000 - height;

	/*prefposition (10, 700, 300, 1000);*/
	prefposition (x1, x2, y1, y2);
	Main_Win = winopen ("3D SGI");
	winconstraints ();
	fminit();
    }
/*
    concave (1);
*/

    RGBmode ();
    subpixel(TRUE);
    gconfig ();

    mmode (MVIEWING); 
    
    getviewport (&left, &right, &bottom, &top);

    getorigin (&X_Size, &Y_Size); /* WILL be reused as Wind.size */
    xcenter = ((left + right) >> 1 ) + X_Size;
    ycenter = ((top + bottom) >> 1 ) + Y_Size;

    zbuffer (1);
    lsetdepth (getgdesc(GD_ZMIN), getgdesc(GD_ZMAX));
    zfunction (ZF_LEQUAL);

    cpack (0);
    clear ();
    zclear ();

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

    size = X_Size * Y_Size;
#ifdef USE_SHORT
    elev_buf = (short *)G_malloc (size * sizeof (short));
#else
#ifdef USE_CHAR
    elev_buf = (unsigned char *)G_malloc (size * sizeof (unsigned char));
#else
    elev_buf = (int *)G_malloc (size * sizeof (int));
#endif
#endif
    norm_buf = (unsigned int*)G_malloc (size * sizeof(unsigned int));

/******************************************************************************/


    fprintf (stderr, "Initial load of data:  ");

#ifdef USE_SHORT
    int_buf = (int *)G_malloc (X_Size * sizeof (int));
#endif
#ifdef USE_CHAR
    int_buf = (int *)G_malloc (X_Size * sizeof (int));
#endif
    {
    int row_off;
    short *ts;
    unsigned char *tc;
    int *ti;
    for (row = 0; row < Y_Size ; row++) {

	row_off = row * X_Size;
#ifdef USE_SHORT
	G_get_map_row(elev_cell, int_buf, row) ; 
	ts = &(elev_buf[row_off]);
	ti = int_buf;
	for(col=0; col < X_Size; col++)
	    *ts++ = *ti++;
#else
#ifdef USE_CHAR
	G_get_map_row(elev_cell, int_buf, row) ; 
	tc = &(elev_buf[row_off]);
	ti = int_buf;
	for(col=0; col < X_Size; col++)
	    *tc++ = *ti++;
#else
	G_get_map_row(elev_cell, &(elev_buf[row_off]), row) ; 
#endif
#endif
    }
    }
#ifdef USE_SHORT
    free(int_buf);
#endif
#ifdef USE_CHAR
    free(int_buf);
#endif
    G_close_cell(elev_cell);

    if (Three_map)
	_newcell (name1, name2, name3);
    else if(strcmp(name1, elev))          /* elev file is not color file */
	_newcell (name1, NULL, NULL);
    else 
	_newcell_is_elev(name1, elev_map);

{
    /* get Z range */
    Z_Min_real = Z_Max_real = elev_buf[0];
    for(i = 1; i < size; ++i){
	if(elev_buf[i] > Z_Max_real) Z_Max_real = elev_buf[i];
	else if(elev_buf[i] < Z_Min_real) Z_Min_real = elev_buf[i];
    }
    if(0 == Z_Min_real){
	Z_Min_notzero = 9999999.;  /* BIG */
	for(i = 1; i < size; ++i){
	    if(elev_buf[i] && elev_buf[i] < Z_Min_notzero)
		Z_Min_notzero = elev_buf[i];
	}
    }
    else
	Z_Min_notzero = Z_Min_real;
	
	
    Z_Mid_real = (Z_Max_real + Z_Min_real) / 2.;
}    
    fprintf (stderr, "elevation range: %f to %f\n", Z_Min_real, Z_Max_real);
    fprintf (stderr, "Done.\n");

/******************************************************************************/


    initialize2 ();

    if(View_file)
	get_settings(v3name);
    
    do_fast_display();


    {
	Actuator *a;
	static int first=1;
	static int init=1;
        
	redraw_ok = 1;
	while (1)
	{
	    extern Actuator *filetypein;	/* in script.c */

	    a = pnl_dopanel ();
            
	    if(init){
		if(8 == getplanes())
		    remap_pnl_colors();    /* for indigo compatibility */
		init = 0;
	    }
	    
	    if (Main_Win == pnl_userredraw ())
	    {
		redraw_ok = 1;
		reshapeviewport ();
		redraw_event ();
	    }

	    serve_actuator (a);

	    if (first){
		first = 0;
		P_Script->visible = 0;
		pnl_fixpanel (P_Script);

		if( AUTO_FILE != NULL){
		     strcpy (PNL_ACCESS(Typein, filetypein, str), AUTO_FILE);
		     pnl_beginreadscript(PNL_ACCESS(Typein, filetypein, str));
		}
	    }
	}
    }
}


reset_Z_Range()
{

    Zoff = Z_Min_real;	/* offset to add to get real Z */

    Z_Min = 0;
    Z_Max = XYscale * (Z_Max_real - Z_Min_real);
    Z_Mid = (Z_Max + Z_Min) / 2.;
    Z_Span_real = Z_Span = (Z_Max_real - Z_Min_real);

    Z_exag = 1.0;
    Range = 7;
    while (Z_Max*Z_exag >= 250. && Range < 14){
	Range++;
	Z_exag *= .1 ; 
    }
    while (Z_Max*Z_exag < 25. && Range >= 0){
	Range--;
	Z_exag *= 10.; 
    }

    Init_Range = Range;
    Z_exag *= XYscale;
}

float
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
    float x, y, z, coord[2][3], fr_to[2][4];
    float look_theta;
    float alpha, beta;
    float zup[3], yup[3], zupmag, yupmag;


    x = FROM_TO[FROM][X];
    y = FROM_TO[FROM][Y];
    z = FROM_TO[FROM][Z];

    if(x < X_Max + 20. && x > X_Min - 20. && 
	    y < Y_Max + 20. && y > Y_Min - 20. && 
	    z < Z_Max + 20. && z > Z_Min - 20.){
	nearclip = 0.01;
	farclip = 1500.0;
    }
    else{
	nearclip = 20.0;
	farclip = 4000.0;
    }

    getviewport (&left, &right, &bottom, &top);
    aspect = (float) (right-left) / (top-bottom);

    if(Aortho->val){
	Osize = 1 + 2000 * Aheight->val;
	if(FROM_TO[FROM][X] < X_Min)
	    Osize += (X_Min - FROM_TO[FROM][X]);
	if(FROM_TO[FROM][X] > X_Max)
	    Osize += (FROM_TO[FROM][X] - X_Max);
	if(FROM_TO[FROM][Y] < Y_Min)
	    Osize += (Y_Min - FROM_TO[FROM][Y]);
	if(FROM_TO[FROM][Y] > Y_Max)
	    Osize += (FROM_TO[FROM][Y] - Y_Max);

	ortho(-Osize*aspect,Osize*aspect,-Osize,Osize, nearclip, farclip);
    }
    else
	perspective (persp, aspect, nearclip, farclip);

    loadmatrix(ID_matrix);

/* ******************************************************************** */
    /* This block of code is used to keep pos z in the up direction,
     * correcting for system default which is pos y in the up
     * direction.  Involves finding up vectors for both y up and
     * z up, then determining angle between them.  LatLon mode uses y as
     * up direction instead of z, so no correction necessary.  Next rewrite,
     * we should use y as up for all drawing.
    */
    fr_to[FROM][X] = FROM_TO[FROM][X];
    fr_to[FROM][Y] = FROM_TO[FROM][Y];
    fr_to[FROM][Z] = FROM_TO[FROM][Z];
    fr_to[TO][X] = FROM_TO[TO][X];
    fr_to[TO][Y] = FROM_TO[TO][Y];
    fr_to[TO][Z] = FROM_TO[TO][Z];

    update_up_vect();

    if(!LatLon)
    {

    /* neg alpha OK since sin(-x) = -sin(x) */
    alpha = PI/2.0 - facos(fr_to[FROM][Z] - fr_to[TO][Z]);

    zup[X] = fr_to[TO][X];
    zup[Y] = fr_to[TO][Y];

    if(fsin(alpha))
	zup[Z] = fr_to[TO][Z] + 1 / fsin(alpha);
    else
	zup[Z] = fr_to[FROM][Z] + 1.0;

    zupmag = distance(fr_to[FROM],zup);

    yup[X] = fr_to[TO][X];
    yup[Z] = fr_to[TO][Z];

    /* neg beta OK since sin(-x) = -sin(x) */
    beta = PI/2.0 - facos(fr_to[TO][Y] - fr_to[FROM][Y]);
    if(fsin(beta)) yup[Y] = fr_to[TO][Y] - 1 / fsin(beta);
    else yup[Y] = fr_to[FROM][Y] + 1.0;

    yupmag = distance(fr_to[FROM],yup);

    look_theta = (1800.0/PI) * 
		facos(((zup[X]-fr_to[FROM][X])*(yup[X]-fr_to[FROM][X])
		+ (zup[Y]-fr_to[FROM][Y])*(yup[Y]-fr_to[FROM][Y]) 
		+ (zup[Z]-fr_to[FROM][Z])*(yup[Z]-fr_to[FROM][Z]))/
		(zupmag * yupmag));

    if(fr_to[TO][X] - fr_to[FROM][X] < 0.0) look_theta = -look_theta;
    if(fr_to[TO][Z] - fr_to[FROM][Z] < 0.0){ /* looking down */
	if(fr_to[TO][Y] - fr_to[FROM][Y] < 0.0)
	    look_theta = 1800 - look_theta;
    }
    else{  /* looking up */
	if(fr_to[TO][Y] - fr_to[FROM][Y] > 0.0) 
	    look_theta = 1800 - look_theta;
    }
    }

/* ******************************************************************** */
    
    /*  Original
    lookat(FROM_TO[FROM][X],FROM_TO[FROM][Y],FROM_TO[FROM][Z],
           FROM_TO[TO][X],FROM_TO[TO][Y],FROM_TO[TO][Z],
	   (int)(Atwist->val + Alook->val-900));
    */
    if(LatLon){
	get_ll_fromto(fr_to);
	lookat(fr_to[FROM][X],fr_to[FROM][Y],fr_to[FROM][Z],
	       fr_to[TO][X],fr_to[TO][Y],fr_to[TO][Z], (int)Atwist->val);
    }
    else
	lookat(fr_to[FROM][X],fr_to[FROM][Y],fr_to[FROM][Z],
           fr_to[TO][X],fr_to[TO][Y],fr_to[TO][Z], (int)(Atwist->val +
	   1800 + look_theta));


    New_view = 1;
}



initialize ()
{
/*
    linesmooth (SML_ON);
*/
    install_panels ();
    XBase = X_Min - (X_Max - X_Mid) * RANGE_FACTOR;
    YBase = Y_Min - (Y_Max - Y_Mid) * RANGE_FACTOR;
    ZBase = Z_Min;

    XRange = X_Max + (X_Max - X_Mid) * RANGE_FACTOR - XBase;
    YRange = Y_Max + (Y_Max - Y_Mid) * RANGE_FACTOR - YBase;
/*
    XRange = YRange = 1000 * (RANGE_FACTOR + 1.0);
*/
    _update_zbounds ();


    UNIT_FROM_TO[FROM][X] = 0.;
    UNIT_FROM_TO[FROM][Y] = 0.;
    UNIT_FROM_TO[FROM][Z] = 0.;
    UNIT_FROM_TO[FROM][W] = 1.;
    UNIT_FROM_TO[TO][X] = 1.;
    UNIT_FROM_TO[TO][Y] = 0.;
    UNIT_FROM_TO[TO][Z] = 0.;
    UNIT_FROM_TO[TO][W] = 1.;

    persp = 400;

    calculate_model();  /* viewing model for light adjustment */
    if(PROJECTION_LL == wind.proj)  {
	CenterSphere = 1;
	build_trigtable();  /* for mapping lat-long data to globe */
    }

    /* for transform_fromto () */
    P_pushmatrix ();

}

g_default_flat_view()
{
    FROM_TO[FROM][X] = X_Mid;
    FROM_TO[FROM][Y] = Y_Min - (Y_Max - Y_Min);
    FROM_TO[FROM][Z] = .25 * 3000.;
    FROM_TO[FROM][W] = 1.;
    FROM_TO[TO][X] = X_Mid;
    FROM_TO[TO][Y] = Y_Mid;
    FROM_TO[TO][Z] = Z_Min;
    FROM_TO[TO][W] = 1.;
}

g_default_ll_view()
{
    FROM_TO[FROM][X] = X_Mid;
    FROM_TO[FROM][Y] = Y_Mid;
    FROM_TO[FROM][Z] = .25 * 3000.; 
    FROM_TO[FROM][W] = 1.;
    FROM_TO[TO][X] = 0.;
    FROM_TO[TO][Y] = 0.;
    FROM_TO[TO][Z] = 0.;
    FROM_TO[TO][W] = 1.;
}

initialize2 ()
{
    int f_res;
    char buffer[80];

    /* correct Z_range in case elevation file changed (also see newelev.c) */
    reset_Z_Range();

    f_res = (int)((X_Size < Y_Size? X_Size: Y_Size) / 30.0);
    if (!f_res) f_res = 1;
    fast_res = X_Mod = Y_Mod = f_res;
    slow_res = X_Modr = Y_Modr = 2;
    Range = Init_Range;		/* index into Range_val array */
    InFocus = 0;
    LatLon = 0;
    update_fast_res();
    update_slow_res();
    Model_showing = 0;
    V_Width = 1;
    update_vect_width();
    set_font();


    Display_type = D_POLY;
    Apoly->val = 1;
    pnl_fixact (Apoly);
    Agrid->val = 0;
    pnl_fixact (Agrid);
    Agridc->val = 0;
    pnl_fixact (Agridc);
    Agpoly->val = 0;
    pnl_fixact (Agpoly);

    Anozero->val = 0;
    pnl_fixact (Anozero);

    Alight->val = 1;
    pnl_fixact (Alight);

    Asurface->val = 0;
    pnl_fixact (Asurface);

    Abright->val = 0.85;
    pnl_fixact (Abright);

    Ashine->val = 0.3;
    pnl_fixact (Ashine);

    Aambient->val = 0.1;
    pnl_fixact (Aambient);
    
    Aflat->val = 0;
    pnl_fixact(Aflat);
    
    V_Color = 0xFFFFFF;
    Avcolor->val = 7;
    pnl_fixact(Avcolor);

    Adim->val = 0;
    pnl_fixact (Adim);
    
    BGcolor = 0x000000;
    Abgcolor->val = 0;
    pnl_fixact(Abgcolor);

    STcolor = 0x00FF00;
    Astcolor->val = 2;
    pnl_fixact(Astcolor);

    Adrape->val = 1;
    pnl_fixact(Adrape);

    Afocus->val = 0;
    pnl_fixact(Afocus);

    Atriangle->val = 0;
#ifdef XS24
    Atriangle->val = 1;
#endif
    pnl_fixact (Atriangle);

    Shading = 1;
    Ashading->val = Shading;
    pnl_fixact (Ashading);

    if (!Vect_file){
	Avect->selectable = 0;
	pnl_fixact (Avect);
    }
    Avectz->val = 0.2;
    pnl_fixact (Avectz);
    
    if (!Site_file){
	Asites->selectable = 0;
	pnl_fixact (Asites);
    }

    Afringe->val = Fringe_on = 0;
    pnl_fixact (Afringe);

    Adotype->val = 0;
    pnl_fixact (Adotype);
    
    /* Initialize vertex functions */
    vert_func = (Func_ptr *)flat_vertex;
    normvert_func = (Func_ptr *)flat_norm_vertex;
    v_vert_func = (Func_ptr *)v_flat_vertex;
    vd_vert_func = (Func_ptr *)vd_flat_vertex;

    if(PROJECTION_LL == wind.proj)  /* Lat-Long */
	Aglobe->visible = 1;
    else
	Aglobe->visible = 0;

    Aglobe->val = 0;
    pnl_fixact (Aglobe);

    Alook->val = 900.0;
    pnl_fixact (Alook);

    Atwist->val = 0.0;
    pnl_fixact (Atwist);

    Aincl->val = 300.0;
    pnl_fixact (Aincl);

    /* generate starting point for viewpoint */
    g_default_flat_view();
    
    /* generate starting point for light */
    /* NW corner, middle height  */
    LightPos[X] = XBase;
    LightPos[Y] = YRange;
    LightPos[Z] = Z_Max;
    _update_lightpos ();

    /* Init Actuators */
    PNL_ACCESS (Point, Axy, x) = .5;
    PNL_ACCESS (Point, Axy, y) = 0.;
    pnl_fixact (Axy);
/*
    PNL_ACCESS (Point, Alightxy, x) = .7;
    PNL_ACCESS (Point, Alightxy, y) = 0.;
    pnl_fixact (Alightxy);

    Altheight->val = 1.0;
    pnl_fixact (Altheight);
*/

    Afollow->val = 1;
    pnl_fixact (Afollow);

    Ared->val = 0.9;
    pnl_fixact (Ared);

    Agrn->val = 0.9;
    pnl_fixact (Agrn);

    Ablu->val = 0.9;
    pnl_fixact (Ablu);

    Ashowmod->val = 1;
    pnl_fixact (Ashowmod);

    Aheight->val = .25;
    pnl_fixact (Aheight);

    Apersp->val = 400.;
    pnl_fixact (Apersp);

    Aexag->val = Range_val[Range];
    Aexag->maxval = 10 * Range_val[Range];
    pnl_fixact (Aexag);

    Alevel->val = 0;
    pnl_fixact(Alevel);

    Aterrain->val = 1;
    pnl_fixact(Aterrain);

    Alookforward->val = 0;
    pnl_fixact(Alookforward);

    Apathtilt->val = 0.5;
    pnl_fixact(Apathtilt);

    strcpy(PNL_ACCESS(Typein, Apathht, str),"500.00");
    pnl_fixact(Apathht);

    sprintf(buffer,"%.2f",Z_Max_real);
    strcpy(PNL_ACCESS(Typein, Apathel, str),buffer);
    pnl_fixact(Apathel);

    Atension->val = 0.8;
    pnl_fixact(Atension);

    Aptension->val = 0.9;  /* higher tension for vect path, prob more nodes */
    pnl_fixact(Aptension);

    Numkeys = 0;
    Viewsteps = 25;  /* default */
    
    _update_persp ();
    _update_xy ();
    update_range ();

}

void
redraw_event ()
{
	int x, y;
	getviewport (&left, &right, &bottom, &top);

	getorigin (&x, &y);
	xcenter = ((left + right) >> 1 ) + x;
	ycenter = ((top + bottom) >> 1 ) + y;
	update_projection ();

	cpack (BGcolor);
	clear ();
	do_fast_display();
	if(getdisplaymode())
	    swapbuffers ();
	    
}

leave ()
{
    gexit ();
    exit (0);
}

void  
remap_pnl_colors() 
{
      pnl_white_color 		= 7; 
      pnl_black_color 		= 0; 
      pnl_bevel_light_color 	= PNL_4BIT_BEVEL_LIGHT_COLOR;
      mapcolor(pnl_bevel_light_color, PNL_RGB_BEVEL_LIGHT_COLOR);
      pnl_normal_color 		= PNL_4BIT_NORMAL_COLOR;
      mapcolor(pnl_normal_color, PNL_RGB_NORMAL_COLOR);
      pnl_background_color 	= PNL_4BIT_BACKGROUND_COLOR;
      mapcolor(pnl_background_color, PNL_RGB_BACKGROUND_COLOR);
      pnl_other_color 		= PNL_4BIT_OTHER_COLOR;
      mapcolor(pnl_other_color, PNL_RGB_OTHER_COLOR);
      pnl_highlight_color 	= PNL_4BIT_HIGHLIGHT_COLOR;
      mapcolor(pnl_highlight_color, PNL_RGB_HIGHLIGHT_COLOR);
      pnl_bevel_dark_color 	= PNL_4BIT_BEVEL_DARK_COLOR;
      mapcolor(pnl_bevel_dark_color, PNL_RGB_BEVEL_DARK_COLOR);
      pnl_label_color 		= PNL_4BIT_LABEL_COLOR;
      mapcolor(pnl_label_color, PNL_RGB_LABEL_COLOR);

}


update_up_vect()
{
float v[4];
    
    if(LatLon){
	get_ll_upvect(UP_VECT);
	return(1);
    }

    v[X] = 0.0;
    v[W] = 1.0; 
    
    v[Y] = 0.0;
    v[Z] = 1.0; 

    P_popmatrix();
    P_pushmatrix();
    P_rotate ((int) Alook->val, 'z');
    P_rotate ((int) Aincl->val, 'y');
    P_rotate((int) -Atwist->val,'x');
    P_transform (1, v, UP_VECT);


}

