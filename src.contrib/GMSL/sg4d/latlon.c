/*
**  Written by Bill Brown, Summer 1992 
**  US Army Construction Engineering Research Lab
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/

#include "externs.h"
#include "math.h"
/*
#include "gis.h"
*/



/*  changes coord (longitude, latitude, altitude) to spherical
    xyz coordinates with radius rad.
    Assumes coord has been "un_normalized" from modelling coordinates, i.e.,
    longitude = (x)/XYscale+wind.west */

/* y up */
latlon_to_sphere(coord,rad)
float coord[3];
float rad;
{
double lon, lat, slon, slat, clon, clat;
float rho;


    lon = PI - coord[0] * PI/180.; 
    lat = PI/2. - coord[1] * PI/180.;
    rho = coord[2] + rad;

    slon = sin(lon);
    clon = cos(lon);
    slat  = sin(lat);
    clat  = cos(lat);
    
    coord[0] = rho * slat * clon;
    coord[2] = rho * slat * slon;
    coord[1] = rho * clat;

}


latlon_to_sphere_zup(coord,rad)
float coord[3];
float rad;
{
double lon, lat, slon, slat, clon, clat;
float rho;


    lon = PI - coord[0] * PI/180.; 
    lat = PI/2. - coord[1] * PI/180.;
    rho = coord[2] + rad;

    slon = sin(lon);
    clon = cos(lon);
    slat  = sin(lat);
    clat  = cos(lat);
    
    coord[0] = rho * slat * clon;
    coord[1] = rho * slat * slon;
    coord[2] = rho * clat;

}

static double *Slon, *Clon, *Slat, *Clat;


/* Actually should return error if memory out and use diff draw func */
build_trigtable()
{
int i;
static int first=1;
double lat, lon;
    
    if(first){
	if(NULL == (Slon = (double *)malloc(X_Size * sizeof(double)))){
	    fprintf(stderr,"Not enough memory for lookup table\n");
	    exit(0);
	}
	if(NULL == (Clon = (double *)malloc(X_Size * sizeof(double)))){
	    fprintf(stderr,"Not enough memory for lookup table\n");
	    exit(0);
	}
	if(NULL == (Slat = (double *)malloc(Y_Size * sizeof(double)))){
	    fprintf(stderr,"Not enough memory for lookup table\n");
	    exit(0);
	}
	if(NULL == (Clat = (double *)malloc(Y_Size * sizeof(double)))){
	    fprintf(stderr,"Not enough memory for lookup table\n");
	    exit(0);
	}
	first = 0;
    }
    for(i = 0; i < X_Size; i++){
	lon = PI - (double)(wind.west + i * wind.ew_res)*PI/180.;
	Slon[i] = sin(lon);
	Clon[i] = cos(lon);
    }
    for(i = 0 ; i < Y_Size ; i++){
	lat = PI/2. - (double)(wind.north -  i*wind.ns_res)*PI/180.;
	Slat[i] = sin(lat);
	Clat[i] = cos(lat);
    }
}
	

/* y up */
/* optimized to use sin/cos lookup table when drawing on grid */
g_latlon_to_sphere(ilon, ilat, coord,rad)    
int ilon, ilat;
float coord[3];
float rad;
{
double lon, lat, slon, slat, clon, clat;
float rho;

    rho = coord[2] + rad;

    coord[0] = rho * Slat[ilat] * Clon[ilon];
    coord[2] = rho * Slat[ilat] * Slon[ilon];
    coord[1] = rho * Clat[ilat];

}



p_display_llqstrip (xmod, ymod, cancel)
    int xmod, ymod;  /* number of real cells per view cell */
    int cancel;
{
    register int row, col;
    float xres, yres;   /* world size of view cell */

    int ycnt, xcnt;    /* number of view cells across */

    float x1, x2, y1, y2, n[3], v[3], vpn[3];
    double normalizer;
    int x1off, x2off;
    long y1off, y2off;
    int wrap=0;
float test[3];


    xcnt = (X_Size % xmod? X_Size / xmod +1 : X_Size / xmod);
    ycnt = (Y_Size % ymod? Y_Size / ymod +1 : Y_Size / ymod);
    xres = (X_Res * xmod)/XYscale;
    yres = (Y_Res * ymod)/XYscale;

    wrap = (wind.east - wind.west == 360.);
    
    for (row = 0; row < ycnt - 1 ; row++) 
    {
	/* optimized */
	y1 = wind.north - row*yres; 
	y2 = wind.north - (row+1)*yres;
	y1off = row*ymod * X_Size;
	y2off = (row+1)*ymod * X_Size;

	if (!(row % 4)){     /* check every 4th row */
	    if(cancel)        /* when not in fast_display mode */
		if (check_cancel(Adraw))
		    return (1); 
	}

	x1 = wind.west;
	x1off = 0;

	bgnqstrip ();

	/* top left */ 
	FNORM(norm_buf[y1off+x1off],n);
	v[X] = x1;
	v[Y] = y1;
	v[Z] = (float)((elev_buf[y1off+x1off] - Zoff) * Z_exag);
	vpn[X] = v[X] + n[X];
	vpn[Y] = v[Y] + n[Y];
	vpn[Z] = v[Z] + n[Z];
	g_latlon_to_sphere(x1off, (int)(y1off/X_Size), v, RADIUS);
/*
	latlon_to_sphere( v, RADIUS);
*/
	if(Alight->val){
	    latlon_to_sphere(vpn,RADIUS);
	    n[X] = vpn[X] - v[X];
	    n[Y] = vpn[Y] - v[Y];
	    n[Z] = vpn[Z] - v[Z];
	    normalizer = sqrt((double)(n[X]*n[X] + n[Y]*n[Y] + n[Z]*n[Z]));
	    n[X] /= normalizer;
	    n[Y] /= normalizer;
	    n[Z] /= normalizer;
	    n3f(n);
	}
	FSET_COLOR (y1off, x1off, y1off, x1off);
	v3f(v);


	/* bottom left */
	FNORM(norm_buf[y2off+x1off],n);
	v[X] = x1;
	v[Y] = y2;
	v[Z] = (float)((elev_buf[y2off+x1off] - Zoff) * Z_exag);
	vpn[X] = v[X] + n[X];
	vpn[Y] = v[Y] + n[Y];
	vpn[Z] = v[Z] + n[Z];
	g_latlon_to_sphere(x1off, (int)(y2off/X_Size), v, RADIUS);
/*
	latlon_to_sphere( v, RADIUS);
*/
	if(Alight->val){
	    latlon_to_sphere(vpn,RADIUS);
	    n[X] = vpn[X] - v[X];
	    n[Y] = vpn[Y] - v[Y];
	    n[Z] = vpn[Z] - v[Z];
	    normalizer = sqrt((double)(n[X]*n[X] + n[Y]*n[Y] + n[Z]*n[Z]));
	    n[X] /= normalizer;
	    n[Y] /= normalizer;
	    n[Z] /= normalizer;
	    n3f(n);
	}
	FSET_COLOR (y1off, x1off, y2off, x1off);
	v3f(v);

	
	for (col = 0 ; col < xcnt-1 ; col++)
        {
				    
	    /* optimized */
	    x2 = wind.west + (col+1)*xres;
	    x1off = col * xmod;
	    x2off = (col+1)*xmod;

	    /* top right */
	    FNORM(norm_buf[y1off+x2off],n);
	    v[X] = x2;
	    v[Y] = y1;
	    v[Z] = (float)((elev_buf[y1off+x2off] - Zoff) * Z_exag);
	    vpn[X] = v[X] + n[X];
	    vpn[Y] = v[Y] + n[Y];
	    vpn[Z] = v[Z] + n[Z];
	    g_latlon_to_sphere(x2off, (int)(y1off/X_Size), v, RADIUS);
/*
	    latlon_to_sphere( v, RADIUS);
*/
	    if(Alight->val){
		latlon_to_sphere(vpn,RADIUS);
		n[X] = vpn[X] - v[X];
		n[Y] = vpn[Y] - v[Y];
		n[Z] = vpn[Z] - v[Z];
		normalizer = sqrt((double)(n[X]*n[X] + n[Y]*n[Y] + n[Z]*n[Z]));
		n[X] /= normalizer;
		n[Y] /= normalizer;
		n[Z] /= normalizer;
		n3f(n);
	    }
	    FSET_COLOR (y1off, x1off, y1off, x2off);
	    v3f(v);
/*
if(100 == row && col < 20){
    test[0] = x2;
    test[1] = y1;
    test[2] = (float)((elev_buf[y1off+x2off] - Zoff) * Z_exag);
    latlon_to_sphere(test, RADIUS);
    fprintf(stderr,"old:%f,%f,%f new:%f,%f,%f\n", 
	test[X], test[Y], test[Z], v[X], v[Y], v[Z]);
}
*/
	    /* bottom right */
	    FNORM(norm_buf[y2off+x2off],n);
	    v[X] = x2;
	    v[Y] = y2;
	    v[Z] = (float)((elev_buf[y2off+x2off] - Zoff) * Z_exag);
	    vpn[X] = v[X] + n[X];
	    vpn[Y] = v[Y] + n[Y];
	    vpn[Z] = v[Z] + n[Z];
	    g_latlon_to_sphere(x2off, (int)(y2off/X_Size), v, RADIUS);
/*
	    latlon_to_sphere( v, RADIUS);
*/
	    if(Alight->val){
		latlon_to_sphere(vpn,RADIUS);
		n[X] = vpn[X] - v[X];
		n[Y] = vpn[Y] - v[Y];
		n[Z] = vpn[Z] - v[Z];
		normalizer = sqrt((double)(n[X]*n[X] + n[Y]*n[Y] + n[Z]*n[Z]));
		n[X] /= normalizer;
		n[Y] /= normalizer;
		n[Z] /= normalizer;
		n3f(n);
	    }
	    FSET_COLOR (y1off, x1off, y2off, x2off);
	    v3f(v);

	} /* ea col */

	if(wrap){
	    /* cap to edge */
	    x2 = wind.east;
	    x1off = x2off;
	    x2off = 0;    /* wrap around */ 

	    /* top right */
	    FNORM(norm_buf[y1off+x2off],n);
	    v[X] = x2;
	    v[Y] = y1;
	    v[Z] = (float)((elev_buf[y1off+x2off] - Zoff) * Z_exag);
	    vpn[X] = v[X] + n[X];
	    vpn[Y] = v[Y] + n[Y];
	    vpn[Z] = v[Z] + n[Z];
	    g_latlon_to_sphere(x2off, (int)(y1off/X_Size), v, RADIUS);
    /*
	    latlon_to_sphere(v,RADIUS);
    */
	    if(Alight->val){
		latlon_to_sphere(vpn,RADIUS);
		n[X] = vpn[X] - v[X];
		n[Y] = vpn[Y] - v[Y];
		n[Z] = vpn[Z] - v[Z];
		normalizer = sqrt((double)(n[X]*n[X] + n[Y]*n[Y] + n[Z]*n[Z]));
		n[X] /= normalizer;
		n[Y] /= normalizer;
		n[Z] /= normalizer;
		n3f(n);
	    }
	    FSET_COLOR (y1off, x1off, y1off, x2off);
	    v3f(v);

	    /* bottom right */
	    FNORM(norm_buf[y2off+x2off],n);
	    v[X] = x2;
	    v[Y] = y2;
	    v[Z] = (float)((elev_buf[y2off+x2off] - Zoff) * Z_exag);
	    vpn[X] = v[X] + n[X];
	    vpn[Y] = v[Y] + n[Y];
	    vpn[Z] = v[Z] + n[Z];
	    g_latlon_to_sphere(x2off, (int)(y2off/X_Size), v, RADIUS);
    /*
	    latlon_to_sphere(v,RADIUS);
    */
	    if(Alight->val){
		latlon_to_sphere(vpn,RADIUS);
		n[X] = vpn[X] - v[X];
		n[Y] = vpn[Y] - v[Y];
		n[Z] = vpn[Z] - v[Z];
		normalizer = sqrt((double)(n[X]*n[X] + n[Y]*n[Y] + n[Z]*n[Z]));
		n[X] /= normalizer;
		n[Y] /= normalizer;
		n[Z] /= normalizer;
		n3f(n);
	    }
	    FSET_COLOR (y1off, x1off, y2off, x2off);
	    v3f(v);
	}

	endqstrip ();
	    
    } /* ea row */

}

ll_norm_vertex(v, n, cindex1, cindex2)
float v[3], n[3];
long cindex1, cindex2;
{
float vpn[3];
double normalizer;

        v[0] = v[0]/XYscale+wind.west; 
        v[1] = v[1]/XYscale+wind.south; 
	vpn[X] = v[X] + n[X];
	vpn[Y] = v[Y] + n[Y];
	vpn[Z] = v[Z] + n[Z];
	latlon_to_sphere(v,RADIUS);

	if(Alight->val){
	    latlon_to_sphere(vpn,RADIUS);
	    n[X] = vpn[X] - v[X];
	    n[Y] = vpn[Y] - v[Y];
	    n[Z] = vpn[Z] - v[Z];
	    normalizer = sqrt((double)(n[X]*n[X] + n[Y]*n[Y] + n[Z]*n[Z]));
	    n[X] /= normalizer;
	    n[Y] /= normalizer;
	    n[Z] /= normalizer;
	    n3f(n);
	}
	cpack(visual[Shading? cindex2: cindex1]);
        v3f(v);
}

flat_norm_vertex(v, n, cindex1, cindex2)
float v[3], n[3];
long cindex1, cindex2;
{
	n3f(n);
	cpack(visual[Shading? cindex2: cindex1]);
	v3f(v);
}

ll_vertex(x, y, z)
float x, y, z;
{
/* GLOBAL vector[3] */

        vector[0] = x/XYscale+wind.west;
        vector[1] = y/XYscale+wind.south; 
        vector[2] = z; 
        latlon_to_sphere(vector, RADIUS); 
        v3f(vector);
}

flat_vertex(x, y, z)
float x, y, z;
{
/* GLOBAL vector[3] */

	vector[0] = x;
	vector[1] = y;
	vector[2] = z;
	v3f(vector);
}

v_ll_vertex(v)
float v[3];
{

        vector[X] = v[X]/XYscale+wind.west; 
        vector[Y] = v[Y]/XYscale+wind.south; 
	vector[Z] = v[Z];
        latlon_to_sphere(vector, RADIUS); 
        v3f(vector);
}

v_flat_vertex(v)
float v[3];
{
	v3f(v);
}

vd_ll_vertex(v)
double v[3];
{
float fv[3];

        fv[X] = v[X]/XYscale+wind.west; 
        fv[Y] = v[Y]/XYscale+wind.south; 
	fv[Z] = v[Z];
        latlon_to_sphere(fv, RADIUS); 
        v3f(fv);
}

vd_flat_vertex(v)
double v[3];
{
	v3d(v);
}

get_ll_fromto(fr_to)
float fr_to[2][4];
{
	
	fr_to[FROM][X] = FROM_TO[FROM][X]/XYscale + wind.west;
	fr_to[FROM][Y] = FROM_TO[FROM][Y]/XYscale + wind.south;
	latlon_to_sphere(fr_to[FROM], RADIUS);

	if(CenterSphere)
	    fr_to[TO][X] = fr_to[TO][Y] = fr_to[TO][Z] = 0.0;
	else{
	    fr_to[TO][X] = FROM_TO[TO][X]/XYscale + wind.west;
	    fr_to[TO][Y] = FROM_TO[TO][Y]/XYscale + wind.south;
	    latlon_to_sphere(fr_to[TO], RADIUS);
	}
}



/* TODO: not working with twist - see attempts in get_ll_upvect2 */

get_ll_upvect(up)
float up[4];
{
float up_v[2][4];
float ft_dir[3];
float fr_to[2][4];
	
    get_ll_fromto(fr_to);
    get_norm_direction(ft_dir, fr_to);
    up_v[TO][X] = up_v[TO][Z] = 0.0;
    up_v[TO][Y] = 1.0;

    /* component of va on vb = va . vb / |vb| */

    up_v[FROM][X] = ft_dir[Y] * ft_dir[X]; 
    up_v[FROM][Y] = ft_dir[Y] * ft_dir[Y];
    up_v[FROM][Z] = ft_dir[Y] * ft_dir[Z]; 

    get_norm_direction(up, up_v);

}


get_ll_upvect2(up)
float up[4];
{
float v[4], dir, incl;
float fr_to[2][4];
static int variant = 0;


    get_ll_fromto(fr_to);
    get_direction (&dir, &incl, fr_to);

    v[X] = v[Z] = 0.0;
    v[Y] = v[W] = 1.0;

    P_popmatrix();
    P_pushmatrix();

    switch(variant)
    {
        case(0): 
	P_rotate ((int) dir, 'y');
	P_rotate ((int) incl, 'z');
	P_rotate((int) -Atwist->val,'x');
	break;
        case(4): 
	P_rotate((int) -Atwist->val,'x');
	P_rotate ((int) incl, 'z');
	P_rotate ((int) dir, 'y');
	break;
        case(5): 
	P_rotate((int) -Atwist->val,'x');
	P_rotate ((int) dir, 'y');
	P_rotate ((int) incl, 'z');
	break;
        case(1): 
	P_rotate ((int) dir, 'y');
	P_rotate((int) -Atwist->val,'x');
	P_rotate ((int) incl, 'z');
	break;
        case(2): 
	P_rotate ((int) incl, 'z');
	P_rotate((int) -Atwist->val,'x');
	P_rotate ((int) dir, 'y');
	break;
        case(3): 
	P_rotate ((int) incl, 'z');
	P_rotate ((int) dir, 'y');
	P_rotate((int) -Atwist->val,'x');
	break;
    }
    fprintf(stderr,"variant %d\n", variant);
    variant = (variant+1)%6;

    P_transform (1, v, up);

}




