
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


#include "gis.h"
#include "externs.h"
#include "math.h"

double G_adjust_easting();
extern norm_needupdate();


void
new_vect()
{
char   vname[30];
char  *vect_map;


    vect_map = G_ask_vector_old("enter name of new vector file", vname);

    if (vect_map == NULL){
	fprintf (stderr,  "Could not find file '%s'", vname);
    }
    else
    {
	if(Vect_file){
	    Vect_close(&Map);
	    Vect_destroy_line_struct(Points);
	}
	Vect_z = Z_Min;

	if (0 >= Vect_open_old (&Map, vname, vect_map))
	    Vect_file = 0;
	else{
	    Vect_file = 1;
	    Points = Vect_new_line_struct ();
	    Vect_set_constraint_region (&Map, wind.north, wind.south,
					wind.east, wind.west);
	    Avect->selectable = 1;
	    pnl_fixact (Avect);
	    fprintf(stderr,"%s loaded.\n", vname);
	}
    }
}
/******************************/

double z_up;

do_vect_display ()

{
    register int i;
    double e_ing, vect[3], prev[3];
    double east1, east2;
    int  ret; 
    int row, col, do_adjust;
    float xres, yres;
        

    Vect_rewind (&Map);

    z_up = 2.0 * Avectz->val * X_Res;  /*distance to float vects over surface*/
    
    if(getdisplaymode()){
	frontbuffer (1);
	backbuffer (0);
    }
    zwritemask(0x0);


    if (Aflat->val){
	do_fast_vect_display ();  
	vect[Z] = Vect_z;	/* Z is fixed */
    }

    /* set line attributes - color & width */
    cpack(Dcolor[VECT_COLOR]);
    linewidth ((short)V_Width);

    do_adjust = (wind.proj == PROJECTION_LL);
 
    fprintf(stderr,"plotting ... ");
    /* plot vectors */

    while (!check_cancel(Avect))
    {
	if (0 > (ret = Vect_read_next_line (&Map, Points))){
	    break;
	}
	if (ret != LINE && ret != DOT && ret != AREA) break;


	for (i = 0 ; i < Points->n_points ; i++) 
	{   

	    /* calculate x & y */

	    e_ing = Points->x[i] ;
	    vect[X] = (e_ing - wind.west) * XYscale;
	    vect[Y] = (Points->y[i] - wind.south) * XYscale;
	    
	    /* G_adjust_easting always makes easting greater than
	    wind.west, but this messes up clipping to edge of surface
	    on the west edge, so we have to do some checking and 
	    readjust sometimes. */


	    if(i){
		if (do_adjust){
		    
		    east1 = Points->x[i-1];
		    east2 = Points->x[i];

		    while ((east1-east2) > 180)
			east2 += 360;
		    while ((east2-east1) > 180)
			east1 += 360;
		    while (east1 > wind.east)
		    {
			east1 -= 360.0;
			east2 -= 360.0;
		    }
		    while (east1 < wind.west)
		    {
			east1 += 360.0;
			east2 += 360.0;
		    }
		    prev[X] = (east1 - wind.west) * XYscale;
		    vect[X] = (east2 - wind.west) * XYscale;

		    fill_line (prev,vect);

		    if (east2 > wind.east || east2 < wind.west)
		    {
			while (east2 > wind.east)
			{
			    east1 -= 360.0;
			    east2 -= 360.0;
			}
			while (east2 < wind.west)
			{
			    east1 += 360.0;
			    east2 += 360.0;
			}
			prev[X] = (east1 - wind.west) * XYscale;
			vect[X] = (east2 - wind.west) * XYscale;

			fill_line (prev,vect);
		    }
		}
		else
		    fill_line(prev, vect);
	    }

	    prev[X] = vect[X];
	    prev[Y] = vect[Y];
	}
    }
    Avect->val = 0;
    pnl_fixact(Avect);

    fprintf(stderr,"done.\n");

    if(getdisplaymode()){
	frontbuffer (0);
	backbuffer (1);
    }
    zwritemask(0xffffffff);


    /* reset attributes */

    linewidth (1);
    cpack (0xffffff);
}


/* draws bounding box when adjusting height of flat vector plot */

do_fast_vect_display ()
{
    double vect[3];
    
    if(Aflat->val){
	vect[2] = Vect_z;	/* Z is fixed */
	cpack(0x777777);

	bgnclosedline ();
	vect[0] = X_Min;
	vect[1] = Y_Min;
	vd_vert_func (vect);
	vect[0] = X_Min;
	vect[1] = Y_Max;
	vd_vert_func (vect);
	vect[0] = X_Max;
	vect[1] = Y_Max;
	vd_vert_func (vect);
	vect[0] = X_Max;
	vect[1] = Y_Min;
	vd_vert_func (vect);
	endclosedline ();
    }
}


/*
TODO: check the tri_interp in drape.c - doesn't seem to match
on edges of NULL - drape_get_segments shouldn't get NULL elevations
(below func should work without checking EMBNULL)
*/

fill_line(bgn, end)
double *bgn, *end;
{
int npts, i, j;
Point3 *points;

    points = drape_get_segments(bgn, end, &npts);
    if(npts<2) return(0);

    for(i=0, j=0 ; i < npts; i++){
	if(!EMBNULL(points[i][Z]) && !_point_is_masked(points[i])){
	    if(!j) bgnline();
	    if(!Aflat->val)
		points[i][Z] = (points[i][Z] - (double)Zoff) * Z_exag + z_up;
	    vd_vert_func(points[i]);
	    j++;
	    if(j > 250){
		endline();
		bgnline();
		vd_vert_func(points[i]);
		j = 1;
	    }
	}
	else if(j){
	    endline();
	    j = 0;
	}
    }
    if(j) endline();

    return(1);

}


/* No LONGER USED */
/* TODO NULL - return not used, so check when calling */
buf_interp(xo, yo, v)
int xo,yo;   /* x & y offsets from top left corner of data */
double v[3];
{
    
    double t, u;
    long elev_offset;

    elev_offset = yo * X_Size + xo;

    t = (v[0] - xo * X_Res)/X_Res;
    u = ((Y_Size - yo) * Y_Res - v[1]) /Y_Res;
/*
    fprintf(stderr,"t = %lf, u = %lf\n",t,u);
*/

    if(yo < (Y_Size - 1) && xo < (X_Size - 1)){

	v[2] = (1.0 - t) * (1.0 - u) * elev_buf[elev_offset] +
		  t * (1.0 - u) * elev_buf[elev_offset + 1] +
		  t * u * elev_buf[elev_offset + 1 + X_Size] +
		  u * (1.0 - t) * elev_buf[elev_offset + X_Size];
    }
    else{ 
    /*
	t = (v[0] - xo * X_Res)/X_Res;
	u = ((Y_Size - yo) * Y_Res - v[1]) /Y_Res;
    */
	v[2] = elev_buf[elev_offset];     /* for edges of data - fix later */
    }


}

edge_interp(v)
float v[3];
{
double dv[3];
int ret;

   dv[X] = v[X];
   dv[Y] = v[Y];
   dv[Z] = v[Z];
   ret = d_edge_interp(dv);
   v[Z] = dv[Z];

   return(ret);

}

/* NULL - returns without setting v[Z] */
d_edge_interp(v)
double v[3];
{
int xo,yo;   /* x & y offsets from top left corner of data */
double t, u;
long elev_offset;
   
    if(v[X] > X_Max || v[X] < 0.0 || v[Y] > Y_Max || v[Y] < 0.0)
	return(0);
    if(v[X] == X_Max)
	xo = X_Size - 1;
    else
	xo = v[X] / X_Res;
    if(v[Y] == Y_Max)
	yo = 0;
    else
	yo = Y_Size - 1 - v[Y]/Y_Res;
    elev_offset = yo * X_Size + xo;

    if(Has_null){
	if (EMBNULL(elev_buf[elev_offset])) return(0);
	if (EMBNULL(elev_buf[elev_offset + 1])) return(0);
	if (EMBNULL(elev_buf[elev_offset + 1 + X_Size])) return(0);
	if (EMBNULL(elev_buf[elev_offset + X_Size])) return(0);
    }
    
    t = (v[0] - xo * X_Res)/X_Res;
    u = ((Y_Size - yo) * Y_Res - v[1]) /Y_Res;
/*
    fprintf(stderr,"t = %lf, u = %lf\n",t,u);
*/

    if(yo < (Y_Size - 1) && xo < (X_Size - 1)){

	v[2] = (1.0 - t) * (1.0 - u) * elev_buf[elev_offset] +
		  t * (1.0 - u) * elev_buf[elev_offset + 1] +
		  t * u * elev_buf[elev_offset + 1 + X_Size] +
		  u * (1.0 - t) * elev_buf[elev_offset + X_Size];
    }
    else{ 
    /*
	t = (v[0] - xo * X_Res)/X_Res;
	u = ((Y_Size - yo) * Y_Res - v[1]) /Y_Res;
    */
	v[2] = elev_buf[elev_offset];     /* for edges of data - fix later */
    }

    v[Z] = (v[Z] - Zoff) * Z_exag;

    return(1);

}


/* viewcell_interp returns:
 *    -1 if the vertex is outside the region 
 *     0 if (the "no zeros" option is toggled AND the cell containing 
 *       the vertex has all four vertices == 0 || NULL)  
 *	 OR all four vertices == NULL 
 *     1 if vertex is on drawable surface 
*/

viewcell_interp(v)
double v[3];
{
    double t, u, xmax, ymin, xres, yres;
    int row, col, ok1, ok2, ok3, ok4;
    long x2off, y2off, x1off, y1off;
    float e1,e2,e3,e4,avg;
    int  omits;

    
    xres = X_Res * X_Modr;
    yres = Y_Res * Y_Modr;

    xmax = (int)(X_Max/xres) * xres;
    ymin = Y_Max - (int)(Y_Max/yres) * yres;
    
    if(v[X] < 0. || v[X] > xmax || v[Y] < ymin || v[Y] > Y_Max)
	return (-1);                /* outside region */

    col = (int)(v[X]/xres);
    row = (int)((Y_Max - v[Y])/yres);

    /* edges */
    while (col * X_Modr > X_Size) col--;
    while (row * Y_Modr > Y_Size) row--;

    x1off = col * X_Modr;
    y1off = row * Y_Modr * X_Size;
    x2off = (col+1)*X_Modr;
    y2off = (row+1)*Y_Modr * X_Size;
    
    e1 = elev_buf[y1off + x1off];
    e2 = elev_buf[y1off + x2off];
    e3 = elev_buf[y2off + x2off];
    e4 = elev_buf[y2off + x1off];
    
    omits = 0;
    ok1 = ok2 = ok3 = ok4 = 1;
    if(Anozero->val){
	if (!e1){ ok1 = 0; ++omits;}
	if (!e2){ ok2 = 0; ++omits;}
	if (!e3){ ok3 = 0; ++omits;}
	if (!e4){ ok4 = 0; ++omits;}
    }
    if(Has_null){
	if (EMBNULL(e1)){ e1 = 0.0; ok1 = 0; ++omits;}
	if (EMBNULL(e2)){ e2 = 0.0; ok2 = 0; ++omits;}
	if (EMBNULL(e3)){ e3 = 0.0; ok3 = 0; ++omits;}
	if (EMBNULL(e4)){ e4 = 0.0; ok4 = 0; ++omits;}
    }

    if(omits){
	if (omits == 1){
	    if (!ok1) e1 = (e2 + e4)/2.; 
	    if (!ok2) e2 = (e1 + e3)/2.;
	    if (!ok3) e3 = (e2 + e4)/2.;
	    if (!ok4) e4 = (e3 + e1)/2.;
	}
	else if (omits < 4){
	    avg = (e1 + e2 + e3 + e4)/(4. - omits);
	    if (!ok1) e1 = avg;
	    if (!ok2) e2 = avg; 
	    if (!ok3) e3 = avg;
	    if (!ok4) e4 = avg;
	}
    }
    

    t = (v[0] - col * xres)/xres;
    u = ((Y_Max - row*yres) - v[1]) /yres;

	v[2] = (1.0 - t) * (1.0 - u) * e1 +
		  t * (1.0 - u) * e2 +
		  t * u * e3 +
		  u * (1.0 - t) * e4;

    if( omits > 3) {
	return(0);
    }
	
    return (1);

}


vcellnorm_interp(v, norm)
float v[3], norm[3];
{
    double t, u;
    int row, col,i;
    long x2off, y2off, x1off, y1off;
    float xres, yres;
    float tl[3], tr[3], br[3], bl[3];
    
    xres = X_Res * X_Modr;
    yres = Y_Res * Y_Modr;

    col = (int)(v[X]/xres);
    row = (int)((Y_Max - v[Y])/yres);

    /*  return 0 if norms haven't been calculated for current view */
    if(norm_needupdate(0)) return(0);

    if (col > (int)(X_Max/xres)-1 || row > (int)(Y_Max/yres)-1)
	return(0);                 /* outside drawable region */

    x1off = col * X_Modr;
    y1off = row * Y_Modr * X_Size;
    x2off = (col+1)*X_Modr;
    y2off = (row+1)*Y_Modr * X_Size;

    if(Has_null){
	if (EMBNULL(elev_buf[y1off + x1off])) return(0);
	if (EMBNULL(elev_buf[y1off + x2off])) return(0);
	if (EMBNULL(elev_buf[y2off + x2off])) return(0);
	if (EMBNULL(elev_buf[y2off + x1off])) return(0);
    }
    
    t = (v[0] - col * xres)/xres;
    u = ((Y_Max - row*yres) - v[1]) /yres;

    FNORM(norm_buf[y1off + x1off],tl);
    FNORM(norm_buf[y1off + x2off],tr);
    FNORM(norm_buf[y2off + x2off],br);
    FNORM(norm_buf[y2off + x1off],bl);
    
    for(i=0; i<3; i++){
	norm[i] = (1.0 - t) * (1.0 - u) * tl[i] +
		  t * (1.0 - u) * tr[i] +
		  t * u * br[i] +
		  u * (1.0 - t) * bl[i];
    }
/*
fprintf(stderr,"%f, %f, %f\n",norm[X], norm[Y], norm[Z]);
*/
    return (1);

}


