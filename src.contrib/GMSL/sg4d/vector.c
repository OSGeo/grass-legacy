
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
*/
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
int Cnt;

#define VECT_RES  (X_Res/2.0)

do_vect_display ()

{
    register int i;
    double e_ing, vect[3], prev[3];
    long elev_offset;
    int  outside, ret; 
    long color;    
    int row, col;
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
	vect[2] = Vect_z;	/* Z is fixed */
    }

    /* set line attributes - color & width */
    
    if(Adim->val){  
	if (V_Color != 0x777777)
	    color = V_Color & 0x777777;
	else
	    color = 0x333333;
    }
    else
	color = V_Color;

    cpack(color);
    linewidth ((short)V_Width);

 
    fprintf(stderr,"plotting ... ");
    /* plot vectors */

    while (!check_cancel(Avect))
    {
	if (0 > (ret = Vect_read_next_line (&Map, Points))){
	    break;
	}
	if (ret != LINE && ret != DOT && ret != AREA) break;

	Cnt = 0;
	bgnline ();
	for (i = 0 ; i < Points->n_points ; i++) 
	{   
	    outside = 0;

	    /* calculate x & y */
	    e_ing = G_adjust_easting (Points->x[i], &wind);
	    vect[0] = (e_ing - wind.west) * XYscale;
	    vect[1] = (Points->y[i] - wind.south) * XYscale;

	    if(!Aflat->val){

		/* clipping */
		outside =  (!viewcell_interp(vect));
		
		if (!outside){

		    vect[2] = (vect[2] - (double)Zoff) * Z_exag + z_up;
		
		    if (Cnt)            /* not first point in line */
			fill_line(prev, vect);	
		    prev[0] = vect[0];
		    prev[1] = vect[1];
		    prev[2] = vect[2];
		} /* end if not_outside */
		
	    } /* end if not_flat */
	    else{     /* flat clipping  */
		xres = X_Res * X_Modr;
		yres = Y_Res * Y_Modr;
		col = (int)(vect[X]/xres);
		row = (int)((Y_Max - vect[Y])/yres);
		outside = ((col | row)<0 || col > (int)(X_Max/xres)-1 ||
						row > (int)(Y_Max/yres)-1);
	    }
	    
	    if (!outside){
		vd_vert_func (vect);
		Cnt++;
		if (Cnt > 253)
		{
		    endline ();
		    bgnline ();
		    vd_vert_func (vect);
		    Cnt = 1;
		}
	    }
	    else{   /* outside */
		endline ();
		bgnline ();
		Cnt = 0;
	    }
	}
	endline ();
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


/* recursive routine to divide single line between two nodes into smaller
 * segments to more closely reflect the resolution of the elevation
 * buffer - not the ideal solution, which would be to intersect the
 * line with the edge of each triangle in the surface.*/

fill_line (vert3a, vert3c)
      double *vert3a, *vert3c;
{
      float deltaX, deltaY;
      double vert3b[3];
      long elev_offset;
      
      deltaX = fabs (vert3a[0] - vert3c[0]);
      deltaY = fabs (vert3a[1] - vert3c[1]);

      if ((deltaX + deltaY) > VECT_RES){

	  /* calculate x & y midpoint */
	  vert3b[0] = (vert3a[0] + vert3c[0])/2.0;
	  vert3b[1] = (vert3a[1] + vert3c[1])/2.0;

	  viewcell_interp(vert3b);
	/* interps elevation at vert3b[0], vert3b[1] and stores in vert3b[2] */

	  vert3b[2] = (vert3b[2] - (double)Zoff) * Z_exag + z_up;

	  fill_line(vert3a, vert3b);
	  fill_line(vert3b, vert3c);
      }
      else{
	  vd_vert_func (vert3a);
	  Cnt ++;
	  if (Cnt > 253)
	  {
	      endline ();
	      bgnline ();
	      vd_vert_func (vert3a);
	      Cnt = 1;
	  }
      }

      return (1);

}


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

}


/* viewcell_interp returns zero if the vertex is outside the region OR if
 * the "no zeros" option is toggled and the cell containing the vertex has
 * all four vertices == 0. */

viewcell_interp(v)
double v[3];
{
    double t, u;
    int row, col;
    long x2off, y2off, x1off, y1off;
    float xres, yres;
    float e1,e2,e3,e4,avg;
    int  zeros;
    
    xres = X_Res * X_Modr;
    yres = Y_Res * Y_Modr;

    col = (int)(v[X]/xres);
    row = (int)((Y_Max - v[Y])/yres);
    
    if (col < 0 || row < 0 || col > (int)(X_Max/xres)-2 ||
					row > (int)(Y_Max/yres)-2)
	return (0);                /* outside drawable region */

    x1off = col * X_Modr;
    y1off = row * Y_Modr * X_Size;
    x2off = (col+1)*X_Modr;
    y2off = (row+1)*Y_Modr * X_Size;
    
    e1 = (int)elev_buf[y1off + x1off];
    e2 = (int)elev_buf[y1off + x2off];
    e3 = (int)elev_buf[y2off + x2off];
    e4 = (int)elev_buf[y2off + x1off];
    
    zeros = 0;
    if(Anozero->val){

	if (!e1)  ++zeros;
	if (!e2)  ++zeros;
	if (!e3)  ++zeros;
	if (!e4)  ++zeros;

	/*
	if( zeros > 0) return(0);
	*/

	if(zeros){
	    if (zeros == 1){
		if (!e1) e1 = (e2 + e4)/2.; 
		if (!e2) e2 = (e1 + e3)/2.;
		if (!e3) e3 = (e2 + e4)/2.;
		if (!e4) e4 = (e3 + e1)/2.;
	    }
	    else if (zeros < 4){
		avg = (e1 + e2 + e3 + e4)/(4. - zeros);
		if (!e1) e1 = avg;
		if (!e2) e2 = avg; 
		if (!e3) e3 = avg;
		if (!e4) e4 = avg;
	    }
	}

    }
    

    t = (v[0] - col * xres)/xres;
    u = ((Y_Max - row*yres) - v[1]) /yres;

	v[2] = (1.0 - t) * (1.0 - u) * e1 +
		  t * (1.0 - u) * e2 +
		  t * u * e3 +
		  u * (1.0 - t) * e4;

    if( zeros > 3) return(0);
	
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






