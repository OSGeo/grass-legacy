
/*
**  Written by Bill Brown, Winter 1991 - 1992 
**  US Army Construction Engineering Research Lab
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/


#include <math.h>
#include "externs.h"
#include "device.h"
#include "gis.h"

void recalc_normals();
void recalc_normals2();
void calculate_model(); /*  model for positioning lights  */ 
void position_model();
void draw_model();
void do_lights();
int whats_here();
extern float distance();
int get_centroid();

norm_needupdate(update)
int update;     /* boolean 0 means just checking, 1 means norms recalculated */
{
  static float old_zscale = 0.0;
  static int old_res = 0;
     
      if(!old_res){    /* first call */ 
	  if(update){
	      old_zscale = Z_exag;
	      old_res = X_Modr;
	      recalc_normals(X_Modr, Y_Modr, elev_buf, norm_buf, Z_exag);
	  }
	  return(1);
      }
      if(Alight->val && (old_zscale != Z_exag || old_res != X_Modr)){
	  if(update){
	      old_zscale = Z_exag;
	      old_res = X_Modr;
	      recalc_normals(X_Modr, Y_Modr, elev_buf, norm_buf, Z_exag);
	  }
	  return(1);
      }	
      return(0);       /* normals are current */ 
}

void
do_lights(check_normals)
int check_normals; /* boolean - need to check surface norms? */
{

  static float white_light1[]={
      LCOLOR, 1.00, 1.00, 1.00,
      POSITION, 10.0, 200.0, 50.0, 1.0,
      AMBIENT, 0.0,0.0,0.0,
      LMNULL};

  static float white_light2[]={
      LCOLOR, 0.25, 0.25, 0.25,
      POSITION, 0.0, 0.0, 10.0, 0.0,
      AMBIENT, 0.2,0.2,0.2,
      LMNULL};

   static float dull_material[]={
      AMBIENT, 0.1, 0.1, 0.1,
      DIFFUSE, 0.8, 0.8, 0.8,
      SPECULAR, 0.4, 0.4, 0.4,
      SHININESS, 20.0,
      LMNULL};   

   static float glossy_material[]={
      AMBIENT, 0.1, 0.1, 0.1,
      DIFFUSE, 0.9, 0.8, 0.9,
      SPECULAR, 0.7, 0.7, 0.8,
      SHININESS, 90.0,
      LMNULL};   

  static float model[]={
      ATTENUATION, 1.0, 0.0,
      LMNULL};

  static int first = 1; 
  static int n = 0; 
  float lightpos[3];

	if(LatLon){
	    lightpos[X] = LightPos[X]/XYscale + wind.west;
	    lightpos[Y] = LightPos[Y]/XYscale + wind.south;
	    lightpos[Z] = LightPos[Z];
	    latlon_to_sphere(lightpos, RADIUS);
	    white_light1[5] = lightpos[X];  
	    white_light1[6] = lightpos[Y];  
	    white_light1[7] = lightpos[Z];
	    white_light1[8] = 0.0;   /* infinite */
	}
	else{
	    white_light1[5] = LightPos[X];  
	    white_light1[6] = LightPos[Y];  
	    white_light1[7] = LightPos[Z];
	    white_light1[8] = 1.0;   /* local */
	}

#ifdef MATCOLR

	    white_light1[1] = Abright->val;
	    white_light1[2] = Abright->val;
	    white_light1[3] = Abright->val;

	    glossy_material[1] = Aambient->val * Ared->val;
	    glossy_material[2] = Aambient->val * Agrn->val;
	    glossy_material[3] = Aambient->val * Ablu->val;
				
	    glossy_material[5] = Ared->val;
	    glossy_material[6] = Agrn->val;
	    glossy_material[7] = Ablu->val;

#else /* color sliders represent light color */

	    white_light1[1] = Abright->val * Ared->val;
	    white_light1[2] = Abright->val * Agrn->val;
	    white_light1[3] = Abright->val * Ablu->val;
#endif

	white_light1[10] = 0.5 * Aambient->val;
	white_light1[11] = 0.5 * Aambient->val;
	white_light1[12] = 0.5 * Aambient->val;
	white_light2[10] = white_light2[11] = white_light2[12] = Aambient->val;
	white_light2[1] = white_light2[2] = white_light2[3] = Aambient->val;
	dull_material[1] = dull_material[2] =
				dull_material[3] = 0.5 * Aambient->val;

	dull_material[9] = dull_material[10] =
				dull_material[11] = Ashine->val;
	glossy_material[9] = glossy_material[10] =
				glossy_material[11] = Ashine->val;
				
	glossy_material[13] = 60.0 + (int)(Ashine->val * 68.0);

	if(!first){
	  lmdef(DEFMATERIAL, 1, 15, glossy_material);
	  lmdef(DEFMATERIAL, 2, 15, dull_material);
	  lmdef(DEFLIGHT, 1, 14, white_light1);
/*
fprintf(stderr,"light at %f, %f, %f\n", white_light1[5],
				white_light1[6], white_light1[7]);
fprintf(stderr,"eye at %f, %f, %f\n", FROM_TO[FROM][X], FROM_TO[FROM][Y], 
					 FROM_TO[FROM][Z]); 
*/
	}
	if(first){
	  lmdef(DEFLIGHT, 1, 14, white_light1);
	  lmdef(DEFLIGHT, 2, 14, white_light2);
	  lmdef(DEFLMODEL, 1, 5, model);
	  lmdef(DEFMATERIAL, 1, 15, glossy_material);
	  lmdef(DEFMATERIAL, 2, 15, dull_material);

	  lmbind(LIGHT1,1); 
	  lmbind(LIGHT2,2); 
	  lmbind(LMODEL,1); 

	  first = 0;
	}

	if(check_normals){
	    norm_needupdate(1);
	}
	
        if(Alight->val){
	    if(Agrid->val || Agridc->val){
		lmbind(MATERIAL, 0);
		lmcolor (LMC_COLOR);
	    }
	    else if(Asurface->val){
	        lmbind(MATERIAL,1);
	        lmcolor(LMC_NULL);
	    }
	    else{
	        lmbind(MATERIAL,2);
	        lmcolor (LMC_DIFFUSE);
	    }
	}
        else{
	    lmbind(MATERIAL, 0);
	    lmcolor (LMC_COLOR);
	}
}



/* This one uses the previous and next cells for normal calculations to 
 * produce smoother normals */
 
void
recalc_normals(xmod, ymod, elev, norm, zscale)
	int		xmod, ymod;
	int	        *elev;
	unsigned int	*norm;
	float		zscale;
{
 	long offset, noffset, slice;
	int row, col;
	float temp[3], normalizer, dz1, dz2;
	float x_res_z1, y_res_z1, x_res_z2, y_res_z2;
	float c_z1, c_z1_sq, c_z2, c_z2_sq;   /* for optimization */
	int xcnt, ycnt;
	
	xcnt = (X_Size % xmod? X_Size / xmod +1 : X_Size / xmod);
	ycnt = (Y_Size % ymod? Y_Size / ymod +1 : Y_Size / ymod);

	/* optimized */
	c_z1 = X_Res * Y_Res * xmod * ymod;
	c_z1_sq = c_z1 * c_z1;
	c_z2 = c_z1 * 2.0;
	c_z2_sq = c_z2 * c_z2;
	x_res_z1 = X_Res * zscale * xmod;
	y_res_z1 = Y_Res * zscale * ymod;
	x_res_z2 = x_res_z1 * 2.0;
	y_res_z2 = y_res_z1 * 2.0;

	slice = ymod * X_Size;


/* OPTIMIZED for constant dy & dx
 * The norm array is always the same size, but diff resolutions
 * force resampled data points to have their normals recalculated,
 * then only those norms are passed to n3f during drawing.
 * Norms are converted to a packed unsigned int for storage,
 * must be converted back at time of use.
 */
	fprintf(stderr,"\nrecalculating normals...");
	/* first row - just use single cell */
	offset = 0;
	for (col = 0; col < xcnt-1 ; col++) {
	   noffset = (offset + col*xmod);
	   dz1 = elev[noffset + xmod] - elev[noffset];
	   dz2 = elev[noffset + slice] - elev[noffset];
	   temp[0] = (float) -dz1 * y_res_z1;
	   temp[1] = (float) dz2 * x_res_z1;
	   temp[2] = c_z1;
	   normalizer = fsqrt(temp[0] * temp[0] +
			    temp[1] * temp[1] +
			    c_z1_sq); 
	   if(!normalizer) normalizer= 1.0;
	   norm[noffset] = 
	     ((unsigned int)(((temp[X]/normalizer)*XYMAXPOS)+XYMAXPOS) << 21) |
	     ((unsigned int)(((temp[Y]/normalizer)*XYMAXPOS)+XYMAXPOS) << 10) |
	     (unsigned int)((temp[Z]/normalizer)*ZMAXPOS); /* already pos   */
	}    
       noffset = (offset + col*xmod);
       dz1 = elev[noffset] - elev[noffset-xmod];
       dz2 = elev[noffset + slice] - elev[noffset];
       temp[0] = (float) -dz1 * y_res_z1;
       temp[1] = (float) dz2 * x_res_z1;
       temp[2] = c_z1;
       normalizer = fsqrt(temp[0] * temp[0] +
			temp[1] * temp[1] +
			c_z1_sq); 
       if(!normalizer) normalizer= 1.0;
	   norm[noffset] = 
	     ((unsigned int)(((temp[X]/normalizer)*XYMAXPOS)+XYMAXPOS) << 21) |
	     ((unsigned int)(((temp[Y]/normalizer)*XYMAXPOS)+XYMAXPOS) << 10) |
	     (unsigned int)((temp[Z]/normalizer)*ZMAXPOS); 

    /* now use four neighboring points for rows 1 - (n-1) */
    for (row = 1; row < ycnt-1 ; row++) {
	if(!(row%100)) fprintf(stderr,"%d ", row);
	offset = row * slice;
	/* first cell in row */
       dz1 = elev[offset + xmod] - elev[offset];
       dz2 = elev[offset + slice] - elev[offset];
       temp[0] = (float) -dz1 * y_res_z1;
       temp[1] = (float) dz2 * x_res_z1;
       temp[2] = c_z1;
       normalizer = fsqrt(temp[0] * temp[0] +
			temp[1] * temp[1] +
			c_z1_sq); 
       if(!normalizer) normalizer= 1.0;
       norm[offset] = 
	 ((unsigned int)(((temp[X]/normalizer)*XYMAXPOS)+XYMAXPOS) << 21) |
	 ((unsigned int)(((temp[Y]/normalizer)*XYMAXPOS)+XYMAXPOS) << 10) |
	 (unsigned int)((temp[Z]/normalizer)*ZMAXPOS); /* already pos   */

	for (col = 1; col < xcnt-1 ; col++) {
	   noffset = (offset + col*xmod);
	   dz1 = elev[noffset + xmod] - elev[noffset-xmod];
	   dz2 = elev[noffset + slice] - elev[noffset-slice];
	   temp[0] = (float) -dz1 * y_res_z2;
	   temp[1] = (float) dz2 * x_res_z2;
	   temp[2] = c_z2;
	   normalizer = fsqrt(temp[0] * temp[0] +
			    temp[1] * temp[1] +
			    c_z2_sq); 
	   if(!normalizer) normalizer= 1.0;
	   norm[noffset] = 
	     ((unsigned int)(((temp[X]/normalizer)*XYMAXPOS)+XYMAXPOS) << 21) |
	     ((unsigned int)(((temp[Y]/normalizer)*XYMAXPOS)+XYMAXPOS) << 10) |
	     (unsigned int)((temp[Z]/normalizer)*ZMAXPOS); /* already pos   */
	}    
	/* last column */
       noffset = (offset + col*xmod);
       dz1 = elev[noffset] - elev[noffset-xmod];
       dz2 = elev[noffset + slice] - elev[noffset-slice];
       temp[0] = (float) -dz1 * y_res_z1;
       temp[1] = (float) dz2 * x_res_z1;
       temp[2] = c_z1;
       normalizer = fsqrt(temp[0] * temp[0] +
			temp[1] * temp[1] +
			c_z1_sq); 
       if(!normalizer) normalizer= 1.0;
	   norm[noffset] = 
	     ((unsigned int)(((temp[X]/normalizer)*XYMAXPOS)+XYMAXPOS) << 21) |
	     ((unsigned int)(((temp[Y]/normalizer)*XYMAXPOS)+XYMAXPOS) << 10) |
	     (unsigned int)((temp[Z]/normalizer)*ZMAXPOS); 
    }
    /* last row */
    offset = row * slice;
    for (col = 0; col < xcnt-1 ; col++) {
       noffset = (offset + col*xmod);
       dz1 = elev[noffset + xmod] - elev[noffset];
       dz2 = elev[noffset] - elev[noffset-slice];
       temp[0] = (float) -dz1 * y_res_z1;
       temp[1] = (float) dz2 * x_res_z1;
       temp[2] = c_z1;
       normalizer = fsqrt(temp[0] * temp[0] +
			temp[1] * temp[1] +
			c_z1_sq); 
       if(!normalizer) normalizer= 1.0;
       norm[noffset] = 
	 ((unsigned int)(((temp[X]/normalizer)*XYMAXPOS)+XYMAXPOS) << 21) |
	 ((unsigned int)(((temp[Y]/normalizer)*XYMAXPOS)+XYMAXPOS) << 10) |
	 (unsigned int)((temp[Z]/normalizer)*ZMAXPOS); /* already pos   */
    }    
    /* last column */
   noffset = (offset + col*xmod);
   dz1 = elev[noffset] - elev[noffset-xmod];
   dz2 = elev[noffset] - elev[noffset-slice];
   temp[0] = (float) -dz1 * y_res_z1;
   temp[1] = (float) dz2 * x_res_z1;
   temp[2] = c_z1;
   normalizer = fsqrt(temp[0] * temp[0] +
		    temp[1] * temp[1] +
		    c_z1_sq); 
   if(!normalizer) normalizer= 1.0;
       norm[noffset] = 
	 ((unsigned int)(((temp[X]/normalizer)*XYMAXPOS)+XYMAXPOS) << 21) |
	 ((unsigned int)(((temp[Y]/normalizer)*XYMAXPOS)+XYMAXPOS) << 10) |
	 (unsigned int)((temp[Z]/normalizer)*ZMAXPOS); 
   
   fprintf(stderr,"\n");
}

/* calculate a unit sphere (radius 1) with normals to use as a model
 * for adjusting and positioning lights.  Actually, space is allocated
 * for both vertices and normals, but only normals are assigned, since
 * they will have to be scaled and translated later to produce the 
 * actual vertices in world coordinates.
*/

void
calculate_model()
{
    int i,j;
    float *p, theta, dtheta, rad;
    int arc_offset;

    if(NULL == (vertp = (float *)malloc(3*13*24*sizeof(float)))){
	fprintf(stderr,"out of memory\n");
	exit(1);
    }
    if(NULL == (normp = (float *)malloc(3*13*24*sizeof(float)))){
	fprintf(stderr,"out of memory\n");
	exit(1);
    }
    
    dtheta = (2.0 * PI)/ 24.0;

    /* semicircle */
    for(i = 0, p = normp, theta = 0.0; i <= 12; theta = dtheta * ++i){
	*p++  = fsin(theta);
	*p++  = fcos(theta);
	*p++  = 0.0;
    }
    /* sweep arc */
    for(i = 1, theta = dtheta; i < 24; theta = dtheta * ++i){
	arc_offset =  i * 13 * 3;
	for(j = 0; j <= 12; j++){
	    rad = normp[j*3 + X] > 0 ? normp[j*3 + X] : -normp[j*3 + X]; 
	    normp[arc_offset + j*3 + X] = rad * fcos(theta);
	    normp[arc_offset + j*3 + Y] = normp[j*3 + Y]; 
	    normp[arc_offset + j*3 + Z] = rad * -fsin(theta);
	}
    }
}
    
void
position_model()
{
float trans[4], scale, view_height, from[4];
float *n, *v;
int numverts, i;
   
    from[X] = FROM_TO[FROM][X];
    from[Y] = FROM_TO[FROM][Y];
    from[Z] = FROM_TO[FROM][Z];

    if(LatLon){
	from[X] = from[X]/XYscale + wind.west;
	from[Y] = from[Y]/XYscale + wind.south;
	trans[X] = from[X]; 
	trans[Y] = from[Y]; 
	trans[Z] = (Z_Mid - Zoff) * Z_exag/XYscale;

	latlon_to_sphere(from, RADIUS);
	latlon_to_sphere(trans, RADIUS);
	get_screen_pos(trans, Mod_center);
    }
    else{
        if(!los_intersect(trans, FROM_TO)){
	   if(!get_centroid(trans)){
	       trans[X] = FROM_TO[TO][X];
	       trans[Y] = FROM_TO[TO][Y];
	       trans[Z] = FROM_TO[TO][Z]; 
	    }
	}
	else{      /* set for check_under_mod rectread/rectwrite */
	   Mod_center[X] = right / 2;
	   Mod_center[Y] = top / 2;
	}
    }

    /* calculate correct scale */
    if(Aortho->val)
	view_height = Osize;
    else
	view_height = distance(from, trans) * 
			   ftan(((float)persp/20.0) * (PI/ 180.0));
    scale = view_height / 8.0;
    numverts = 13 * 24;
    for(n = normp, v = vertp, i = 0; i < numverts; i++){
	*v++ = *n++ * scale + trans[X];
	*v++ = *n++ * scale + trans[Y];
	*v++ = *n++ * scale + trans[Z];
    }
    New_view = 0;
}


void
draw_model()
{
    int i,j,arc_offset;

    do_lights(0);
    lmbind(MATERIAL,1);
    lmcolor(LMC_NULL);

    /*  actually this would be better, but doesn't work on Indigo
    zwritemask(0x0);     
    backface(1);
    */

    zwritemask(0xffffffff);   /* actually writes to zbuffer, so any vectors */
    backface(1);              /* draw afterwards will disappear where the   */
   			      /* model was.  Hopefully the indigo writemask */
			      /* bug will be fixed in 4.0.6.                */


    if(!getdisplaymode()){
	frontbuffer(TRUE);
	backbuffer(FALSE);
    }

    arc_offset =  13 * 3;
    for(i=0; i<23; i++){
	for(j=0; j<12; j++){
	    bgnpolygon();
	    if(j != 0){
		n3f(&normp[(i+1) * arc_offset + j*3]);
		v3f(&vertp[(i+1) * arc_offset + j*3]);
	    }
	    n3f(&normp[i * arc_offset + j*3]);
	    v3f(&vertp[i * arc_offset + j*3]);
	    n3f(&normp[i * arc_offset + (j+1)*3]);
	    v3f(&vertp[i * arc_offset + (j+1)*3]);
	    if(j != 11){
		n3f(&normp[(i+1) * arc_offset + (j+1)*3]);
		v3f(&vertp[(i+1) * arc_offset + (j+1)*3]);
	    }
	    endpolygon();
	}
    }
    for(j=0; j<12; j++){   /* close last arc with first */
	    bgnpolygon();
	    if(j != 0){
		n3f(&normp[j*3]);
		v3f(&vertp[j*3]);
	    }
	    n3f(&normp[i * arc_offset + j*3]);
	    v3f(&vertp[i * arc_offset + j*3]);
	    n3f(&normp[i * arc_offset + (j+1)*3]);
	    v3f(&vertp[i * arc_offset + (j+1)*3]);
	    if(j != 11){
		n3f(&normp[(j+1)*3]);
		v3f(&vertp[(j+1)*3]);
	    }
	    endpolygon();
    }

    backface(0);
    zwritemask(0xffffffff);

    if(getdisplaymode())
	swapbuffers ();
    else{
	frontbuffer(FALSE);
	backbuffer(TRUE);
    }
    Model_showing = 1;
    lmcolor(LMC_COLOR);
}


void 
check_under_model ()
{
    static int first = 1;
    static int model_dim[2], ox, oy, max_dim;
    static long *model_buf = NULL;

    if(!Model_showing){
	max_dim = right > top ? right : top;
	if(first){
	    model_dim[X] = model_dim[Y] = max_dim / 5; 
	    if(NULL == (model_buf = (long *)malloc((1 + model_dim[X]) *
				(1 + model_dim[Y]) * sizeof(long)))){
		fprintf(stderr,"out of memory\n");
		exit(1);
	    }	
	    first = 0;
	}
	/* check for recent resize */
	else if(model_dim[X] != max_dim / 5){
	    free(model_buf);
	    model_dim[X] = model_dim[Y] = max_dim / 5; 
	    if(NULL == (model_buf = (long *)malloc((1 + model_dim[X]) *
				(1 + model_dim[Y]) * sizeof(long)))){
		fprintf(stderr,"out of memory\n");
		exit(1);
	    }
	}
	if(Ashowmod->val){
	    if(New_view) position_model();
	    ox = Mod_center[X] - (model_dim[X]/2);
	    oy = Mod_center[Y] - (model_dim[Y]/2);
	    lrectread (ox, oy, ox+model_dim[X], oy+model_dim[Y], model_buf);
	    draw_model();
	}
    }
    else if(!Ashowmod->val){

#ifdef FOUR_OH
	    dither(DT_OFF);
#endif  /* FOUR_OH */

	    lrectwrite (ox, oy, ox+model_dim[X], oy+model_dim[Y], model_buf);

#ifdef FOUR_OH
	    dither(DT_ON);
#endif  /* FOUR_OH */

	    Model_showing = 0;
    }
}


    

/* This version doesn't depend on feedback mode, only requires getting
 * the current matrix. Finds approx centroid of visible area, using a 
 * "scatter & average" method of checking an arbitrary number (100) of
 * points equally scattered around the surface to see if they're visible.
 * Returns number of vertices checked that are visible (0 - 100).
*/

int
get_centroid(centroid)
float centroid[3]; 
{
    float world_center[4], new_center[4];
    float vertices[100][4], new_vert[100][4];
    float m[4][4];
    float xres, yres, xmod, ymod, clip;
    int   numhits, row, col, i, ret;

    xmod = X_Size/9;
    ymod = Y_Size/9;
    xres = X_Res * xmod;
    yres = Y_Res * ymod;
    i = 0; 
    for(row = 0;  row < 10; ++row){
	for(col = 0; col < 10; ++col){
	    vertices[i][X] = X_Min +  col * xres;
	    vertices[i][Y] = Y_Max -  row * yres;
	    vertices[i][Z] = 
(elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag;
	    vertices[i++][3] = 1.0;
	}
    }

    get_cur_matrix(m);

    P__transform (100, vertices, new_vert, m);

    centroid[X] = centroid[Y] = centroid[Z] = 0.0;
    numhits = 0;
    for(i = 0; i < 100; i++){
	clip = new_vert[i][W];
	if(new_vert[i][X] > -clip && new_vert[i][X] < clip){
	    if(new_vert[i][Y] > -clip && new_vert[i][Y] < clip)
		if(new_vert[i][Z] > -clip && new_vert[i][Z] < clip){
		    centroid[X] += vertices[i][X];
		    centroid[Y] += vertices[i][Y];
		    numhits ++;
		}		    
	}	
    }	
    if(numhits){
	centroid[X] /= numhits;
	centroid[Y] /= numhits;
	row = (Y_Max - centroid[Y]) / Y_Res;
	col = (centroid[X] - X_Min) / X_Res;
	centroid[Z] = (elev_buf[row*X_Size + col] - Zoff) * Z_exag;  

	/* now find screen location & set Model center */
	world_center[X] = centroid[X];
	world_center[Y] = centroid[Y];
	world_center[Z] = centroid[Z];
	world_center[W] = 1.0;

	get_screen_pos(world_center, Mod_center);
/*
	P__transform (1, world_center, new_center, m);

	Mod_center[X] = right - right *
		((new_center[W] - new_center[X]) / (2.0 * new_center[W]));
	Mod_center[Y] = top - top *
		((new_center[W] - new_center[Y]) / (2.0 * new_center[W]));
*/

    }	
    else{
	Mod_center[X] = right / 2;
	Mod_center[Y] = top / 2;
    }

    return(numhits);
}

get_cur_matrix(m)
float m[4][4];
{
float tm[4][4], pm[4][4];


    getmatrix(tm);
    mmode(MPROJECTION);
    getmatrix(pm);
    mmode(MVIEWING);
    pushmatrix();
    loadmatrix(pm);
    multmatrix(tm);
    getmatrix(m);
    popmatrix();
}

get_screen_pos(world, screen)
float world[4];
int screen[2];
{
float m[4][4];
float new[4];
    
    world[W] = 1.0;

    get_cur_matrix(m);
    P__transform (1, world, new, m);

    screen[X] = right - right *
	    ((new[W] - new[X]) / (2.0 * new[W]));
    screen[Y] = top - top *
	    ((new[W] - new[Y]) / (2.0 * new[W]));
}



