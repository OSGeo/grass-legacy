/*
**  Written by Bill Brown, Winter 1991 - 1992 
**  US Army Construction Engineering Research Lab
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/

#include "externs.h"
#include "math.h"
#include "device.h"
/*
#include "gis.h"
*/

extern get_centroid();
extern get_direction();
extern float distance();
extern buf_interp();
extern viewcell_interp();

void reset_fromto ();
void draw_x ();

void
keep_focus()
{
    float view_dir, incl_dir;
    
    reset_fromto();
    
/* need to figure out inclination and view direction from FROM_TO 
 * and set dials & vals for Aincl & Alook, respectively */
    
    get_direction (&view_dir, &incl_dir, FROM_TO);

    Alook->val = view_dir;
    pnl_fixact (Alook);
    Aincl->val = incl_dir;
    pnl_fixact (Aincl);

    New_view = 1;

}


void 
set_real_to()
{

    short val;
    long ret;
    float fr_to[2][4];

    fprintf(stderr,"\nLEFT MOUSE BUTTON to mark new center of view\n");
    fprintf(stderr,"ESCAPE key to cancel\n");
   
    if(!isqueued(ESCKEY)) 
	qdevice(ESCKEY);
    if(!isqueued(LEFTMOUSE)) 
	qdevice(LEFTMOUSE);
    if(!isqueued(RIGHTMOUSE)) 
	qdevice(RIGHTMOUSE);

    while(TRUE){
	
	if (qtest()){
	    switch (ret = qread (&val)) {
		case LEFTMOUSE:
		    if(val){
			if(!get_los(fr_to)) break;
			if(los_intersect(REAL_TO, fr_to)){
			    draw_x (REAL_TO, 0, MARK_SIZ);
			    fprintf(stderr,"<new center set>\n");
			    zclear();
			    New_view = 1;
			    return;
			}
			else{
			    fprintf(stderr,"selected point not on surface\n");
			    fprintf(stderr,"\nLEFT MOUSE BUTTON to mark new center of view\n");
			    fprintf(stderr,"ESCAPE key to cancel\n");
			}
		    }
		    break;
		case RIGHTMOUSE:
		case ESCKEY:
		    InFocus = 0;
		    return;
		    break;
	    }
	}
    }	    
}



int
whats_here()
{

    short val;
    long ret;
    float  fr_to[2][4], surf_pt[3];
    struct Cell_head c;

    fprintf(stderr,"\nleft mouse button : What's here? \n");
    fprintf(stderr,"right mouse button : Quit \n");

    if(!isqueued(ESCKEY)) 
	qdevice(ESCKEY);
    if(!isqueued(LEFTMOUSE)) 
	qdevice(LEFTMOUSE);
    if(!isqueued(RIGHTMOUSE)) 
	qdevice(RIGHTMOUSE);
    
    while(TRUE){

	if (qtest())
	    switch (ret = qread (&val)) {
		case LEFTMOUSE:
		    if(val){
			if(!get_los(fr_to)) break;
			if (los_intersect(surf_pt,fr_to)){
			    zbuffer(0);
			    draw_x(surf_pt, 0, MARK_SIZ);
			    zbuffer(1);
			    G_get_set_window (&c);
			    show_what(surf_pt[X]/XYscale + c.west,
			      surf_pt[Y]/XYscale + c.south,
			      surf_pt[Z]/Z_exag + Zoff);
			    fprintf(stderr,"\nleft button : What's here? \n");
			    fprintf(stderr,"right button : Quit \n");
			}
			else
			    fprintf(stderr,"selected point not on surface\n");
		    }
		    break;
		case RIGHTMOUSE:
		case ESCKEY:
		    fprintf(stderr,"<done>\n");
		    return(0);
		    break;
	    }
    }
}

int
zplane_intersect(surf_pt, fr_to, zval)
float 	surf_pt[4];
float   fr_to[2][4];
float   zval;
{
double mult, u_dx, u_dy, u_dz, normalizer;
    
    if(LatLon) return(0);

    u_dx = (fr_to[TO][X] - fr_to[FROM][X]);
    u_dy = (fr_to[TO][Y] - fr_to[FROM][Y]);
    u_dz = (fr_to[TO][Z] - fr_to[FROM][Z]);

    normalizer = sqrt(u_dx*u_dx + u_dy*u_dy + u_dz*u_dz);
    if(!normalizer) return(0);

    u_dx /= normalizer;
    u_dy /= normalizer;
    u_dz /= normalizer;

    mult = (zval - fr_to[FROM][Z])/u_dz;

    surf_pt[X] = fr_to[FROM][X] + mult*u_dx;
    surf_pt[Y] = fr_to[FROM][Y] + mult*u_dy;
    surf_pt[Z] = zval; 
    
    return(1);

}


/* slow routine good for checking if a single point is within the
   projection parallelpiped */

pt_is_visible(pt)
float pt[4];
{
float m[4][4], trans[4], clip;
    
    pt[W] = 1.0;
    get_cur_matrix(m);
    P__transform (1, pt, trans, m);

    clip = trans[W];
    return((trans[X] > -clip && trans[X] < clip) && 
	   (trans[Y] > -clip && trans[Y] < clip) && 
	   (trans[Z] > -clip && trans[Z] < clip));

}


/* crude method of intersecting line of sight with closest surface 
   uses fr_to vector to determine the point of first intersection
   which is returned in surf_pt.  */

int
los_intersect(surf_pt, fr_to) 
float 	surf_pt[4];
float   fr_to[2][4];
{
    double dx, dy, dz, u_dx, u_dy, u_dz, normalizer;
    double a[3], incr, min_incr, len;
    int   outside, above, edge, istep;
    double b[3];
    int looking_up = 0;

    
    if(LatLon) return(0);

    u_dx = (fr_to[TO][X] - fr_to[FROM][X]);
    u_dy = (fr_to[TO][Y] - fr_to[FROM][Y]);
    u_dz = (fr_to[TO][Z] - fr_to[FROM][Z]);

    normalizer = sqrt(u_dx*u_dx + u_dy*u_dy + u_dz*u_dz);
    if(!normalizer) return(0);

    u_dx /= normalizer;
    u_dy /= normalizer;
    u_dz /= normalizer;

    istep = edge = 0; 
    incr = 5.0;
    min_incr = .02;
    len = 0.0;
    dx = incr * u_dx;
    dy = incr * u_dy;
    dz = incr * u_dz;
    a[X] = fr_to[FROM][X];
    a[Y] = fr_to[FROM][Y];
    a[Z] = fr_to[FROM][Z];

    b[X] = (double)a[X];
    b[Y] = (double)a[Y];
    if(viewcell_interp(b)){
	b[Z] = (b[Z] - Zoff) * Z_exag;
	if ( a[Z] < b[Z] ){   /*  viewing from below surface  */
/*
fprintf(stderr,"view from below\n");
*/
	    return (0);      /*  don't use this method */
	    }
    }

    if(u_dz > 0.0){   /* looking up  */
	looking_up =1;
    }
    
    while (incr > min_incr){
	outside = 0;
	above = 0;
	if ( a[X] < X_Min || a[X] > X_Max || a[Y] < Y_Min || a[Y] > Y_Max ){
	    outside = 1;
	    if (istep > 3) edge = 1;
	}
        else{
	    b[X] = (double)a[X];
	    b[Y] = (double)a[Y];
	    if(viewcell_interp(b)){
		b[Z] = (b[Z] - Zoff) * Z_exag;
		if ( a[Z] > b[Z] )
		    above = 1;
	    }
	    else{ 
		outside = 1;
		if (istep > 3) edge = 1;
	    }
	}

	while (outside || above){
	    a[X] = a[X] + dx;
	    a[Y] = a[Y] + dy;
	    a[Z] = a[Z] + dz;
	    len += incr;
	    outside = 0;
	    above = 0;
	    if ( a[X] < X_Min || a[X] > X_Max || a[Y] < Y_Min || a[Y] > Y_Max ){
		outside = 1;
	    }
	    else{
		b[X] = (double)a[X];
		b[Y] = (double)a[Y];
		if(viewcell_interp(b)){
		    b[Z] = (b[Z] - Zoff) * Z_exag;
		    if ( a[Z] > b[Z] ) 
			above = 1;
		}
		else 
		    outside = 1;
	    }
	    if (a[Z] < Z_Min + incr * u_dz || len > 10000 || 
			    looking_up && a[Z] > Z_Max + incr * u_dz){
/*
fprintf(stderr,"looking over surface\n");
*/
		return 0;  /* over surface */
	    }
	}
	a[X] = a[X] - dx;
	a[Y] = a[Y] - dy;
	a[Z] = a[Z] - dz;
	incr /= 2.0;
	++ istep;
	dx = incr * u_dx;
	dy = incr * u_dy;
	dz = incr * u_dz;
    }
    if ((edge) && (b[Z] - (a[Z]+dz*2.0) > X_Res)){
/*
fprintf(stderr,"looking under surface\n");
*/
	return 0;  /* under surface */
    }

    surf_pt[X] = b[X];
    surf_pt[Y] = b[Y];
    surf_pt[Z] = b[Z];

    return (1);

}

void
reset_fromto()
{
    

    float dx, dy, dz, normalizer;

    dx = REAL_TO[X] - FROM_TO[FROM][X];
    dy = REAL_TO[Y] - FROM_TO[FROM][Y];
    dz = REAL_TO[Z] - FROM_TO[FROM][Z];

    /* normalize to 1 */
    normalizer = fsqrt(dx*dx + dy*dy + dz*dz);
    if(normalizer){
	dx =  dx/normalizer;
	dy =  dy/normalizer;
	dz =  dz/normalizer;
    }

    FROM_TO[TO][X] = FROM_TO[FROM][X] + dx;
    FROM_TO[TO][Y] = FROM_TO[FROM][Y] + dy;
    FROM_TO[TO][Z] = FROM_TO[FROM][Z] + dz;

}


void
draw_x (center,color,siz)
float center[3], siz;
unsigned long color;

{
    float pt[3], norm[3], yspread, xspread;
    int use_norm;
    float exag;
/*
    zwritemask(0x0);
*/

    linewidth(3);
    if(color) cpack(color);
    else
	cpack(~(BGcolor | 0xFF0000));  /* yellowish */

    xspread =  siz * .5;  
    yspread =  siz * .5;

    pt[Z] = center[Z] ;

    use_norm = (!LatLon && vcellnorm_interp(center, norm));
    /*
    exag = Z_exag/XYscale;
    */
    exag = 1.0;

    bgnline();

    pt[X] = center[X] - xspread;
    pt[Y] = center[Y] - yspread;
    if(use_norm){
	pt[Z] = center[Z] + exag*(norm[X]*xspread + norm[Y]*yspread); 
    }
    v_vert_func(pt);
    
    pt[X] = center[X];
    pt[Y] = center[Y];
    pt[Z] = center[Z] ;
    v_vert_func(pt);
    
    pt[X] = center[X] + xspread;
    pt[Y] = center[Y] + yspread;
    if(use_norm)
	pt[Z] = center[Z] - exag*(norm[X]*xspread + norm[Y]*yspread); 
    v_vert_func(pt);

    endline();
    bgnline();

    pt[X] = center[X] - xspread;
    pt[Y] = center[Y] + yspread;
    if(use_norm)
	pt[Z] = center[Z] + exag*(norm[X]*xspread - norm[Y]*yspread); 
    v_vert_func(pt);

    pt[X] = center[X];
    pt[Y] = center[Y];
    pt[Z] = center[Z];
    v_vert_func(pt);

    pt[X] = center[X] + xspread;
    pt[Y] = center[Y] - yspread;
    if(use_norm)
	pt[Z] = center[Z] - exag*(norm[X]*xspread - norm[Y]*yspread); 
    v_vert_func(pt);
    
    endline();
   
/*
    zwritemask(0xffffffff);
*/

    linewidth(1);
    cpack(0xFFFFFF);
}

int
get_los(vect)
float  vect[2][4];

{
    long  ox, oy, x, y, pix_w, pix_h, pix_dx, pix_dy;
    double near_w, near_h, near_dx, near_dy, pi;
    float up_v[2][4], fr_to[2][4], r_v[3];
    float dx,dy,dz, r_vmag, ft_mag;

	fr_to[FROM][X] = FROM_TO[FROM][X];
	fr_to[FROM][Y] = FROM_TO[FROM][Y];
	fr_to[FROM][Z] = FROM_TO[FROM][Z];
	fr_to[TO][X] = FROM_TO[TO][X];
	fr_to[TO][Y] = FROM_TO[TO][Y];
	fr_to[TO][Z] = FROM_TO[TO][Z];

	if(LatLon){
	    get_ll_fromto(fr_to);
	}

	pi = 4.0 * atan(1.0);

	getorigin(&ox, &oy);
	getviewport (&left, &right, &bottom, &top);
	aspect = (float) (right-left) / (top-bottom);
	ox += left;
	oy += bottom;
	pix_w = right - left + 1;
	pix_h = top - bottom + 1;
	x = getvaluator(MOUSEX) - ox;
	y = getvaluator(MOUSEY) - oy;
	if(x < 0 || x > pix_w || y < 0 || y > pix_h)
	    return(0);
	pix_dx = x - pix_w/2;
	pix_dy = y - pix_h/2;
	
	if(Aortho->val)
	    near_h = 2.0 * Osize;
	else
	    near_h = 2.0 * tan(pi * persp/3600.) * nearclip;

	near_w = aspect * near_h;
	near_dy = (double)pix_dy/pix_h * near_h;
	near_dx = (double)pix_dx/pix_w * near_w;
	
	/* calculate right vector, cross product of fr_to & up */
	dx = (fr_to[TO][X] - fr_to[FROM][X]);
	dy = (fr_to[TO][Y] - fr_to[FROM][Y]);
	dz = (fr_to[TO][Z] - fr_to[FROM][Z]);
	ft_mag = fsqrt(dx*dx + dy*dy + dz*dz);
	dx /= ft_mag;
	dy /= ft_mag;
	dz /= ft_mag;

	r_v[X] = dy * UP_VECT[Z] - UP_VECT[Y] * dz; 
	r_v[Y] = dz * UP_VECT[X] - UP_VECT[Z] * dx; 
	r_v[Z] = dx * UP_VECT[Y] - UP_VECT[X] * dy; 
	r_vmag = fsqrt(r_v[X] * r_v[X] + r_v[Y] * r_v[Y] + r_v[Z] *r_v[Z]);
	r_v[X] /= r_vmag;
	r_v[Y] /= r_vmag;
	r_v[Z] /= r_vmag;

	if(Aortho->val){
	    vect[FROM][X] = fr_to[FROM][X] + 
			    (r_v[X] * near_dx + UP_VECT[X] * near_dy);
	    vect[FROM][Y] = fr_to[FROM][Y] +
			    (r_v[Y] * near_dx + UP_VECT[Y] * near_dy);
	    vect[FROM][Z] = fr_to[FROM][Z] +
			    (r_v[Z] * near_dx + UP_VECT[Z] * near_dy);

	    vect[TO][X] = vect[FROM][X] + dx;  
	    vect[TO][Y] = vect[FROM][Y] + dy;
	    vect[TO][Z] = vect[FROM][Z] + dz; 
	}
	else{
	    vect[FROM][X] = fr_to[FROM][X];
	    vect[FROM][Y] = fr_to[FROM][Y];
	    vect[FROM][Z] = fr_to[FROM][Z];

	    vect[TO][X] = vect[FROM][X] + nearclip * dx + 
				(r_v[X] * near_dx + UP_VECT[X] * near_dy);
	    vect[TO][Y] = vect[FROM][Y] + nearclip * dy +
				(r_v[Y] * near_dx + UP_VECT[Y] * near_dy);
	    vect[TO][Z] = vect[FROM][Z] + nearclip * dz +
				(r_v[Z] * near_dx + UP_VECT[Z] * near_dy);
	}
	return(1);
}


void 
put_scale()
{
float zplane,surf_pt[4];
int ret;
   
    get_scalez(&zplane);
    ret = get_3d_pos((int)(!Aautoz->val),zplane,surf_pt, "center of scale");
    if(ret && ret < 3){
	do_scale_obj_display(surf_pt, ret-1);
	fprintf(stderr,"<scale set>\n");
    }
    else if (ret == 3){
	fprintf(stderr,"selected point not visible\n");
    }

}

static long *label_buf;
static long labx, laby, labw, labh;

save_under_label()
{
long  ox, oy;

    getorigin(&ox, &oy);
    getviewport (&left, &right, &bottom, &top);
    ox += left;
    oy += bottom;
    labx = getvaluator(MOUSEX) - ox - get_txtxoffset();
    laby = getvaluator(MOUSEY) - oy - get_txtdescender();
    labw = get_txtwidth() + get_txtxoffset();
    labh = get_txtheight();

    if(label_buf)
	free(label_buf);
    if(NULL == (label_buf = (long *)malloc((1 + labw)*
                                (1 +labh) * sizeof(long)))){
                fprintf(stderr,"out of memory\n");
                exit(1);
            }
    lrectread (labx, laby, labx+labw, laby+labh, label_buf); 

}

void 
put_label()
{
float surf_pt[4];

    if(get_3d_pos(0, 0.0,surf_pt, "first letter of label")){
	save_under_label();
	do_label_display(surf_pt);
	fprintf(stderr,"<label set>\n");
    }

}

void
undo_label()
{

	    if(!label_buf) return;

#ifdef FOUR_OH
	    dither(DT_OFF);
#endif  /* FOUR_OH */

	    frontbuffer(1);
	    lrectwrite (labx, laby, labx+labw, laby+labh, label_buf); 
	    frontbuffer(0);

#ifdef FOUR_OH
	    dither(DT_ON);
#endif  /* FOUR_OH */

}

/* returns 1 if surface pt is located, 0 if request was cancelled by
   user, 2 if surface pt found is on zplane, 3 if surface pt found
   would be clipped by near or far, so
   surf_pt returned is on same line, but within view */

get_3d_pos(use_z, zp, surf_pt, prompt)
int use_z;
float zp, surf_pt[4];
char *prompt;
{

    short val;
    long ret;
    float zplane, fr_to[2][4], dir[3], len;
    
    fprintf(stderr,"\nLEFT MOUSE BUTTON to mark %s\n", prompt);
    fprintf(stderr,"ESCAPE key to cancel\n");
   
    if(!isqueued(ESCKEY)) 
	qdevice(ESCKEY);
    if(!isqueued(LEFTMOUSE)) 
	qdevice(LEFTMOUSE);
    if(!isqueued(RIGHTMOUSE)) 
	qdevice(RIGHTMOUSE);

    if(use_z)
	zplane = zp;
    else if(Anozero->val)
	zplane = (Z_Min_notzero - Zoff) *Z_exag;
    else
	zplane = Z_Min;

    while(TRUE){
	
	if (qtest()){
	    switch (ret = qread (&val)) {
		case LEFTMOUSE:
		    if(val){
			if(!get_los(fr_to)) break;
			if(use_z){
			    if(zplane_intersect(surf_pt, fr_to, zplane))
				if(pt_is_visible(surf_pt))
				    return(2);
			}
			else if(los_intersect(surf_pt, fr_to)){
			    return(1);
			}
			else if(zplane_intersect(surf_pt, fr_to, zplane)){
			    if(pt_is_visible(surf_pt))
				return(2);
			}
			get_vis_pt_on_los(surf_pt, fr_to);
			return(3);
		    }
		    break;
		case RIGHTMOUSE:
		case ESCKEY:
		    return(0);
		    break;
	    }
	}
    }	    
}

get_vis_pt_on_los(pt, los)
float pt[4], los[2][4];
{
float dir[3], len, cent[3];

    get_norm_direction(dir, los);
    get_centroid(cent);
    len = distance(los[FROM], cent);
    pt[X] = los[FROM][X] + dir[X] * len;
    pt[Y] = los[FROM][Y] + dir[Y] * len;
    pt[Z] = los[FROM][Z] + dir[Z] * len;

}




