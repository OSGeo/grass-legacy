/* tinkering with functions to intersect line segments with edges of
surface polygons */

/*
Do intersections in 2D, fill line structure with 3D pts (maybe calling
routine will cache for redrawing).  Get Z value by using linear interp 
between corners.

1) check for easy cases:
    a) colinear with horizontal or vertical edges
    b) colinear with diagonal edges of triangles
2) calculate three arrays of ordered intersections:
    a) with vertical edges
    b) with horizontal edges
    c) with diagonal edges
   and interpolate Z, using simple linear interpolation.
3) eliminate duplicate intersections (need only compare one coord for each)
4) build ordered set of points.

Return static pointer to 3D set of points.  Max number of intersections
will be rows + cols + diags, so should allocate this number to initialize.
Let calling routine worry about copying points for caching.

*/

#include "externs.h"
#include "rowcol.h"
#include "math.h"
#include <stdlib.h>
#include <stdio.h>

#define	DONT_INTERSECT    0
#define	DO_INTERSECT      1
#define COLLINEAR         2

#define LERP(a,l,h)      ((l)+(((h)-(l))*(a)))
#define EQUAL(a,b)       (fabs((a)-(b))<EPSILON)
#define ISNODE(p,res)    (fmod((double)p,(double)res)<EPSILON)
#define SET_ELEV(off, z) (z = elev_buf[off])

static Point3 *I3d;
/* array of points to be returned */

static double EPSILON=0.000001;  
/* make dependent on xres or yres */

static Point3 *Vi, *Hi, *Di; 
/*vertical, horizontal, & diagonal intersections*/

static int Flat;

double dist_squared_2d();
Point3 *_drape_get_segments();
double P2distance ();

static int
drape_line_init(rows, cols)
int rows, cols;
{
static int first=1;

    if(first){
	first = 0;

	if(NULL == (I3d = (Point3 *)calloc(2*(rows+cols), sizeof(Point3))))
	    return (-1);  /* exit? */

	if(NULL == (Vi = (Point3 *)calloc(cols, sizeof(Point3)))){
	    free(I3d);
	    return (-1);  /* exit? */
	}

	if(NULL == (Hi = (Point3 *)calloc(rows, sizeof(Point3)))){
	    free(I3d);
	    free(Vi);
	    return (-1);  /* exit? */
	}

	if(NULL == (Di = (Point3 *)calloc(rows+cols, sizeof(Point3)))){
	    free(I3d);
	    free(Vi);
	    free(Hi);
	    return (-1);  /* exit? */
	}

    }

    return(1);

}


seg_intersect_vregion(bgn, end)
double bgn[2], end[2];
{
double *replace, xl, yb, xr, yt, xi, yi;
int ret=0, inside=0;

/*
    xl = 0.0;
    xr = VCOL2X(VCOLS);
    yt = VROW2Y(0);
    yb = VROW2Y(VROWS);
*/
    xl = VXMIN;
    xr = VXMAX;
    yt = VYMAX;
    yb = VYMIN;

    if(in_vregion(bgn)){
	replace = end;
	inside++;
    }
    if(in_vregion(end)){
	replace = bgn;
	inside++;
    }
    if(inside == 2) return(1);
    else if(inside){  /* one in & one out - replace gets first intersection */
	if(segs_intersect(bgn[X],bgn[Y],end[X],end[Y],xl,yb,xl,yt, &xi, &yi))
	{ }
	else if(segs_intersect(bgn[X],bgn[Y],end[X],end[Y],xr,yb,xr,yt,&xi,&yi))
	{ }
	else if(segs_intersect(bgn[X],bgn[Y],end[X],end[Y],xl,yb,xr,yb,&xi,&yi))
	{ }
	else if(segs_intersect(bgn[X],bgn[Y],end[X],end[Y],xl,yt,xr,yt,&xi,&yi))
	{ }
	replace[X] = xi;
	replace[Y] = yi;
    }
    else{  /* both out - find 2 intersects & replace both */
    double pt1[2], pt2[2];
	
	replace = pt1;
	if(segs_intersect(bgn[X],bgn[Y],end[X],end[Y],xl,yb,xl,yt, &xi, &yi))
	{ 
	    replace[X] = xi;
	    replace[Y] = yi;
	    replace = pt2;
	    inside++; 
	}
	if(segs_intersect(bgn[X],bgn[Y],end[X],end[Y],xr,yb,xr,yt,&xi,&yi))
	{ 
	    replace[X] = xi;
	    replace[Y] = yi;
	    replace = pt2;
	    inside++; 
	}
	if(inside < 2){
	    if(segs_intersect(bgn[X],bgn[Y],end[X],end[Y],xl,yb,xr,yb,&xi,&yi))
	    { 
		replace[X] = xi;
		replace[Y] = yi;
		replace = pt2;
		inside++; 
	    }
	}
	if(inside < 2){
	    if(segs_intersect(bgn[X],bgn[Y],end[X],end[Y],xl,yt,xr,yt,&xi,&yi))
	    { 
		replace[X] = xi;
		replace[Y] = yi;
		inside++; 
	    }
	}
	if(inside < 2) return(0);  /* no intersect or only 1 point on corner */

	/* compare dist of intersects to bgn - closest replaces bgn */
	if(P2distance (bgn, pt1) < P2distance (bgn, pt2)){
	    bgn[X] = pt1[X];
	    bgn[Y] = pt1[Y];
	    end[X] = pt2[X];
	    end[Y] = pt2[Y];
	}
	else{
	    bgn[X] = pt2[X];
	    bgn[Y] = pt2[Y];
	    end[X] = pt1[X];
	    end[Y] = pt1[Y];
	}

    }

    return(1);

}

Point3 *
drape_get_segments(bgn, end, num)
double *bgn, *end;
int *num;
{
    if(0 > drape_line_init(Y_Size, X_Size)){
	fprintf(stderr,"Unable to process vector - out of memory!\n");
	*num = 0;
	return(NULL);
    }

    if(!seg_intersect_vregion(bgn, end)){
	*num = 0;
	return(NULL);
    }

    if(Aflat->val){
    /* will probably want a force_drape option to get all intersects */
	I3d[0][X] = bgn[X];
	I3d[0][Y] = bgn[Y];
	I3d[0][Z] = Vect_z;
	I3d[1][X] = end[X];
	I3d[1][Y] = end[Y];
	I3d[1][Z] = Vect_z;
	*num = 2;
	return(I3d);
    }
    
    Flat = 0;
    return(_drape_get_segments(bgn, end, num));
}


Point3 *
drape_get_allsegments(bgn, end, num)
double *bgn, *end;
int *num;
{
    if(0 > drape_line_init(Y_Size, X_Size)){
	fprintf(stderr,"Unable to process vector - out of memory!\n");
	return(-1);
    }

    if(!seg_intersect_vregion(bgn, end)){
	*num = 0;
	return(NULL);
    }

    if(Aflat->val){
	Flat = 1;
    }
    else
	Flat = 0;


    return(_drape_get_segments(bgn, end, num));
}


Point3 *
_drape_get_segments(bgn, end, num)
double *bgn, *end;
int *num;
{
    double f[3], l[3];
    int vi, hi, di;
    double dir[2], yres, xres;


	xres = VXRES;
	yres = VYRES;

	vi = hi = di = 0;
	v2dir(bgn, end, dir);
/*
fprintf(stderr,"get_segs: %.2f,%.2f -> %.2f,%.2f\n",
bgn[0],bgn[1],end[0],end[1]);
*/
	
	if(dir[X])
	    vi = get_vert_intersects(bgn, end, dir);
/*
fprintf(stderr,"vi = %d, ", vi);
*/

	if(dir[Y])
	    hi = get_horz_intersects(bgn, end, dir);
/*
fprintf(stderr,"hi = %d, ", hi);
*/

	if(!((end[Y]-bgn[Y])/(end[X]-bgn[X]) == yres/xres))
	    di = get_diag_intersects(bgn, end, dir);
/*
fprintf(stderr,"di = %d\n", di);
*/

	interp_first_last(bgn, end, f, l);
/*
fprintf(stderr,"first last = %.2f %.2f %.2f -> %.2f %.2f %.2f\n", 
f[0],f[1],f[2],l[0],l[1],l[2]);
*/

	*num = order_intersects(f, l, vi, hi, di);
	/* fills in return values, eliminates dupes (corners) */

    return(I3d);

}


double
dist_squared_2d(p1, p2)
double *p1, *p2;
{
double dx, dy;
    
    dx = p2[X] - p1[X];
    dy = p2[Y] - p1[Y];

    return(dx * dx + dy * dy);

}

interp_first_last(bgn, end, f, l)
double bgn[2], end[2];
Point3 f, l;
{
    f[X] = bgn[X];
    f[Y] = bgn[Y];
    viewcell_tri_interp(f, 0);

    l[X] = end[X];
    l[Y] = end[Y];
    viewcell_tri_interp(l, 0);

}



/* In gsd_surf, tmesh draws polys like so:
   --------------
   |           /|
   |          / |          
   |         /  |         
   |        /   |        
   |       /    |       
   |      /     |      
   |     /      |     
   |    /       |    
   |   /        |   
   |  /         |  
   | /          | 
   |/           |
   --------------
   UNLESS the top right or bottom left point is masked, in which case a
   single triangle with the opposite diagonal is drawn.  This case is
   not yet handled here & should only occur on edges. 
   pt has X & Y coordinates in it, we interpolate Z here & return:
   1 if point is in view region, otherwise 0.
   This could probably be much shorter, but not much faster.
*/
viewcell_tri_interp(pt, check_mask)
Point3 pt;
int check_mask;
{
Point3 p1, p2, p3, tmp;
int offset, drow, dcol, vrow, vcol;
double xmax, ymin, ymax, alpha;
double scalx, scaly, scalz;

   
    xmax = VXMAX; 
    ymax = VYMAX; 
    ymin = VYMIN;
   
    if(check_mask){
	if(_point_is_masked(pt)){ 
	    return (0); 
	}
    }

    if(pt[X] < 0.0 || pt[Y] > ymax){ /* outside on left or top */
	return (0); 
    }
    if(pt[Y] < ymin || pt[X] > xmax){ /* outside on bottom or right */ 
	return (0); 
    }

    vrow = Y2VROW(pt[Y]);
    vcol = X2VCOL(pt[X]);
    
    if (vrow < VROWS && vcol < VCOLS){ /*not on bottom or right edge*/
	if(pt[X] > 0.0 && pt[Y] < ymax){ /* not on left or top edge */
	    p1[X] = VCOL2X(vcol+1);
	    p1[Y] = VROW2Y(vrow);
	    drow = VROW2DROW(vrow); 
	    dcol = VCOL2DCOL(vcol+1); 
	    offset = DRC2OFF(drow, dcol); 
	    SET_ELEV(offset, p1[Z]);    /* top right */

	    p2[X] = VCOL2X(vcol);
	    p2[Y] = VROW2Y(vrow+1);
	    drow = VROW2DROW(vrow+1);
	    dcol = VCOL2DCOL(vcol); 
	    offset = DRC2OFF(drow, dcol); 
	    SET_ELEV(offset, p2[Z]);    /* bottom left */

	    if((pt[X] - p2[X])/VXRES > (pt[Y] - p2[Y])/VYRES){ 
	    /* lower triangle */
		p3[X] = VCOL2X(vcol+1);
		p3[Y] = VROW2Y(vrow+1);
		drow = VROW2DROW(vrow+1); 
		dcol = VCOL2DCOL(vcol+1); 
		offset = DRC2OFF(drow, dcol); 
		SET_ELEV(offset, p3[Z]);    /* bottom right */
	    }
	    else{
	    /* upper triangle */
		p3[X] = VCOL2X(vcol);
		p3[Y] = VROW2Y(vrow);
		drow = VROW2DROW(vrow); 
		dcol = VCOL2DCOL(vcol); 
		offset = DRC2OFF(drow, dcol); 
		SET_ELEV(offset, p3[Z]);    /* top left */
	    }
	    return(Point_on_plane(p1, p2, p3, pt));
	}
	else if (pt[X] == 0.0){  /* on left edge */
	    if(pt[Y] < ymax){
		vrow = Y2VROW(pt[Y]);
		drow = VROW2DROW(vrow);
		offset = DRC2OFF(drow, 0); 
		SET_ELEV(offset, p1[Z]);

		drow = VROW2DROW(vrow+1);
		offset = DRC2OFF(drow, 0); 
		SET_ELEV(offset, p2[Z]);

		alpha = (pt[Y] - VROW2Y(vrow))/VYRES;
		pt[Z] = LERP(alpha, p1[Z], p2[Z]);
	    }
	    else                             /* top left corner */
		SET_ELEV(0, pt[Z]);
	    return(1);
	}
	else if (pt[Y] == Y_Max){  /* on top edge, not a corner */
	    vcol = X2VCOL(pt[X]);
	    dcol = VCOL2DCOL(vcol);
	    SET_ELEV(dcol, p1[Z]);

	    dcol = VCOL2DCOL(vcol+1);
	    SET_ELEV(dcol, p2[Z]);

	    alpha = (pt[X] - VCOL2X(vcol))/VXRES;
	    pt[Z] = LERP(alpha, p1[Z], p2[Z]);
	    return(1);
	}
    }
    else if(vrow == VROWS){  /* on bottom edge */
	drow = VROW2DROW(VROWS);
	if (pt[X] > 0.0 && pt[X] < xmax){  /* not a corner */
	    vcol = X2VCOL(pt[X]);
	    dcol = VCOL2DCOL(vcol);
	    offset = DRC2OFF(drow, dcol); 
	    SET_ELEV(offset, p1[Z]);

	    dcol = VCOL2DCOL(vcol+1);
	    offset = DRC2OFF(drow, dcol); 
	    SET_ELEV(dcol, p2[Z]);

	    alpha = (pt[X] - VCOL2X(vcol))/VXRES;
	    pt[Z] = LERP(alpha, p1[Z], p2[Z]);
	    return(1);
	}
	else if(pt[X] == 0.0){          /* bottom left corner */
	    offset = DRC2OFF(drow, 0); 
	    SET_ELEV(offset, pt[Z]);
	    return(1);
	}
	else {                          /* bottom right corner */
	    dcol = VCOL2DCOL(VCOLS);
	    offset = DRC2OFF(drow, dcol); 
	    SET_ELEV(offset, pt[Z]);
	    return(1);
	}
    }
    else{                   /* on right edge, not bottom corner */
	dcol = VCOL2DCOL(VCOLS);
	if(pt[Y] < ymax){
	    vrow = Y2VROW(pt[Y]);
	    drow = VROW2DROW(vrow);
	    offset = DRC2OFF(drow, dcol); 
	    SET_ELEV(offset, p1[Z]);

	    drow = VROW2DROW(vrow+1);
	    offset = DRC2OFF(drow, dcol); 
	    SET_ELEV(offset, p2[Z]);

	    alpha = (pt[Y] - VROW2Y(vrow))/VYRES;
	    pt[Z] = LERP(alpha, p1[Z], p2[Z]);
	    return(1);
	}
	else{                          /* top right corner */
	    SET_ELEV(dcol, pt[Z]);
	    return(1);
	}
    }

}

in_vregion(pt)
double pt[2];
{

    return (pt[X] >= VXMIN && pt[Y] <= VYMAX &&
	    pt[X] <= VXMAX && pt[Y] >= VYMIN);

}

order_intersects(first, last, vi, hi, di)
Point3 first, last;
int vi, hi, di;
{
int num, i, found, cv, ch, cd, cnum;
double dv, dh, dd, big, cpoint[2];
    
    cv = ch = cd = cnum = 0;
    num = vi + hi + di;

    cpoint[X] = first[X];
    cpoint[Y] = first[Y];
    if(in_vregion(first)){
	I3d[cnum][X] = first[X];
	I3d[cnum][Y] = first[Y];
	I3d[cnum][Z] = first[Z] ;
	cnum++;
    }
/*
else
fprintf(stderr,"first point not in vregion\n");
*/
    
    /* TODO: big could still be less than first dist */
    big = Y_Max*Y_Max + X_Max*X_Max; /*BIG distance*/
    dv=dh=dd = big; 
    for(i=0; i < num; i=cv+ch+cd){   
	if(cv < vi){
	    dv = dist_squared_2d(Vi[cv], cpoint);
	    if(dv<EPSILON){
		cv++;
		continue;
	    }
	}
	else
	    dv = big;

	if(ch < hi){
	    dh = dist_squared_2d(Hi[ch], cpoint);
	    if(dh<EPSILON){
		ch++;
		continue;
	    }
	}
	else
	    dh = big;

	if(cd < di){
	    dd = dist_squared_2d(Di[cd], cpoint);
	    if(dd<EPSILON){
		cd++;
		continue;
	    }
	}
	else
	    dd = big;

	found = 0;
	if(cd < di){
	    if(dd <= dv && dd <= dh){
		found = 1;
		cpoint[X] = I3d[cnum][X] = Di[cd][X];
		cpoint[Y] = I3d[cnum][Y] = Di[cd][Y];
		I3d[cnum][Z] = Di[cd][Z] ;
		cnum++;
		if(EQUAL(dd, dv)) cv++;
		if(EQUAL(dd, dh)) ch++;
		cd++;
	    }
	}
	if(!found){
	    if(cv < vi){
		if (dv <= dh){
		    found = 1;
		    cpoint[X] = I3d[cnum][X] = Vi[cv][X];
		    cpoint[Y] = I3d[cnum][Y] = Vi[cv][Y];
		    I3d[cnum][Z] = Vi[cv][Z] ;
		    cnum++;
		    if(EQUAL(dv, dh)) ch++;
		    cv++;
		}
	    }
	}
	if(!found){
	    if(ch < hi){
		cpoint[X] = I3d[cnum][X] = Hi[ch][X];
		cpoint[Y] = I3d[cnum][Y] = Hi[ch][Y];
		I3d[cnum][Z] = Hi[ch][Z] ;
		cnum++;
		ch++;
	    }
	}
	if(i == cv+ch+cd){
	    fprintf(stderr,"stuck on %d\n", cnum);
	    fprintf(stderr,"cv = %d, ch = %d, cd = %d\n", cv, ch, cd);
	    fprintf(stderr,"dv = %f, dh = %f, dd = %f\n", dv, dh, dd);
	    break;
	}
    }
/*
    fprintf(stderr,"cv = %d, ch = %d, cd = %d\n", cv, ch, cd);
*/
    if(EQUAL(last[X], cpoint[X]) && EQUAL(last[Y], cpoint[Y]))
	return(cnum);

    if(in_vregion(last)){
	/* TODO: check for last point on corner ? */
	I3d[cnum][X] = last[X];
	I3d[cnum][Y] = last[Y];
	I3d[cnum][Z] = last[Z] ;
	++cnum;
    }
/*
else
fprintf(stderr,"last point not in vregion\n");
*/

    return(cnum);

}



/* TODO: for consistancy, need to decide how last row & last column are
   displayed - would it look funny to always draw last row/col with
   finer resolution if necessary, or would it be better to only show
   full rows/cols? */
/* colinear already eliminated */
static int
get_vert_intersects(bgn, end, dir)
double bgn[2], end[2], dir[2];
{
    int fcol, lcol, incr, hits, num, offset, drow1, drow2;
    double xl, yb, xr, yt, z1, z2, alpha;
    double xres, yres, xi, yi;
    int bgncol, endcol, cols, rows;

    xres = VXRES;
    yres = VYRES;
    cols = VCOLS;
    rows = VROWS;
	
	bgncol = X2VCOL(bgn[X]);
	endcol = X2VCOL(end[X]);
	if(bgncol > cols && endcol > cols) return 0;
	if(bgncol == endcol) return 0;

	fcol = dir[X] > 0? bgncol + 1: 
			   bgncol;
	lcol = dir[X] > 0? endcol: 
			   endcol + 1;
	if(fcol == lcol) return 0;

	/* assuming only showing FULL cols */
	incr = lcol - fcol > 0? 1: -1;
	while(fcol > cols || fcol < 0){
	    fcol += incr;
	}
	while(lcol > cols || lcol < 0){
	    lcol -= incr;
	}
	num = abs(lcol - fcol) + 1;
/*
fprintf(stderr,"firstcol = %d, lastcol = %d, incr = %d\n", fcol, lcol, incr);
*/

	yb = Y_Max - (yres*rows) - EPSILON;
	yt = Y_Max + EPSILON;
	for (hits=0; hits < num; hits++){
	    xl = xr = VCOL2X(fcol); 
	    if(segs_intersect(bgn[X], bgn[Y], end[X], end[Y], xl, yt, xr, yb,
			&xi, &yi)){
		Vi[hits][X] = xi;
		Vi[hits][Y] = yi;
		/* find data rows */
		if(Flat)
		    Vi[hits][Z] = Vect_z;
		else{
		    drow1 = Y2VROW(Vi[hits][Y])*Y_Modr;
		    drow2 = (1 + Y2VROW(Vi[hits][Y]))*Y_Modr;
		    if(drow2 >= Y_Size) drow2 = Y_Size - 1; /*bottom edge*/

		    alpha = ((Y_Max - drow1*Y_Res)- Vi[hits][Y])/yres;

		    offset = DRC2OFF(drow1, fcol * X_Modr); 
		    SET_ELEV(offset, z1);
		    offset = DRC2OFF(drow2, fcol * X_Modr); 
		    SET_ELEV(offset, z2);
		    Vi[hits][Z] = LERP(alpha, z1, z2);
		}
/*
fprintf(stderr,"alpha: %f VZ1: %f VZ2: %f LERP: %f\n",alpha,z1,z2,Vi[hits][Z]);
*/
	    }
	    /* if they don't intersect, something's wrong! */
	    /* should only happen on endpoint, so it will be added later */
	    else{
		hits--;
		num--;
	    }
	    fcol += incr;
	}

	return(hits);
}


static int
get_horz_intersects(bgn, end, dir)
double bgn[2], end[2], dir[2];
{
    int frow, lrow, incr, hits, num, offset, dcol1, dcol2;
    double xl, yb, xr, yt, z1, z2, alpha;
    double xres, yres, xi, yi;
    int bgnrow, endrow, rows, cols;

    xres = VXRES;
    yres = VYRES;
    cols = VCOLS;
    rows = VROWS;
	
	bgnrow = Y2VROW(bgn[Y]);
	endrow = Y2VROW(end[Y]);
	if(bgnrow == endrow) return 0;
	if(bgnrow > rows && endrow > rows) return 0;

	frow = dir[Y] > 0? bgnrow: 
			   bgnrow + 1;
	lrow = dir[Y] > 0? endrow + 1: 
			   endrow;
	if(frow == lrow) return 0;

	/* assuming only showing FULL rows */
	incr = lrow - frow > 0? 1: -1;
	while(frow > rows || frow < 0) frow += incr;
	while(lrow > rows || lrow < 0) lrow -= incr;
	num = abs(lrow - frow) + 1;
/*
fprintf(stderr,"firstrow = %d, lastrow = %d, incr = %d\n", frow, lrow, incr);
*/

	xl = 0.0 - EPSILON;
	xr = xres*cols + EPSILON; 
	for (hits=0; hits < num; hits++){
	    yb = yt = VROW2Y(frow);
	    if(segs_intersect(bgn[X], bgn[Y], end[X], end[Y], xl, yt, xr, yb,
			&xi, &yi)){
		Hi[hits][X] = xi;
		Hi[hits][Y] = yi;

		/* find data cols */
		if(Flat)
		    Hi[hits][Z] = Vect_z;
		else{
		    dcol1 = X2VCOL(Hi[hits][X])*X_Modr;
		    dcol2 = (1 + X2VCOL(Hi[hits][X]))*X_Modr;
		    if(dcol2 >= X_Size) dcol2 = X_Size - 1; /* right edge */

		    alpha = (Hi[hits][X] - (dcol1*X_Res)) / xres;
		    
		    offset = DRC2OFF(frow * Y_Modr, dcol1);
		    SET_ELEV(offset, z1);
		    offset = DRC2OFF(frow * Y_Modr, dcol2);
		    SET_ELEV(offset, z2);
		    Hi[hits][Z] = LERP(alpha, z1, z2);
		}
/*
fprintf(stderr,"alpha: %f HZ1: %f HZ2: %f LERP: %f\n",alpha,z1,z2,Hi[hits][Z]);
*/
	    }
	    /* if they don't intersect, something's wrong! */
	    /* should only happen on endpoint, so it will be added later */
	    else{
/*
fprintf(stderr,"NIH(%d) ", frow);
fprintf(stderr," <%f,%f><%f,%f>",bgn[X],bgn[Y],end[X],end[Y]);
fprintf(stderr," <%f,%f><%f,%f>\n",xl, yt, xr, yb);
*/
		hits--;
		num--;
	    }
	    frow += incr;
	}

	return(hits);

}


/* colinear already eliminated */
static int
get_diag_intersects(bgn, end, dir)
double bgn[2], end[2], dir[2];
{
    int fdig, ldig, incr, hits, num, offset; 
    int vrow, vcol, drow1, drow2, dcol1, dcol2;
    double xl, yb, xr, yt, z1, z2, alpha;
    double xres, yres, xi, yi, dx, dy;
    int diags, cols, rows, lower;
    Point3 pt;

    xres = VXRES;
    yres = VYRES;
    cols = VCOLS;
    rows = VROWS;
    diags = rows + cols; /* -1 ? */

	/* determine upper/lower triangle for last */
	vrow = Y2VROW(end[Y]);
	vcol = X2VCOL(end[X]);
	pt[X] = VCOL2X(vcol);
	pt[Y] = VROW2Y(vrow+1);
	lower = ((end[X] - pt[X])/xres > (end[Y] - pt[Y])/yres);
	ldig = lower? vrow + vcol + 1: vrow + vcol;

	/* determine upper/lower triangle for first */
	vrow = Y2VROW(bgn[Y]);
	vcol = X2VCOL(bgn[X]);
	pt[X] = VCOL2X(vcol);
	pt[Y] = VROW2Y(vrow+1);
	lower = ((bgn[X] - pt[X])/xres > (bgn[Y] - pt[Y])/yres);
	fdig = lower? vrow + vcol + 1: vrow + vcol;

	/* adjust according to direction */
	if(ldig > fdig) fdig++;
	if(fdig > ldig) ldig++;
	if(fdig == ldig) return(0);

	incr = ldig - fdig > 0? 1: -1;

	while(fdig > diags || fdig < 0){
	    fdig += incr;
	}
	while(ldig > diags || ldig < 0){
	    ldig -= incr;
	}
	num = abs(ldig - fdig) + 1;
/*
fprintf(stderr,"firstdig = %d, lastdig = %d, incr = %d\n", fdig, ldig, incr);
*/

	for (hits=0; hits < num; hits++){
	    yb = Y_Max - (yres*(fdig < rows? fdig: rows)) - EPSILON;
	    xl = VCOL2X((fdig < rows? 0: fdig-rows)) - EPSILON; 
	    yt = Y_Max - (yres*(fdig < cols? 0 : fdig-cols)) + EPSILON;
	    xr = VCOL2X((fdig < cols? fdig: cols)) + EPSILON; 
	    if(segs_intersect(bgn[X], bgn[Y], end[X], end[Y], xl, yb, xr, yt,
			&xi, &yi)){
		Di[hits][X] = xi;
		Di[hits][Y] = yi;

		if(ISNODE(xi, xres)){ /* then it's also a ynode */
		    num--;
		    hits--;
		    continue;
		}

		/* find data rows */
		drow1 = Y2VROW(Di[hits][Y])*Y_Modr;
		drow2 = (1 + Y2VROW(Di[hits][Y]))*Y_Modr;
		if(drow2 >= Y_Size) drow2 = Y_Size - 1; /* bottom edge */

		/* find data cols */
		if(Flat)
		    Di[hits][Z] = Vect_z;
		else{
		    dcol1 = X2VCOL(Di[hits][X])*X_Modr;
		    dcol2 = (1 + X2VCOL(Di[hits][X]))*X_Modr;
		    if(dcol2 >= X_Size) dcol2 = X_Size - 1; /* right edge */
		    
		    dx = DCOL2X(dcol2) - Di[hits][X]; 
		    dy = DROW2Y(drow1) - Di[hits][Y];
		    alpha = sqrt(dx*dx + dy*dy) / sqrt(xres*xres + yres*yres);

		    offset = DRC2OFF(drow1, dcol2); 
		    SET_ELEV(offset, z1);
		    offset = DRC2OFF(drow2, dcol1); 
		    SET_ELEV(offset, z2);
		    Di[hits][Z] = LERP(alpha, z1, z2);
		}
/*
fprintf(stderr,"alpha: %f DZ1: %f DZ2: %f LERP: %f\n",alpha,z1,z2,Di[hits][Z]);
*/
	    }
	    /* if they don't intersect, something's wrong! */
	    /* should only happen on endpoint, so it will be added later */
	    else{
		hits--;
		num--;
	    }
	    fdig += incr;
	}

	return(hits);
}



/* lines_intersect:  AUTHOR: Mukesh Prasad
 *
 *   This function computes whether two line segments,
 *   respectively joining the input points (x1,y1) -- (x2,y2)
 *   and the input points (x3,y3) -- (x4,y4) intersect.
 *   If the lines intersect, the output variables x, y are
 *   set to coordinates of the point of intersection.
 *
 *   Entry
 *        x1, y1,  x2, y2   Coordinates of endpoints of one segment.
 *        x3, y3,  x4, y4   Coordinates of endpoints of other segment.
 *
 *   Exit
 *        x, y              Coordinates of intersection point.
 *
 *   The value returned by the function is one of:
 *
 *        DONT_INTERSECT    0
 *        DO_INTERSECT      1
 *        COLLINEAR         2
 *
 */

/**************************************************************
 *                                                            *
 *    NOTE:  The following macro to determine if two numbers  *
 *    have the same sign, is for 2's complement number        *
 *    representation.  It will need to be modified for other  *
 *    number systems.                                         *
 *                                                            *
 **************************************************************/
/* for INTS only
#define SAME_SIGNS( a, b )	\
		(((long) ((unsigned long) a ^ (unsigned long) b)) >= 0 )
*/

#define SAME_SIGNS( a, b )	\
		((a >= 0 && b >= 0) || (a < 0 && b < 0))

int segs_intersect( x1, y1, x2, y2, x3, y3, x4, y4, x, y)
double x1, y1, x2, y2, x3, y3, x4, y4, *x, *y;
{
    double a1, a2, b1, b2, c1, c2; /* Coefficients of line eqns. */
    double r1, r2, r3, r4;         /* 'Sign' values */
    double denom, offset, num;     /* Intermediate values */

    /* Compute a1, b1, c1, where line joining points 1 and 2
     * is "a1 x  +  b1 y  +  c1  =  0".
     */
    a1 = y2 - y1;
    b1 = x1 - x2;
    c1 = x2 * y1 - x1 * y2;

    /* Compute r3 and r4.
     */
    r3 = a1 * x3 + b1 * y3 + c1;
    r4 = a1 * x4 + b1 * y4 + c1;

    /* Check signs of r3 and r4.  If both point 3 and point 4 lie on
     * same side of line 1, the line segments do not intersect.
     */

    if ( !EQUAL(r3, 0.0) &&
         !EQUAL(r4, 0.0) &&
         SAME_SIGNS( r3, r4 ))
        return ( DONT_INTERSECT );

    /* Compute a2, b2, c2 */
    a2 = y4 - y3;
    b2 = x3 - x4;
    c2 = x4 * y3 - x3 * y4;

    /* Compute r1 and r2 */
    r1 = a2 * x1 + b2 * y1 + c2;
    r2 = a2 * x2 + b2 * y2 + c2;

    /* Check signs of r1 and r2.  If both point 1 and point 2 lie
     * on same side of second line segment, the line segments do
     * not intersect.
     */

    if ( !EQUAL(r1, 0.0) &&
         !EQUAL(r2, 0.0) &&
         SAME_SIGNS( r1, r2 ))
        return ( DONT_INTERSECT );

    /* Line segments intersect: compute intersection point. 
     */
    denom = a1 * b2 - a2 * b1;
    if ( denom == 0 )
        return ( COLLINEAR );
    offset = denom < 0 ? - denom / 2 : denom / 2;

    /* The denom/2 is to get rounding instead of truncating.  It
     * is added or subtracted to the numerator, depending upon the
     * sign of the numerator.
     */
    num = b1 * c2 - b2 * c1;
    /*
    *x = ( num < 0 ? num - offset : num + offset ) / denom;
    */
    *x = num / denom;

    num = a2 * c1 - a1 * c2;
    /*
    *y = ( num < 0 ? num - offset : num + offset ) / denom;
    */
    *y = num / denom;

    return ( DO_INTERSECT );
}


/* Plane defined by three points here; user fills in unk[X] & unk[Y] */
Point_on_plane(p1, p2, p3, unk)
Point3 p1, p2, p3, unk;
{
double plane[4];

   P3toPlane(p1, p2, p3, plane); 
   return(XY_intersect_plane(unk, plane));
}


/* Ax + By + Cz + D = 0, so z = (Ax + By + D) / -C */
/* user fills in intersect[X] & intersect[Y] */
XY_intersect_plane(intersect, plane)
double intersect[3], plane[4];
{
double x, y;

    if(!plane[Z]) return (0);  /* doesn't intersect */

    x = intersect[X];
    y = intersect[Y];
    intersect[Z] = (plane[X]*x + plane[Y]*y + plane[W])/-plane[Z];

    return(1);
}


P3toPlane(p1, p2, p3, plane)
Point3 p1, p2, p3;
double plane[4];
{
Point3 v1, v2, norm;
    
    v1[X] = p1[X] - p3[X];
    v1[Y] = p1[Y] - p3[Y];
    v1[Z] = p1[Z] - p3[Z];

    v2[X] = p2[X] - p3[X];
    v2[Y] = p2[Y] - p3[Y];
    v2[Z] = p2[Z] - p3[Z];

    V3Cross(v1, v2, norm);

    plane[X] = norm[X];
    plane[Y] = norm[Y];
    plane[Z] = norm[Z];
    plane[W] = -p3[X]*norm[X] - p3[Y]*norm[Y] - p3[Z]*norm[Z];
/*
fprintf(stderr,"Plane = %f, %f, %f, %f\n", plane[X],plane[Y],plane[Z],plane[W]);
*/

    return(1);
}


/* return the cross product c = a cross b */
V3Cross(a, b, c)
Point3 a, b, c;
{
	c[X] = (a[Y]*b[Z]) - (a[Z]*b[Y]);
	c[Y] = (a[Z]*b[X]) - (a[X]*b[Z]);
	c[Z] = (a[X]*b[Y]) - (a[Y]*b[X]);
	return(1);
}

/************************************************************************/

double
P2distance (from, to)
double *from, *to;
{
    double x, y;

    x = from[X] - to[X];
    y = from[Y] - to[Y];
    return (sqrt (x*x + y*y));
}

/************************************************************************/
/* get a normalized direction from v1 to v2, store in v3 */

v2dir(v1, v2, v3)
double v1[2], v2[2], v3[2];
{
double n, dx, dy;

    dx = v2[X] - v1[X];
    dy = v2[Y] - v1[Y];
    n = sqrt(dx * dx + dy * dy);
    v3[X] =  dx/n;
    v3[Y] =  dy/n;

}
/************************************************************************/

/* return 1 if outside of region or NULL or (no-zeros & zero) */
_point_is_masked(v)
Point3 v;
{ 
    double xmax, ymin, xres, yres;
    int row, col;
    long x2off, y2off, x1off, y1off;
    float e1,e2,e3,e4;
    int  omits;

    
    xres = VXRES;
    yres = VYRES;

    xmax = VXMAX;
    ymin = VYMIN;
    
    if(v[X] < 0. || v[X] > xmax || v[Y] < ymin || v[Y] > Y_Max)
	return (1);                /* outside region */

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
    if(Anozero->val){
	if (!e1){ ++omits;}
	if (!e2){ ++omits;}
	if (!e3){ ++omits;}
	if (!e4){ ++omits;}
    }
    if(Has_null){
	if (EMBNULL(elev_buf[y1off + x1off])){ ++omits;}
	if (EMBNULL(elev_buf[y1off + x2off])){ ++omits;}
	if (EMBNULL(elev_buf[y2off + x2off])){ ++omits;}
	if (EMBNULL(elev_buf[y2off + x1off])){ ++omits;}
    }

    if( omits > 3) {
	return(1);
    }
	
    return (0);

}


