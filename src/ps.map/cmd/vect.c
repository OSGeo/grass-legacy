/* Functions: nearest, adjust_line, parallel_line 
**
** Author: Radim Blazek Feb 2000
** 
**
*/
#include "math.h"
#include "Vect.h"
#include "gis.h"
#include "vector.h"

#define LENGTH(DX, DY)  (  sqrt( (DX*DX)+(DY*DY) )  )

/* vector() calculates vector form two points */
static void vect(double x1, double y1, double x2, double y2, double *x, double *y )
{
    double dx, dy, l;
    dx  = x2 - x1;
    dy  = y2 - y1;	
    l   = LENGTH ( dx, dy );
    if ( l == 0){
    }
    *x  = 5 / 0;
    *x  = dx/l;
    *y  = dy/l;
}

/* nearest returns nearest longitude coordinate, copy from src/libes */
static double nearest(double e0,double e1)
{
    while (e0 - e1 > 180)
        e1 += 360.0;
    while (e1 - e0 > 180)
        e1 -= 360.0;
    return e1;
}  

/* if projection is PROJECTION_LL adjust_line will change
**   longitudes to nearest form previous point */
void adjust_line (struct line_pnts *Points)
{
    int i, np;
    if (G_projection() == PROJECTION_LL)
    {  
	np = Points->n_points;
	for (i = 1; i < np ; i++)
	{
	    Points->x[i] = nearest (Points->x[i-1], Points->x[i]);
	}
    }
}  

/* line_rm_dupl removes duplicate points from line */
void line_rm_dupl (struct line_pnts *Points)  
{
    int i, j;

    if ( Points->n_points > 0 ) {
	j = 1;
	for (i = 1; i < Points->n_points; i++)
	{
	    if ( Points->x[i] != Points->x[j-1] || Points->y[i] != Points->y[j-1] )
	    {
		Points->x[j] = Points->x[i];
    		Points->y[j] = Points->y[i];	
		j++;
	    }

	}
	Points->n_points = j;
    }	
}

/* find_cross find first crossing between segments from s1 to s2 and from s3 to s4 
** s5 is set to first segment and s6 to second
** neighbours are taken as crossing each other only if overlap
** returns: 1 found
**         -1 found overlap 
**          0 not found    
*/
int find_cross ( struct line_pnts *Points, int s1, int s2, int s3, int s4,  int *s5, int *s6 )  
{
    int i, j, np, ret;
    double *x, *y;

    x = Points->x;
    y = Points->y;
    np = Points->n_points;

    for ( i=s1; i<=s2; i++) 
    {
	for ( j=s3; j<=s4; j++) 
	{
	    if ( j==i ){
		continue; 	    
	    }
	    ret = dig_test_for_intersection ( x[i], y[i], x[i+1], y[i+1], x[j], y[j], x[j+1], y[j+1] );     
	    if ( ret == 1 &&  ( (i-j) > 1 || (i-j) < -1 )  )
	    {
		*s5 = i;
		*s6 = j;
		return 1;
	    }	    
	    if (  ret == -1  )
	    {
		*s5 = i;
		*s6 = j;
		return -1;
	    }
	}    
    }
    return 0;
}

/* point_in_buf - test if point px,py is in d buffer of Points
** returns:  1 in buffer  
**           0 not  in buffer   
*/
int point_in_buf ( struct line_pnts *Points, double px, double py, double d )
{
    int i, np;
    double sd;

    np = Points->n_points;
    d *= d;
    for ( i=0; i < np-1; i++) 
    {
	sd = dig_distance2_point_to_line ( px, py, Points->x[i], Points->y[i], Points->x[i+1], Points->y[i+1] );     
	if ( sd <= d )
	{
	    return 1;
	}	    
    }
    return 0;
}

/* reverse_line - reverse order of Points ( direction of line ) */
void reverse_line ( struct line_pnts *Points )
{
    int i,j, np;
    double x,y;

    np = (int) Points->n_points/2 ;

    for ( i=0; i<np; i++) 
    {
	j = Points->n_points - i - 1;
	x = Points->x[i];  
	y = Points->y[i];
	Points->x[i] = Points->x[j];  
	Points->y[i] = Points->y[j];
	Points->x[j] = x;
	Points->y[j] = y;
    }
}

/* clean_parallel - clean parallel line created by parallel_line:
** - looking for loopes and if loop doesn't contain any other loop
**   and centroid of loop is in buffer removes this loop (repeated)
** - removes all end points in buffer 
** Points - parallel line, oPoints - original line, d - offset 
** note1: on some lines (multiply selfcrossing; lines with end points 
**        in buffer of line other; some shapes of ends ) may create nosense
** note2: this function is stupid and slow, somebody more clever
**        than I am should write paralle_line + clean_parallel 
**        better;    RB March 2000
*/
void clean_parallel ( struct line_pnts *Points, struct line_pnts *oPoints, double d  )  
{
    int i, j, l, np, npn, sa, sb;
    int first=0, current, last, lcount;
    double *x, *y, *xn, *yn, px, py, ix, iy;
    struct line_pnts *sPoints;    

    x = Points->x;
    y = Points->y;
    np = Points->n_points;    
    sPoints = Vect_new_line_struct(); 
    dig_alloc_points (sPoints, np);
    npn=1;

    /* remove loopes */
    while( first < np-2 ){

	/* find first loop which doesn't contain any other loop */
	current=first;  last=Points->n_points-2;  lcount=0;
	while( find_cross ( Points, current, last-1, current+1, last,  &sa, &sb ) != 0 ) 
	{  
	    if ( lcount == 0 ){ first=sa; }	
		current=sa+1;
		last=sb;    
	    lcount++;
	}
	if ( lcount == 0 ) { break; }   /* loop not found */

	/* remove loop if in buffer */		
	if ( (sb-sa) == 1 ){ /* neighbouring lines overlap */  
	    j=sb+1;
	    npn=sa+1;
	} else {
	    dig_find_intersection ( x[sa],y[sa],x[sa+1],y[sa+1],x[sb],y[sb],x[sb+1],y[sb+1], &ix,&iy);
	    sPoints->x[0]=ix;
	    sPoints->y[0]=iy;
	    l=1;
	    for ( i=sa+1 ; i < sb+1; i++ ) /* create loop polygon */
	    {	
	        sPoints->x[l]=x[i];
		sPoints->y[l]=y[i];
		l++;
	    }
	    sPoints->n_points = l;
	    Vect_find_poly_centroid  ( sPoints, &px, &py);   
	    if ( point_in_buf( oPoints, px, py, d )  ){ /* is loop in buffer ? */
		npn=sa+1;
		x[npn] = ix;
		y[npn] = iy;
		j=sb+1;
		npn++;
		if ( lcount == 0 ){ first=sb; }	
	    } else {  /* loop is not in buffer */
		first=sb;
	        continue;
	    }
	}	    

	for (i=j;i<Points->n_points;i++) /* move points down */ 
	{
	    x[npn] = x[i];
	    y[npn] = y[i];
	    npn++;
	}    
	Points->n_points=npn;
    }
    
    /* remove points from start in buffer */
    j=0;
    for (i=0;i<Points->n_points-1;i++) 
    {
	px=(x[i]+x[i+1])/2;
	py=(y[i]+y[i+1])/2;
	if ( point_in_buf ( oPoints, x[i], y[i], d*0.9999) 
	     && point_in_buf ( oPoints, px, py, d*0.9999) ){
	    j++;
	} else {
	    break;
	}
    }
    if (j>0){
	npn=0;    
	for (i=j;i<Points->n_points;i++) 
	{
	    x[npn] = x[i];
	    y[npn] = y[i];
	    npn++;
	}
	Points->n_points = npn;
    }    
    /* remove points from end in buffer */
    j=0;
    for (i=Points->n_points-1 ;i>=1; i--) 
    {
	px=(x[i]+x[i-1])/2;
	py=(y[i]+y[i-1])/2;
	if ( point_in_buf ( oPoints, x[i], y[i], d*0.9999) 
	     && point_in_buf ( oPoints, px, py, d*0.9999) ){
	    j++;
	} else {
	    break;
	}
    }
    if (j>0){
	Points->n_points -= j;
    }    
    Vect_destroy_line_struct ( sPoints );    
}

/* parallel_line - remove duplicate points from input line and 
** creates new parallel line in 'd' offset distance; 
** 'tol' is tolerance between arc and polyline;
** this function doesn't care about created loopes;
** returns pointer to new line 
*/
struct line_pnts *parallel_line (struct line_pnts *Points, double d, double tol)  
{
    int i, j, np, na, side;
    double *x, *y, nx, ny, tx, ty, vx, vy, ux, uy, wx, wy;
    double atol, atol2, a, av, aw;
    struct line_pnts *nPoints;    

    nPoints = Vect_new_line_struct(); 
    line_rm_dupl ( Points );  
    np = Points->n_points;
    x = Points->x;
    y = Points->y;
    
    if ( np == 0 )
    {
	return nPoints;
    }
    if ( np == 1 )
    {
	Vect_append_point ( nPoints, x[0], y[0] );
	return nPoints;
    }

    if ( d == 0 )
    {
	Vect_copy_xy_to_pnts ( nPoints, x, y, np );
	return nPoints;
    }

    side = d/abs(d);
    atol = 2 * acos( 1-tol/abs(d) );

    for (i = 0; i < np-1; i++)
    {
	vect ( x[i], y[i], x[i+1], y[i+1], &tx, &ty);
	vx  = ty * d;
	vy  = -tx * d;

	nx = x[i] + vx; 
	ny = y[i] + vy;
	Vect_append_point ( nPoints, nx, ny ); 	

	nx = x[i+1] + vx; 
	ny = y[i+1] + vy;
	Vect_append_point ( nPoints, nx, ny );
	
	if ( i < np-2 ) {  /* use polyline instead of arc between line segments */
	    vect ( x[i+1], y[i+1], x[i+2], y[i+2], &ux, &uy);
	    wx  =  uy * d;
	    wy  = -ux * d;		    
	    av = atan2 ( vy, vx );  
	    aw = atan2 ( wy, wx );
	    a = (aw - av) * side;
	    if ( a < 0 )  a+=2*PI;
	    if ( a < PI && a > atol)
	    {
		na = (int) (a/atol);
		atol2 = a/(na+1) * side;
		for (j = 0; j < na; j++)
		{
		    av+=atol2;
		    nx = x[i+1] + abs(d) * cos(av);
		    ny = y[i+1] + abs(d) * sin(av); 
		    Vect_append_point ( nPoints, nx, ny );
		}
	    }
	}   
    }
    line_rm_dupl ( nPoints );  
    return nPoints;
}






