/* %W% %G% */
/* put_dot.c    1.0   4/01/91
*                                                                       
*     Purpose                                                           
*        output sites(points) in a psuedo-random pattern,      
*         within the bounds of the polygon.                             
*                                                                       
*/                                                                       

#include <math.h>
#include  "gis.h"
#include "Vect.h"
#include "dots.h"

#ifndef HUGE_VAL
#define HUGE_VAL 9999999999999.0
#endif
#define		SITE_DIR	"site_lists"
/*#define DEBUG 1*/

static int I4, ipass;
static double maxd, mind, centrdx, centrdy, f_area ;
static double N,S,E,W;



put_dots(map, area, dot_cnt, temp, size_only)
struct Map_info *map;
int  area, dot_cnt, size_only;
FILE *temp;
{
    int ii, new_cnt ;
    int island ;
    double xorg, yorg ;
    P_AREA *Areas;

    N = E = -999999999.;
    W = S = 999999999.;


                     /* Calculate polygon area */
     if (V2_get_area(map,area,&Areas) != 0) 
		G_fatal_error("could not get area info\n");
     if (tmp_find_area(map,Areas,&f_area,&centrdx,&centrdy,&N,&E,&W,&S) != 0)
        {
        fprintf(stderr,"\nArea %d has NO centroid, aborting !!!\n",area);
        exit(-1);
        }

#ifdef DEBUG
printf("bounds: N %12.2lf,     S %12.2lf\n        E %12.2lf,     W %12.2lf\n",N,S,E,W);
printf("area= %.2lf, centrdx= %.2lf, centrdy= %.2lf\n",f_area,centrdx,centrdy);
#endif

     if (dot_ratio > f_area/dot_cnt) 
        {
        dot_ratio = f_area/dot_cnt;
        min_area = f_area;
        area_cnt = dot_cnt;
        }
    
     if (size_only) return(0);


     /* dot creation stuff here */
	 new_cnt = dot_cnt * ((N-S) * (E-W)/f_area);

	make_dots(temp,map,area,W,E,S,N,new_cnt,dot_cnt,centrdx,centrdy);


/*------------------------------------------ All dots Processed  */
    return(0);


}



make_dots(temp,map,garea,x1,x2,y1,y2,dots,actual,centx,centy)
FILE *temp;
struct Map_info *map;
int garea;
double x1, x2, y1, y2;
int dots,actual;
double centx,centy;
{
double area, x, y, dx, dy, factor, ratio;
int i, ii, ix ,iy, iix, iiy, r, r1, tx, ty,a ,b,DOX;
P_AREA *Areas;
int XL,XG,YL,YG;


area = (x2 - x1) * (y2 - y1);
ratio = area/dots;
factor = sqrt(ratio);
ix = (x2 - x1)/(factor);
iy = (y2 - y1)/(factor);
r = dots - (ix * iy);


if (r > 0) {
	tx = ix + 1;
	ty = iy + 1;
	r = ((tx) * (ty)) - dots;
	for (iix = ix -1; iix <= ix +1; iix++)
		for (iiy = iy -1; iiy <= iy +1; iiy++) {
			r1 = (iix * iiy) - dots ;
			if (r1 > 0 && r1 < r) {
				r = r1;
				tx = iix;
				ty = iiy;
				}
			}
	}

ix = tx;
iy = ty;


if ((x2-x1) > (y2-y1)) DOX = 1;
else DOX = 0;

dx = (x2-x1)/(ix + .1);
dy = (y2-y1)/(iy + .1);
V2_get_area(map,garea,&Areas);

i = 0;
x = centx;
y = centy;
do_point(temp,map,x,y,Areas,garea,&i);
ii = 1;
XL = XG = YL = YG = 0;
while ( i < actual ) {
	for (a = ii; a >= -ii; a -= (2 * ii))
	for (b = -ii; b <= ii; b++){
		if (DOX) {
			x = centx + (a * dx);
			y = centy + (b * dy);
			}
		else {
			x = centx + (b * dx);
			y = centy + (a * dy);
			}
		if (y < y1) YL = 1;
		if (y > y2) YG = 1;
		if (x < x1) XL = 1;
		if (x > x2) XG = 1;
		if (i < actual) do_point(temp,map,x,y,Areas,garea,&i);
		}
	for (a = ii; a >= -ii; a -= (2 * ii))
	for (b = 1-ii; b <= ii-1; b++){
		if (DOX) {
			x = centx + (b * dx);
			y = centy + (a * dy);
			}
		else {
			x = centx + (a * dx);
			y = centy + (b * dy);
			}
		if (y < y1) YL = 1;
		if (y > y2) YG = 1;
		if (x < x1) XL = 1;
		if (x > x2) XG = 1;
		if (i < actual) do_point(temp,map,x,y,Areas,garea,&i);
		}
	if (XL && XG && YL && YG) break;
	ii++;
	}
if (i < actual) do_point(temp,map,centx+dy/2.0,centy+dy/2.0,Areas,garea,&i);
if ( i == 0 ){
/*DEBUG*/ fprintf(stderr,"actual_%d wanted %d got %d\n",actual,dots,i);
	fprintf(stderr,"X_%lf %lf Y_%lf %lf CX_%lf CY_%lf\n",x1,x2,y1,y2,centx,centy);
	}
}




/* do point if in area */

int do_point(temp,map,x,y,Areas,garea,pnt)
FILE *temp;
struct Map_info *map;
double x, y;
P_AREA *Areas;
int garea,*pnt;
{
P_AREA *Axxx;
int iflag, ii, island;
double inarea;

/*fprintf(stderr,"IN_A x_%lf y_%lf ",x,y);*/
iflag = 0;
		if ((inarea = dig_point_in_area(map,x,y,Areas)) != 0.0)
			{
			iflag = 1;
     /* ----------------------------------------------
        Dot coord. is INSIDE Area, check for Islands */

		      if (map->Area[garea].n_isles != 0) 
	 			{
	 			for (ii=0; ii < map->Area[garea].n_isles; ii++)
		    		{
           			island = map->Area[garea].isles[ii];
	      /* set "Areas" pointer to the island */
           			V2_get_area(map,island,&Axxx);
					if ((inarea = dig_point_in_area(map,x,y,Axxx)) == 0.0)
						iflag = 0;
         			}
				}
         	if (iflag) {
				fprintf(temp,"%lf|%lf|#%d\n",x,y,*pnt);
				*pnt = *pnt + 1;
				}
			}

return(iflag);
}

