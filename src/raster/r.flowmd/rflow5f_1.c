/****************************************************************************
**  Written by Jaroslav Hofierka 1992,1997 and 2000.
**  J. Gresaka 22, 085 01 Bardejov, Slovakia, hofierka@geomodel.sk
**  Copyright Jaroslav Hofierka 1992,1997,2000
**  email: hofierka@geomodel.sk
*****************************************************************************/

/*
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the
 *   Free Software Foundation, Inc.,
 *   59 Temple Place - Suite 330,
 *   Boston, MA  02111-1307, USA.
 *
 */


#define M2_PI    2 * M_PI
#define RAD      360. / (2. * M_PI)
#define DEG      (2. * M_PI)/360.
#define UNDEF    365. * DEG           /*change to undefined when available*/
#define UNDEFZ   0.                   /*change to undefined when available*/
#define SKIP    "1"
#define BIG      1.e20
#define IBIG     32767
#define EPS      1.e-4

                                
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "dig_atts.h"
#include "bitmap.h"

FILE *felevin,*faspin, *fflout, *flgout, *fdsout;
FILE *fbarierin;

char *elevin;
char *aspin;
char *barierin;
char *flout = NULL;
char *lgout = NULL;
char *dsout = NULL;
char *mapset = NULL;
char *per;
char *down;
char mapname[1024];

struct Cell_head cellhd,elevhd,barhd,asphd;
struct Map_info Map;
struct dig_head Head;
struct BM *bitbar;

void    calculate();
double   coef_of_line();
int     quadrant();
int     combine_y();
int     combine_x();
int     vertex();
void    new_point_x();
void    new_point_y();
void    which_one();
int     mesh_vertex();
int     mesh_line();
void     go_on();
double amax1();

int     ori, typ, n, m, point, skip, ip1, jp1, ip2, jp2;
float     **z, **o;
double  stepx, stepy, xp, yp, op, zp, dp, *px, *py, angle;
float     **lg, **density;
double  xmin, xmax, ymin, ymax;   
int     imin0, imax0, jmin0, jmax0;

int (*func)();


main(argc, argv)
int argc;
char *argv[];
 

{
/* double amax1();*/
 double amin1();
 int    max();
 int    min();

 struct GModule *module;
 struct
  {
	struct Option *elevin, *aspin, *barierin, *skip, *flout, *lgout, *dsout;
	} parm;
 struct 
  {     struct Flag   *down;
        } flag;


	G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"construction of slope lines (flowlines),  flowpath lengths "
		"and flowline  densities from a raster digital elevation "
		"model using a modified multiple directions algorithm.";

        if(G_get_set_window(&cellhd)==-1) exit(0);
        stepx = cellhd.ew_res/cellhd.ew_res;
        stepy = cellhd.ns_res/cellhd.ew_res;
        n/*n_cols*/ = cellhd.cols;
        m/*n_rows*/ = cellhd.rows; 
        xmin = 0.;
        ymin = 0.;

	parm.elevin = G_define_option();
	parm.elevin->key = "elevin";
	parm.elevin->type = TYPE_STRING;
	parm.elevin->required = YES;
        parm.elevin->gisprompt = "old,cell,raster"; 
	parm.elevin->description = "Name of the elevation raster file";

	parm.aspin = G_define_option();
	parm.aspin->key = "aspin";
	parm.aspin->type = TYPE_STRING;
	parm.aspin->required = YES;
        parm.aspin->gisprompt = "old,cell,raster"; 
	parm.aspin->description = "Name of the aspect raster file";

	parm.barierin = G_define_option();
	parm.barierin->key = "barierin";
	parm.barierin->type = TYPE_STRING;
	parm.barierin->required = NO;
        parm.barierin->gisprompt = "old,cell,raster"; 
	parm.barierin->description = "Name of the barier raster file";

/*   use this if slopemin will be added */
/*
	parm.dmin1 = G_define_option();
	parm.dmin1->key = "dmin";      
	parm.dmin1->type = TYPE_DOUBLE;
        parm.dmin1->answer = dminchar;
	parm.dmin1->required = NO;
        parm.dmin1->description = "Min distance between points (extra points ignored)";
*/
	parm.skip = G_define_option();
	parm.skip->key = "skip";      
	parm.skip->type = TYPE_INTEGER;
        parm.skip->answer = SKIP;
	parm.skip->required = NO;
        parm.skip->description = "Number of cells skiped when constr. flowl.";

	parm.flout = G_define_option();
	parm.flout->key = "flout";
	parm.flout->type = TYPE_STRING;
	parm.flout->required = NO;
	parm.flout->description = "Output flowline file (vector)";

	parm.lgout = G_define_option();
	parm.lgout->key = "lgout";
	parm.lgout->type = TYPE_STRING;
	parm.lgout->required = NO;
	parm.lgout->description = "Output slope length file (raster)";

	parm.dsout = G_define_option();
	parm.dsout->key = "dsout";
	parm.dsout->type = TYPE_STRING;
	parm.dsout->required = NO;
	parm.dsout->description = "Output flowline density file (raster)";


 
	flag.down = G_define_flag();
	flag.down->key = 'd';
        flag.down->description = "Compute downslope flowlines";


	if (G_parser(argc,argv))
		exit(1);

        ori=flag.down->answer;      

/*       fprintf(stdout, "ori= %d \n", ori);*/

	elevin = parm.elevin->answer;
        aspin = parm.aspin->answer;
        barierin = parm.barierin->answer;
	flout  = parm.flout->answer;
	lgout = parm.lgout->answer;
	dsout = parm.dsout->answer;

	sscanf(parm.skip->answer,"%d",&skip) ;

        
/**********end of parser - ******************************/

       INPUT();
       calculate();
       OUTGR(); 
       fprintf(stderr,"\n");
       
   return 1;
}


INPUT()

{
    FCELL    *cell1, *cell2;
    FCELL    *cell3;
    int     fd1, fd2, fd3, row, row_rev;
    int     l,i,j;

    cell1=G_allocate_f_raster_buf();
    cell2=G_allocate_f_raster_buf();

    if (barierin != NULL)
    cell3=G_allocate_f_raster_buf();
 
    z = (float **)G_malloc(sizeof(float*)*(m));
    o = (float **)G_malloc(sizeof(float*)*(m));

  for(l=0;l<m;l++) 
   {
    z[l]   = (float*)G_malloc(sizeof(float)*(n));
    o[l]   = (float*)G_malloc(sizeof(float)*(n));
    }

  if((mapset=G_find_cell(elevin,""))==NULL)
    G_fatal_error("cell file %s not found", elevin);

  if (G_get_cellhd(elevin,mapset,&elevhd)<0)
    G_fatal_error("Cannot get elev header");

  if((mapset=G_find_cell(aspin,""))==NULL)
    G_fatal_error("cell file %s not found", aspin);

  if (G_get_cellhd(aspin,mapset,&asphd)<0)
    G_fatal_error("Cannot get aspect header");

  if (!((cellhd.ew_res==elevhd.ew_res)&&(cellhd.ew_res==asphd.ew_res)&&
        (cellhd.ns_res==elevhd.ns_res)&&(cellhd.ns_res==asphd.ns_res)))
    G_fatal_error("input files resolution differs from current region resolution");
        

  fd1 = G_open_cell_old(elevin,mapset);
  fd2 = G_open_cell_old(aspin,mapset);

  if (barierin != NULL)
  {
     fd3 = G_open_cell_old(barierin,mapset);
     bitbar = BM_create (n, m);
    fprintf(stdout, "DEG %f, bitbar created\n", DEG);
  }

/*  for (row=0; row<m; ++row)*/
  for (row=1; row<m-1; row++)
  {
      G_get_f_raster_row(fd1,cell1,row);
      G_get_f_raster_row(fd2,cell2,row);
      if (barierin != NULL)
      G_get_f_raster_row(fd3,cell3,row);

    for (j=1; j<n-1; j++)
    {
       row_rev = m - row - 1;
       z[row_rev][j] = (float ) cell1[j];
       o[row_rev][j] = (float ) cell2[j];
       if ( barierin != NULL )
       {
          if (cell3[j] == 0)
             BM_set (bitbar, j, row_rev, 0);
          else
             {
             BM_set (bitbar, j, row_rev, 1);
             }
        }
     }
   }
  G_close_cell(fd1);
  G_close_cell(fd2);


/*******transformation of angles from 0 to east counterclock
        to 0 to north clocwise, for ori=0 upslope flowlines
        turn the orientation 2*M_PI ************/

  for (i = 1; i < m-1; i++)
  {
      for (j = 1; j < n-1; j++)
      { 
        if ( o[i][j] == 0 )
             o[i][j] = 365;
        else    
           if( o[i][j] < 90 )
               o[i][j] = 90 - o[i][j]; 
           else
              o[i][j] = 450  - o[i][j];

         if (ori == 0) 
         {
		if ((o[i][j] >= 0) && (o[i][j] <= 180))
                	o[i][j] = o[i][j] + 180;
		else
                        if ((o[i][j] > 180) && (o[i][j] <= 360))
		        	o[i][j] = o[i][j] - 180;
         }
      }
   }
}

int     OUTGR()
{
   FCELL     *cell3,*cell4;
    CELL data2;
   int      cf3,cf4;
   int      i,iarc,j; 
   float    dsmax=0.;
   char     msg[100];
   struct Colors colors;
   struct History hist;
     
   
        if (lgout != NULL)  
        {
	  cell3 = G_allocate_f_raster_buf();
	  cf3 = G_open_fp_cell_new (lgout);
	  if (cf3 < 0)
	  {
		sprintf (msg, "unable to create raster map %s", lgout);
		G_fatal_error (msg);
		exit(1);
	  }
        }

        if (dsout != NULL)  
        {
	  cell4 = G_allocate_f_raster_buf();
	  cf4 = G_open_fp_cell_new (dsout);
	  if (cf4 < 0)
	  {
		sprintf (msg, "unable to create raster map %s", dsout);
		G_fatal_error (msg);
		exit(1);
	  }
        }

       if(G_set_window (&cellhd) < 0)
         exit(3);

       if (m != G_window_rows())
       {
	fprintf (stderr, "OOPS: rows changed from %d to %d\n", m, G_window_rows());
	exit(1);
       }
       if (n != G_window_cols())
       {
	fprintf (stderr, "OOPS: cols changed from %d to %d\n", n, G_window_cols());
	exit(1);
       }

      for(iarc=0;iarc<m;iarc++) 
      { 
	i=m-iarc-1;
        if(lgout!=NULL)  
        {
          for(j=0;j<n;j++) 
          {
            cell3[j]=(FCELL) lg[i][j];
          }
          G_put_f_raster_row (cf3, cell3);          
        }
        if(dsout!=NULL)  
        {
          for(j=0;j<n;j++) 
          {
            cell4[j]=(FCELL)density[i][j];
            dsmax = amax1(dsmax,density[i][j]);
          }
          G_put_f_raster_row (cf4, cell4);          
        }
      }
        if(lgout!=NULL)
	G_close_cell (cf3);
        if(dsout!=NULL)
	G_close_cell (cf4);

	/* colortable for flowline density */
	if (dsout!=NULL)
	{
	    G_init_colors (&colors);

	    data2 = (CELL)dsmax;
    G_add_color_rule (   -1,   0,   0,   0,     -1,   0,   0,   0, &colors);
    G_add_color_rule (    0, 255, 255, 255,      5, 255, 255,   0, &colors);
    G_add_color_rule (    5, 255, 255,   0,     30,   0, 255, 255, &colors);
    G_add_color_rule (   30,   0, 255, 255,    100,   0, 127, 255, &colors);
    G_add_color_rule (  100,   0, 127, 255,   1000,   0,   0, 255, &colors);
    G_add_color_rule ( 1000,   0,   0, 255,  data2,   0,   0,   0, &colors);
		mapset = G_find_file("cell",dsout,"");
		if(mapset==NULL)
		{
		    sprintf(msg,"file [%s] not found", dsout);
		    G_fatal_error(msg);
		}
		G_write_colors (dsout, mapset, &colors);
         }

     if (barierin != NULL) BM_destroy (bitbar);

        return  1;
}       



double amax1(arg1,arg2) 
 double arg1;
 double arg2;
{
 double res;
 if (arg1>=arg2) { 
   res = arg1;
 }
 else  {
   res = arg2;
 }
 return res;
}



double amin1(arg1,arg2) 
 double arg1;
 double arg2;
{
 double res;
 if (arg1<=arg2) { 
   res = arg1;
 }
 else  {
   res = arg2;
 }
 return res;
}

 

int min(arg1,arg2) 
 int arg1;
 int arg2;
{
 int res;
 if (arg1 <= arg2) 
  { 
   res = arg1;
  }
 else  
  {
   res = arg2;
   }
 return res;
}


int max(arg1,arg2) 
 int arg1;
 int arg2;
{
 int res;
 if (arg1>=arg2) { 
   res = arg1;
 }
 else  {
   res = arg2;
 }
 return res;
}

int quadrant()
{
    int q;

    q = 1;
    if (angle > (0.5 * M_PI) && angle <= M_PI)         q = 2;
    if (angle > M_PI         && angle <= (1.5 * M_PI)) q = 3;
    if (angle > (1.5 * M_PI) && angle <= (2. * M_PI))  q = 4;
    return q;
}

double coef_of_line()

{
	double pom;
	pom = yp * sin(angle) - xp * cos(angle);
	return (pom);
}


void new_point_x (c, xl, yl, dis)
double *xl,*yl,*dis;
int c;
{
	double x1, d, dx, dy, pom, pomx, pomy;

	x1 = - (xmin + c * stepx);
	d = - sin(angle);

        if ( fabs(d) < EPS )
        {
           *xl = BIG;
           *yl = BIG;
           *dis = BIG;
           return;
        }
        else
        {
	   dx = x1 * sin(angle);
	   dy = - dp + x1 * cos(angle);
	   *xl = dx / d;
	   *yl = dy / d;
        }
	pomx = *xl - xp;
        pomx = pomx * pomx;
	pomy = *yl - yp;
        pomy = pomy * pomy;
        pom = pomx + pomy;
	pom = sqrt(pom);
	*dis = pom;

}

void new_point_y (c, xl, yl, dis)
double *xl,*yl,*dis;
int c;
{
	double y1, d, dx, dy, pom, pomx, pomy;

	y1 = - (ymin + c * stepy);
	d = - cos(angle);

        if ( fabs(d) < EPS )
        {
           *xl = BIG;
           *yl = BIG;
           *dis = BIG;
           return;
        }
        else
        {
	   dx = y1 * sin(angle) + dp;
	   dy = y1 * cos(angle);
	   *xl = dx / d;
	   *yl = dy / d;
        }
	pomx = *xl - xp;
        pomx = pomx * pomx;
	pomy = *yl - yp;
        pomy = pomy * pomy;
        pom = pomx + pomy;
	pom = sqrt(pom);
	*dis = pom;
}

void which_one ( dis1, dis2, x1,  y1, x2, y2)
double dis1,dis2,x1,y1,x2,y2;

/* decides which point is closer to the previous point of flowline*/

{
   if (dis1 < dis2) 
   {
      xp = x1; 
      yp = y1;
   }
   else 
   {
      xp = x2;
      yp = y2;
   }
}

int combine_y(k1, l1, k2, l2)
int k1,l1,k2,l2;
{
	double d1, d2, a1, a2, c1, c2, z1;
	int bp1 = 0;
	int bp2 = 0;

        if ((l1 < 0) || (l1 > m-1) ||
            (l2 < 0) || (l2 > m-1) ||
            (k1 < 0) || (k1 > n-1) ||
            (k2 < 0) || (k2 > n-1))
            return (0);

	d1 = ymin + (double)l1 * stepy;
	d2 = ymin + (double)l2 * stepy;
	a1 = (double)o[l1][k1] * DEG;
        a2 = (double)o[l2][k2] * DEG;
/***** NEW ***/
	if (barierin != NULL){
	bp1 = (int) BM_get( bitbar, k1, l1);
        bp2 = (int) BM_get( bitbar, k2, l2);
	if(bp1 != 0 || bp2 != 0) return (0);
	}
	if (!(a1 == UNDEF || a2 == UNDEF)) 
        {
		c1 = (yp - d1) / (d2 - d1);
		c2 = 1. - c1;
                
		if (fabs (a1 - a2) >= M_PI) 
                {
                    if ((a1 >= 0) && (a2 > M_PI))
                    {
                       a2 -= M2_PI;
                       angle = c1 * a2 + c2 * a1;
                       if (angle < 0)
                           angle += M2_PI;
                    }
                    else
                    {
                        a1 -= M2_PI;
                        angle = c1 * a2 + c2 * a1;
                        if (angle < 0)
                            angle += M2_PI;
                    }
                 }
                 else
                    angle = c1 * a2 + c2 * a1;

	         a1 = (double)z[l1][k1];
                 a2 = (double)z[l2][k2];
	         z1 = c1 * a2 + c2 * a1 ;

	        if (ori == 1) 
                {
			if (z1 >= zp) return 0;
		}
		else 
                {
			if (z1 <= zp) return 0;
		}
		zp = z1;

		if (dsout != NULL) {
                          if (c2 > c1) {
      if (ip1 != k1 || jp1 != l1) {
			density[l1][k1] += c2;
			ip1 = k1; jp1 = l1;
				}
	if ((ip1 != k2 || jp1 != l2) && (ip2 != k2 || jp2 != l2)) {
		density[l2][k2] += c1;
		ip2 = k2; jp2 = l2;
			} 
			}
			else {
      if (ip1 != k2 || jp1 != l2) {
			density[l2][k2] +=c1;
				ip1 = k2; jp1 = l2;
                                    }
	if ((ip1 != k1 || jp1 != l1) && (ip2 != k1 || jp2 != l1)) {
		density[l1][k1] += c2;
		ip2 = k1; jp2 = l1;
				}
				}
                              }

                func = mesh_line;
                return 1;
	}
	else
		return 0;
}

int combine_x (k1, l1, k2, l2)
int k1,l1,k2,l2;
{
	double d1, d2, a1, a2, c1, c2, z1;
        int bp1 = 0;
        int bp2 = 0;


        if ((l1 < 0) || (l1 > m-1) ||
            (l2 < 0) || (l2 > m-1) ||
            (k1 < 0) || (k1 > n-1) ||
            (k2 < 0) || (k2 > n-1))
            return (0);

	d1 = xmin + (double)k1 * stepx;
	d2 = xmin + (double)k2 * stepx;
	a1 = (double)o[l1][k1] * DEG;
        a2 = (double)o[l2][k2] * DEG;
/***** NEW ***/
	if (barierin != NULL){
        bp1 = (int) BM_get( bitbar, k1, l1);
        bp2 = (int) BM_get( bitbar, k2, l2);
        if(bp1 != 0 || bp2 != 0) return (0);
	}

	if (!(a1 == UNDEF || a2 == UNDEF)) 
        {
		c1 = (xp - d1) / (d2 - d1);
		c2 = 1. - c1;

		if (fabs (a1 - a2) >= M_PI) 
                {
                    if ((a1 >= 0) && (a2 > M_PI))
                    {
                       a2 -= M2_PI;
                       angle = c1 * a2 + c2 * a1;
                       if (angle < 0)
                           angle += M2_PI;
                    }
                    else
                    {
                        a1 -= M2_PI;
                        angle = c1 * a2 + c2 * a1;
                        if (angle < 0)
                            angle += M2_PI;
                    }
                }
                else
                    angle = c1 * a2 + c2 * a1;

	        a1 = (double)z[l1][k1];
                a2 = (double)z[l2][k2];
	        z1 = c1 * a2 + c2 * a1;

		if (ori == 1) 
                {
			if (z1 >= zp) return 0;
		}
		else 
                {
			if (z1 <= zp) return 0;
		}
		zp = z1;

                if (dsout != NULL) {
                          if (c2 > c1) {
      if (ip1 != k1 || jp1 != l1) {
                        density[l1][k1] += c2;
                        ip1 = k1; jp1 = l1;
                                }
        if ((ip1 != k2 || jp1 != l2) && (ip2 != k2 || jp2 != l2)) {
                density[l2][k2] += c1;
                ip2 = k2; jp2 = l2;
                        } 
			}
                        else {
      if (ip1 != k2 || jp1 != l2) {
                        density[l2][k2] +=c1;
                                ip1 = k2; jp1 = l2;
                                    }
        if ((ip1 != k1 || jp1 != l1) && (ip2 != k1 || jp2 != l1)) {
                density[l1][k1] += c2;
                ip2 = k1; jp2 = l1;
                                }
                                }

                              }
		
                func = mesh_line;
                return 1;
	}
	else
		return 0;
}

int vertex (k1, l1)
int k1,l1;
{
	double z1;
        int bp1 = 0;

        if (( l1 < 0) || (l1 > m-1) || (k1 < 0) || (k1 > n-1))
           return 0;

	angle = (double)o[l1][k1] * DEG;
	z1 = (double)z[l1][k1]; 
	/***** NEW ***/
	if (barierin != NULL){
        bp1 = (int) BM_get( bitbar, k1, l1);
        if(bp1 != 0) return (0);
	}
	if (angle != UNDEF)
        {
		if (ori == 1)
                {
			if (z1 >= zp) return 0;
		}
	        	else
                        {
			if (z1 <= zp) return 0;
	        	}
		zp = z1;

                if (dsout != NULL) {
      if (ip1 != k1 || jp1 != l1) {
                        density[l1][k1] += 1.;
                        ip1 = k1; jp1 = l1;
                                }
                              }

                func = mesh_vertex;
                return 1;
	}
	else
        {
	   return 0;
        }
}


int mesh_vertex()

{
	double x1, x2, y1, y2, dis, dis1, dis2;
	int q;
	int succes = 0;
	int cond1, cond2;

        dis = 0;
	q = quadrant ();
	dp = coef_of_line ();

	switch (q) {
		case 1 : {
			cond1 = (imax0 >= n );
			cond2 = (jmax0 >= m );
			cond1 = cond1 || cond2;
			if (!cond1) 
                        {
		           new_point_x (imax0, &x1, &y1, &dis1);
			   new_point_y (jmax0, &x2, &y2, &dis2);
			   which_one (dis1, dis2, x1, y1, x2, y2);
                           if (dis1 == BIG)
                           {
                              succes = vertex (imax0-1, jmax0);
                              if (succes == 0) return succes;
                              jmin0++;
                              jmax0++;
                               dis = dis2; 
                           }
                           else 
                              if (dis2 == BIG)
                              {
                                 succes = vertex (imax0, jmax0-1);
                                 if (succes == 0) return succes;
                                 imin0++;
                                 imax0++;
                                 dis = dis1;
                               }
                               else
                               { 
			   if (fabs(dis1 - dis2) > EPS) 
                           {
                             if (dis1 < dis2)
                             {
                	        succes = combine_y(imax0,jmax0,imax0,jmax0-1);
                                if (succes == 0) return succes;
				imax0++;
                                typ = 0;
                                dis = dis1;
			     }
			     else 
                                if (dis1 > dis2)
                                {
         		   succes = combine_x(imax0,jmax0,imax0-1,jmax0);
                                   if (succes == 0) return succes;
	                           jmax0++;
                                   typ = 1;
                                   dis = dis2;
			        }
                              }
                            else
                            {
                              succes = vertex (imax0, jmax0);
                              if (succes == 0) return succes;
                              imax0++;
                              jmax0++;
                              dis = dis1;
                            }
			    imin0++;
			    jmin0++;
                          }
			}
			break;
		}
		case 2 : {
			cond1 = (imax0 >= n);
			cond2 = (jmin0 <= -1);
			cond1 = cond1 || cond2;
			if (!cond1) 
                        {
				new_point_x (imax0, &x1, &y1, &dis1);
				new_point_y (jmin0, &x2, &y2, &dis2);
				which_one (dis1, dis2, x1, y1, x2, y2);
                           if (dis1 == BIG)
                           {
                              succes = vertex (imax0-1, jmin0);
                              if (succes == 0) return succes;
                              jmin0--;
                              jmax0--;
                              dis = dis2;
                           }  
                           else
                           {
                                if (fabs(dis1 - dis2) > EPS) 
                                {
				   if (dis1 < dis2) 
                                   {
                                      succes = combine_y (imax0,jmin0,imax0,jmin0+1);
                                      if (succes == 0) return succes;
                                      imax0++;
                                      typ = 0;
                                      dis = dis1;
				    }
				    else
                                      if (dis1 > dis2) 
                                      {
				       succes = combine_x (imax0,jmin0,imax0-1,jmin0);
                                       if (succes == 0) return succes;
                                       jmin0--;
                                       typ = 1;
                                       dis = dis2;
                                      }
                                 }
                                 else
                                 {
                                    succes = vertex(imax0, jmin0);
                                    if (succes == 0) return succes;
                                    imax0++;
                                    jmin0--;
                                    dis = dis1;
                                  } 
			 imin0++;
			 jmax0--;
                         }
			}
			break;
		}
		case 3 : {
			cond1 = (imin0 <= -1);
			cond2 = (jmin0 <= -1);
			cond1 = cond1 || cond2;
			if (!cond1) 
                        {
		           new_point_x (imin0, &x1, &y1, &dis1);
			   new_point_y (jmin0, &x2, &y2, &dis2);
			   which_one (dis1, dis2, x1, y1, x2, y2);
                              if (dis2 == BIG)
                              {
                                 succes = vertex (imin0, jmax0-1);
                                 if (succes == 0) return succes;
                                 imin0--;
                                 imax0--;
                                 dis = dis1;
                               }
                               else
                               { 
			   if (fabs(dis1 - dis2) > EPS) 
                           {
                             if (dis1 < dis2)
                             {
                	        succes = combine_y(imin0,jmin0,imin0,jmin0+1);
                                if (succes == 0) return succes;
				imin0--;
                                typ = 0;
                                dis = dis1;
			      }
			      else 
                                if (dis1 > dis2)
                                {
         			succes = combine_x(imin0,jmin0,imin0+1,jmin0);
                                if (succes == 0) return succes;
	                        jmin0--;
                                typ = 1;
                                dis = dis2;
		                }
                            }
                            else
                            {
                              succes = vertex (imin0, jmin0);
                              if (succes == 0) return succes;
                              imin0--;
                              jmin0--;
                              dis = dis1;
                            }
			    imax0--;
			    jmax0--;
                          }
			}
			break;
		}
		case 4 : {
			cond1 = (imin0 <= -1);
			cond2 = (jmax0 >= m);
			cond1 = cond1 || cond2;
			if (!cond1) 
                        {
		           new_point_x (imin0, &x1, &y1, &dis1);
			   new_point_y (jmax0, &x2, &y2, &dis2);
			   which_one (dis1, dis2, x1, y1, x2, y2);
                           if (dis1 == BIG)
                           {
                              succes = vertex (imax0-1, jmax0);
                              if (succes == 0) return succes;
                              jmin0++;
                              jmax0++;
                              dis = dis2;
                           }
                          else
                          { 
			   if (fabs(dis1 - dis2) > EPS) 
                           {
                             if (dis1 < dis2)
                             {
                	        succes = combine_y(imin0,jmax0,imin0,jmax0-1);
                                if (succes == 0) return succes;
				imin0--;
                                typ = 0;
                                dis = dis1;
			      }
			      else
                                if (dis1 > dis2)
                                {
         			succes = combine_x(imin0,jmax0,imin0+1,jmax0);
                                if (succes == 0) return succes;
	                        jmax0++;
                                typ = 1;
                                dis = dis2;
			        }
                            }
                            else
                            {
                              succes = vertex (imin0, jmax0);
                              if (succes == 0) return succes;
                              imin0--;
                              jmax0++;
                              dis = dis1;
                            }
			    imax0--;
			    jmin0++;
                          }
			}
			break;
		}
	}
	return succes ;
}

int mesh_line()

{
	double x1, x2, y1, y2, dis, dis1, dis2;
	int q;
	int succes = 0;
	int cond1, cond2;

        dis = 0;
	q = quadrant ();
	dp = coef_of_line ();

	switch (q) {
        	case 1 :
                {
	       	cond1 = (imax0 > (n - 1));
		cond2 = (jmax0 > (m - 1));
		cond1 = cond1 || cond2;
		if (!cond1) 
                   {
			new_point_x (imax0, &x1, &y1, &dis1);
			new_point_y (jmax0, &x2, &y2, &dis2);
			which_one (dis1, dis2, x1, y1, x2, y2);
			if (fabs(dis1 - dis2) > EPS ) 
                        {
			   if (dis1 < dis2) 
                           {
                              dis = dis1;
			      if (typ == 1) 
                              {
                                if (dis2 == BIG)
                                {
                                   succes = vertex (imax0,jmax0-1);
                                   imax0++;
                                   return succes;
                                }
                                else
                                {
	                          succes = combine_y(imax0,jmin0+1,imax0,jmax0);
                                  if (succes == 0) return succes;
             			  jmin0++;
                                }
                              }
			      else
                              {
                        	  succes = combine_y(imax0,jmin0,imax0,jmax0);
                                  if (succes == 0) return succes;
				  imin0++;
                              }
			      imax0++;
                      typ = 0;
                         }	
		         else 
                              if (dis1 > dis2)
                              {
                                 dis = dis2;
				 if (typ == 1) 
                                 {
				     succes=combine_x(imin0,jmax0,imax0,jmax0);
                                     if (succes == 0) return succes;
				     jmin0++;
			         }
				 else 
                                 {
                                   if (dis1 == BIG)
                                   {
                                      succes = vertex (imax0-1,jmax0);
                                      jmax0++;
                                      return succes;
                                    }
                                    else
                                    {
				     succes = combine_x(imin0+1,jmax0,imax0,jmax0);
                                     if (succes == 0) return succes;
				     imin0++;
                                   }
				 }
				 jmax0++;
                                 typ = 1;
			 }
			}
                        else
                        {
                           dis = dis1;
                           succes = vertex(imax0, jmax0);
                           if (succes == 0) return succes;
                           if (typ == 1)
                           {
                            jmin0++;
                           }
                           else  
                           {
                            imin0++;
                           }
                           imax0++;
                           jmax0++; 
			}
                     }
		     break;
		}
		case 2 : {
			cond1 = (imax0 > (n - 1));
			cond2 = (jmin0 < 0);
			cond1 = cond1 || cond2;
			if (!cond1) 
                        {
			   new_point_x (imax0, &x1, &y1, &dis1);
			   new_point_y (jmin0, &x2, &y2, &dis2);
			   which_one (dis1, dis2, x1, y1, x2, y2);
			   if (fabs(dis1 - dis2) > EPS) 
                           {
				if (dis1 < dis2) 
                                {
                                    dis = dis1;
				    if (typ == 1) 
                                    {
                                      if (dis2 == BIG)
                                      {
                                        succes = vertex (imax0, jmax0-1);
                                        imax0++;
                                        return succes;
                                      }
                                      else
                                      {
                                       succes = combine_y(imax0,jmin0,imax0,jmin0+1);
                                       if (succes == 0) return succes;
				       jmax0--;
                                       }
                                      }
                                      else
                                      {
                                       succes = combine_y (imax0,jmax0,imax0,jmin0);
                                       if (succes == 0) return succes;
				       imin0++;
				    }
                                    imax0++;
                                    typ = 0;
				}
				else if (dis1 > dis2) 
                                     {
                                        dis = dis2;
					if (typ == 1) 
                                        {
				           succes=combine_x(imin0,jmin0,imax0,jmin0);
                                           if (succes == 0) return succes;
				           jmax0--;
					}
					else 
                                        {
                                          if (dis1 == BIG)
                                          {
                                             succes = vertex (imax0-1,jmin0);
                                             jmin0--;
                                             return succes;
                                           }
                                           else
                                           {
		               	           succes=combine_x(imin0+1,jmin0,imax0,jmin0);
                                           if (succes == 0) return succes;
					   imin0++;
                                           }
					}
					jmin0--;
                                        typ = 1;
                                      }
                        }
                        else
                        {
                           dis = dis1;
                           succes = vertex(imax0,jmin0);
                           if (succes == 0) return succes;
                           if (typ == 1)
                           {
                              jmax0--;
                           }
                           else 
                           {
                              imin0++;
                           }
                           imax0++;
                           jmin0--; 
			}
		}
		break;
	     }
	     case 3 : {
			cond1 = (imin0 < 0);
			cond2 = (jmin0 < 0);
			cond1 = cond1 || cond2;
			if (!cond1) 
                        {
				new_point_x (imin0, &x1, &y1, &dis1);
				new_point_y (jmin0, &x2, &y2, &dis2);
				which_one (dis1, dis2, x1, y1, x2, y2);
				if (fabs(dis1 - dis2) > EPS) 
                                {
				    if (dis1 < dis2) 
                                    {
                                        dis = dis1;
					if (typ == 1) 
                                        {
                                          if (dis2 == BIG)
                                          {
                                            succes = vertex(imin0, jmax0-1);
                                            imin0--;
                                            return succes;
                                           }
                                           else
                                           {
				           succes=combine_y(imin0,jmin0,imin0,jmin0+1);
                                           if (succes == 0) return succes;
					   jmax0--;
                                           }
					}
					else 
                                        {
	                		    succes=combine_y(imin0,jmin0,imin0,jmax0);
                                           if (succes == 0) return succes;
					   imax0--;
				        }
					imin0--; 
                                        typ = 0;
				     }
				     else if (dis1 > dis2) 
                                          {
                                              dis = dis2;
					      if (typ == 1) 
                                              {
				                 succes=combine_x(imin0,jmin0,imax0,jmin0);
                                                 if (succes == 0) return succes;
						 jmax0--;
					       }
					       else 
                                               {
                                                 if (dis1 == BIG)
                                                 {
                                                    succes = vertex(imax0-1,jmin0);
                                                    jmin0--;
                                                    return succes;
                                                 }
                                                 else
                                                 {
				                    succes=combine_x(imin0,jmin0,imin0+1,jmin0);
                                                    if (succes == 0) return succes;
						    imax0--;
					         }
                                               }
					       jmin0--;
                                               typ=1;
					}
                        }
                        else
                        {
                           dis = dis1;
                           succes = vertex(imin0, jmin0);
                           if (succes == 0) return succes;
                           if (typ == 1) jmax0--;
                               else imax0--;
                           imin0--;
                           jmin0--; 
			}
		     }
		     break;
		}
		case 4 : {
			cond1 = (imin0 < 0);
			cond2 = (jmax0 > (m - 1));
			cond1 = cond1 || cond2;
			if (!cond1) 
                        {
			    new_point_x (imin0, &x1, &y1, &dis1);
			    new_point_y (jmax0, &x2, &y2, &dis2);
			    which_one (dis1, dis2, x1, y1, x2, y2);
			    if (fabs(dis1 - dis2) > EPS)
                            {
				if (dis1 < dis2) 
                                {
                                   dis = dis1;
				   if (typ == 1) 
                                   {
                                      if (dis2 == BIG)
                                      {
                                         succes = vertex(imin0, jmax0-1);
                                         imin0--;
                                         return succes;
                                      }
                                      else
                                      {
				        succes=combine_y(imin0,jmax0,imin0,jmax0-1);
                                        if (succes == 0) return succes;
					jmin0++;
                                      }
				    }
				    else 
                                    {
				        succes=combine_y(imin0,jmax0,imin0,jmin0);
                                        if(succes == 0) return succes;
					imax0--;
				    }
				    imin0--;
                                    typ = 0;
				}
				else if (dis1 > dis2) 
                                     {
                                     dis = dis2;
				     if (typ == 1) 
                                     {
				         succes=combine_x(imin0,jmax0,imax0,jmax0);
                                         if(succes == 0) return succes;
					 jmin0++;
				      }
				      else 
                                      {
                                        if (dis2 == BIG)
                                        {
                                          succes = vertex(imax0-1,jmax0);
                                          jmax0++;
                                          return succes;
                                        }
                                        else
                                        {
				          succes=combine_x(imin0,jmax0,imin0+1,jmax0);
                                          if(succes == 0) return;
					  imax0--;
                                          }
				         }
				         jmax0++;
                                         typ=1;
				}
                        }
                        else
                        {
                           dis = dis1;
                           succes = vertex(imin0, jmax0);
                           if (succes == 0) return succes;
                           if (typ == 1)
                           {
                              jmin0++;
                           }
                              else
                           {
                              imax0--;
                           }
                           imin0--;
                           jmax0++; 
			}
		}
		break;
	     }
	}
	return succes;
}

void go_on()

{

       *(px + point) = (cellhd.west + cellhd.ew_res/2.) + xp * (cellhd.ew_res); 
      *(py + point) = (cellhd.south + cellhd.ns_res/2.) + yp * (cellhd.ew_res); 
	point++;
	return; 
}


void calculate()
{
	int    i, j, done, k, ilg,l, kac, lac;
        int    bp = 0;
	int    bpnext = 0;
	float length;
	double w, pxx, pyy,sx,sy;
        char   msg[100];
       struct line_pnts *Points;

	w = 3.*sqrt((double)(n * n + m * m));
	k = (int) w;
	fprintf(stderr, "\n\nWorking...");
        px = (double *)G_malloc(k * sizeof(double)); 
	py = (double *)G_malloc(k * sizeof(double));

        if (lgout != NULL)
        {
            lg = (float **)G_malloc(sizeof(float)*(m));
            for(l=0;l<m;l++) 
            {
                 lg[l]   = (float*)G_malloc(sizeof(float)*(n));
            }
         }

        if (dsout != NULL)
        {
            density = (float **)G_malloc(sizeof(float)*(m));
            for( l=0; l<m; l++) 
            {
              density[l] = (float*)G_malloc(sizeof(float)*(n));
            }

            for (j = 0; j < m; j++)
            { 
              for (i = 0; i < n; i++)
              density[j][i] = 0.;
            }
        }
        if (flout != NULL) 
        {
           if (0 > Vect_open_new(&Map, flout))
              G_fatal_error("Cannot open vector file");
	      Head.organization[0] = 0;
	      Head.date[0] = 0;
              Head.your_name[0] = 0;
	      sprintf(mapname,"from raster map %s",elevin);
              strncpy (Head.map_name, mapname, 39); /* uses GISLIB strncpy() */
              Head.source_date[0] = 0;
              Head.orig_scale = 0;
              Head.line_3[0] = 0;
              Head.plani_zone = cellhd.zone;
              Head.digit_thresh = 0;
              Head.map_thresh = 0;
              Head.W = cellhd.west;
              Head.E = cellhd.east;
              Head.S = cellhd.south;
              Head.N = cellhd.north;

              Vect_copy_head_data (&Head, &(Map.head));
              Points = Vect_new_line_struct();
         }

    for (j = 0; j < m; j++) 
    {
        G_percent(j,m,5);
	for (i = 0; i < n; i++) 
        {
	imin0 = i - 1;
        imax0 = i + 1;
       	jmin0 = j - 1;
        jmax0 = j + 1;
        xp = xmin + (double)i * stepx;
        yp = ymin + (double)j * stepy;
        angle = (double) o[j][i] * DEG;

    /*    if (angle == 0 )fprintf(stdout, "ZERO aspekt (not changed to 365!) op %lf, j %d,i%d o%d \n",angle,j,i,o[j][i]);*/

        zp = (double) z[j][i];

        if (barierin != NULL)
        bp = (int) BM_get (bitbar, i, j );

        if(bp != 0 && dsout != NULL) density[j][i]=0;

        /*put undefined to length here and zero when zp != Undef when
          available*/

        if ( lgout != NULL ) lg[j][i] = 0;

        if (zp != UNDEFZ && bp == 0)
        {
           point = 0;
           bpnext = 0;   
	   if (angle != UNDEF && bp == 0)
           {
          	go_on();
                func = mesh_vertex; 
		while (((*func)() == 1) && (bpnext == 0)) 
                {
                         if ( barierin != NULL)
                         {
        /*                    kac = (int) G_easting_to_col (xp, cellhd);
                            lac = (int) G_northing_to_row (yp, cellhd);*/
                                sx = fabs(xp / stepx);
                                sy = fabs(yp / stepy);
                            /*    sx += EPS; sy += EPS;*/
                               kac = (int)sx; lac = (int)sy;


                            bpnext = (int) BM_get( bitbar, kac, lac);
	/*	fprintf(stdout, "i,j,kac,lac,bpnext %d %d %d %d %d\n", i,j,kac,lac,bpnext);*/
                        if ( dsout != NULL && bpnext != 0) density[lac][kac]=0;
                          }
	                 go_on();
            if (k < point) 
              {
              sprintf (msg, "not enough memory allocated for px,py");
              G_fatal_error (msg);
              } 
                      }
                 if (flout != NULL && j%skip == 0 && i%skip == 0)
                 {
                    Vect_copy_xy_to_pnts(Points,px,py,point);
                    Vect_write_line(&Map,LINE,Points);
                 }

                 if (lgout != NULL)
                 {
                     length = 0.;
                     for (ilg=0; ilg<point-1; ilg++)
                     {
                           pxx = px[ilg+1] - px[ilg];
                           pyy = py[ilg+1] - py[ilg];
                           length = length +  sqrt(pxx*pxx + pyy*pyy);
                      }
                      lg[j][i] = length;

                 }
	       }
             }
         }
    }

 free(px); free(py);  
 if ( flout != NULL)
 {
            Vect_destroy_line_struct(Points);
	    Vect_close (&Map);
 }
}


