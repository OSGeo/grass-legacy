/*******************************************************************************
This program (r.sun) was writen by Jaro Hofierka in Summer 1993
Copyright Jaro Hofierka, Dept. of Cartography, Comenius University, Bratislava, Slovakia
          Helena Mitasova, USACERL, Champaign, Illinois, USA 
*******************************************************************************/

#define M2_PI    2 * M_PI
#define RAD      360. / (2. * M_PI)
#define DEG      (2. * M_PI)/360.
#define UNDEF    365. * DEG           /*change to undefined when available*/
#define UNDEFZ   0.                   /*change to undefined when available*/
#define SKIP    "1"
#define BIG      1.e20
#define IBIG     32767
#define EPS      1.e-4
#define ZMULT   "1.0"
#define TIME    "12.0"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <malloc.h>
#include "gis.h"
#include "Vect.h"
#include "dig_atts.h"

FILE *felevin,*faspin, *fslopein, *fincidout, *fenergyout;

char *elevin;
char *aspin;
char *slopein;
char *incidout = NULL;
char *energyout = NULL;
char *mapset = NULL;
char *per;
char *shade;
char mapname[1024];

struct Cell_head cellhd;
struct Map_info Map;
struct dig_head Head;

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
void   com_par();
double com_sol_const();
double com_declin();
double lumcline2();
double joules2();

int (*func)();

int     shd, typ, n, m, point, ip, jp;
int     **z, **o, **s;
double  stepx, stepy, xp, yp, op, zp, dp, angle, z1, xp0, yp0;
int     **lumcl, **energy;
double  xmin, xmax, ymin, ymax;
int     imin0, imax0, jmin0, jmax0, in1, ix1, jn1, jx1;
int     d, dej;
double  length, zmult;
double lum_C11, lum_C13, lum_C22, lum_C31, lum_C33, lum_Lx, lum_Ly, lum_Lz,
		lum_R, lum_m, lum_p, sunrise_time, sunset_time, h0, A0;
double latitude, lum_time, declination, slope, aspect;
double longit_l, latid_l, cos_u, cos_v, sin_u, sin_v;
double sin_phi_l, tan_lam_l, lum_C31_l, lum_C33_l;


main(argc, argv)
int argc;
char *argv[];


{
 double amax1();
 double amin1();
 int    max();
 int    min();

 struct
  {
  struct Option *elevin, *zmult, *aspin, *slopein, *incidout, *energyout, *latitude, *dej, *lum_time;
	  } parm;

 struct
  {     struct Flag   *shade;
        } flag;

	  G_gisinit (argv[0]);

		if(G_get_set_window(&cellhd)==-1) exit(0);
		/*ew_res = cellhd.ew_res;*/
/*		stepx = cellhd.ew_res/cellhd.ew_res;*/
		stepx = cellhd.ew_res;
		/*ns_res = cellhd.ns_res;*/
/*		stepy = cellhd.ns_res/cellhd.ew_res;*/
		stepy = cellhd.ns_res;
		n/*n_cols*/ = cellhd.cols;
		m/*n_rows*/ = cellhd.rows;
		/*x_orig = cellhd.west;*/
	/*	xmin = 0.;*/
		xmin = cellhd.west;
		/*y_orig = cellhd.south;*/
/*		ymin = 0.;*/
		ymin = cellhd.south;
		xmax = cellhd.east;
		ymax = cellhd.north;
		printf("\nWARNING !!!!!!! THIS PROGRAM IS UNDER DEVELOPMENT!!!!!");
		printf("PROGRAM HAS BUGS SO BE CAREFUL");
		printf("DON'T DISTRIBUTE, IT HOPEFULLY WILL BE AVAILABLE WHEN FINISHED AND TESTED\n");

	  parm.elevin = G_define_option();
	  parm.elevin->key = "elevin";
	  parm.elevin->type = TYPE_STRING;
	  parm.elevin->required = YES;
	  parm.elevin->gisprompt = "old,cell,raster";
	  parm.elevin->description = "Name of the elevation raster file";

          parm.zmult = G_define_option();
          parm.zmult->key = "zmult";
          parm.zmult->type = TYPE_DOUBLE;
          parm.zmult->answer = ZMULT;
          parm.zmult->required = NO;
          parm.zmult->description = "Mult. factor for elevations";

	  parm.aspin = G_define_option();
	  parm.aspin->key = "aspin";
	  parm.aspin->type = TYPE_STRING;
	  parm.aspin->required = YES;
	  parm.aspin->gisprompt = "old,cell,raster";
	  parm.aspin->description = "Name of the aspect raster file";

	  parm.slopein = G_define_option();
	  parm.slopein->key = "slopein";
	  parm.slopein->type = TYPE_STRING;
	  parm.slopein->required = YES;
	  parm.slopein->gisprompt = "old,cell,raster";
	  parm.slopein->description = "Name of the slope raster file";

	  parm.incidout = G_define_option();
	  parm.incidout->key = "incidout";
	  parm.incidout->type = TYPE_STRING;
	  parm.incidout->required = NO;
          parm.incidout->gisprompt = "new,cell,raster";
	  parm.incidout->description = "Output incidence angle file (raster)";


	  parm.energyout = G_define_option();
	  parm.energyout->key = "energyout";
	  parm.energyout->type = TYPE_STRING;
	  parm.energyout->required = NO;
          parm.energyout->gisprompt = "new,cell,raster";
	  parm.energyout->description = "Output energy file (raster)-only when incidout is NOT def. !!";

	  parm.latitude = G_define_option();
	  parm.latitude->key = "latitude";
	  parm.latitude->type = TYPE_DOUBLE;
	  parm.latitude->required = YES;
	  parm.latitude->description = "Latitude of given region";

	  parm.dej = G_define_option();
	  parm.dej->key = "dej";
	  parm.dej->type = TYPE_INTEGER;
	  parm.dej->required = YES;
	  parm.dej->description = "No. of day";

	  parm.lum_time = G_define_option();
	  parm.lum_time->key = "lum_time";
	  parm.lum_time->type = TYPE_DOUBLE;
	  parm.lum_time->answer = TIME;
	  parm.lum_time->required = NO;
	  parm.lum_time->description = "Desired time - only when energyout is NOT defined !!!";

          flag.shade = G_define_flag();
          flag.shade->key = 's';
          flag.shade->description = "Do you want to incorporate the shading effect of terrain? ";

		
		if(G_parser(argc,argv)) exit(1);

		shd=flag.shade->answer;

          elevin = parm.elevin->answer;
          aspin = parm.aspin->answer; 
          slopein = parm.slopein->answer;
          incidout = parm.incidout->answer;
          energyout = parm.energyout->answer;

                sscanf(parm.zmult->answer, "%lf", &zmult);
                sscanf(parm.latitude->answer, "%lf", &latitude);
                sscanf(parm.dej->answer, "%d", &dej);
                sscanf(parm.lum_time->answer, "%lf", &lum_time);

/*printf("**** %lf\n", zmult);*/

                declination = com_declin(dej);

			latitude = - M_PI * latitude / 180;
if(incidout != NULL)
  {

			lum_time = (lum_time - 12) * 15;
			/* converting to degrees */
			/* Jenco (12-lum_time) * 15 */
			if (lum_time < 0)
			lum_time += 360;
			lum_time = M_PI * lum_time / 180;
			/* conv. to radians */
}

/**********end of parser - ******************************/

	   INPUT();
	   calculate();
	   OUTGR();

   return 1;
}


INPUT()

{
	CELL    *cell1, *cell2;
	CELL    *cell3;
	int     fd1, fd2, fd3, row, row_rev;
	int     l,i,j;

	cell1=G_allocate_cell_buf();
	cell2=G_allocate_cell_buf();
	cell3=G_allocate_cell_buf();

	z = (int **)malloc(sizeof(int)*(m));
	o = (int **)malloc(sizeof(int)*(m));
	s = (int **)malloc(sizeof(int)*(m));

  for(l=0;l<m;l++)
   {
	z[l]   = (int*)malloc(sizeof(int)*(n));
	o[l]   = (int*)malloc(sizeof(int)*(n));
	s[l]   = (int*)malloc(sizeof(int)*(n));

	}

  if((mapset=G_find_cell(elevin,""))==NULL)
  printf("cell file not found\n");

  if((mapset=G_find_cell(aspin,""))==NULL)
  printf("cell file not found\n");

  if((mapset=G_find_cell(slopein,""))==NULL)
  printf("cell file not found\n");

  fd1 = G_open_cell_old(elevin,mapset);
  fd2 = G_open_cell_old(aspin,mapset);
  fd3 = G_open_cell_old(slopein,mapset);

  for (row=0; row<m; row++)
  {
	  G_get_map_row(fd1,cell1,row);
	  G_get_map_row(fd2,cell2,row);
	  G_get_map_row(fd3,cell3,row);

	for (j=0; j<n; j++)
	{
	   row_rev = m - row - 1;
	   z[row_rev][j] = (int ) cell1[j];
	   o[row_rev][j] = (int ) cell2[j];
	   s[row_rev][j] = (int ) cell3[j];

	 }
   }
  G_close_cell(fd1);
  G_close_cell(fd2);
  G_close_cell(fd3);


/*******transformation of angles from 0 to east counterclock
		to 0 to north clocwise, for ori=0 upslope flowlines
		turn the orientation 2*M_PI ************/

  /*for (i = 0; i < m; ++i)*/
  for (i = 0; i < m; i++)
  {
	  for (j = 0; j < n; j++)
	  {
		if ( o[i][j] == 0 )
			 o[i][j] = 365;
		else
		   if( o[i][j] < 90 )
			   o[i][j] = 90 - o[i][j];
		   else
			  o[i][j] = 450  - o[i][j];
	  /*   printf("o,z = %d  %d i,j, %d %d \n", o[i][j],z[i][j],i,j);*/

	  }
   }
}

int     OUTGR()
{
   CELL     *cell4,*cell5,data1,data2;
   int      cf4, cf5;
   int      i,iarc,j;
   int       dsmax=0,dsmin=0;
   char     msg[100];
   struct Colors colors;
   struct History hist;


		if (incidout != NULL)
		{
		cell4 = G_allocate_cell_buf();
		cf4 = G_open_cell_new (incidout);
		if (cf4 < 0)
		{
			sprintf (msg, "unable to create raster map %s", incidout);
			G_fatal_error (msg);
			exit(1);
		}
		}

		if (energyout != NULL)
		{
		cell5 = G_allocate_cell_buf();
		cf5 = G_open_cell_new (energyout);
		if (cf5 < 0)
		{
			sprintf (msg, "unable to create raster map %s", energyout);
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
		if(incidout!=NULL)
		{
		  for(j=0;j<n;j++)
		  {
			cell4[j]=(CELL) lumcl[i][j];
		  }
		  G_put_map_row (cf4, cell4);
		}

		if(energyout!=NULL)
		{
		  for(j=0;j<n;j++)
		  {
/*      printf("energy, i, j %d %d %d\n", energy[i][j], i, j);*/
			cell5[j]=(CELL)energy[i][j];
			dsmax = max(dsmax, energy[i][j]);
		  }
		  G_put_map_row (cf5, cell5);
		}
	  }
		if(incidout!=NULL)
	  G_close_cell (cf4);
		if(energyout!=NULL)
	  G_close_cell (cf5);

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
/********************END OF GRASS ADDITIONS - INPUT, FUNCTIONS******/
void com_par()
{
	double old_time, time, aa, pom, xpom, ypom;

		time = lum_time * RAD;
		time = time / 15 + 12;
/*		printf ("\nlatitude= %.2f declination= %.2f time= %dh %dmin:\n"
			"--------------------------------------------------\n",
				-latitude*RAD, -declination*RAD, (int)time,
				(int)(60 * (time - (int)time)));*/

			old_time = lum_time;

			lum_C11 = sin(latitude) * cos(declination);
			lum_C13 = -cos(latitude) * sin(declination);
			lum_C22 = cos(declination);
			lum_C31 = cos(latitude) * cos(declination);
			lum_C33 = sin(latitude) * sin(declination);

			lum_Lx = -lum_C22 * sin(lum_time);    /* skontroluj toto pre Maros. sustavu*/
			lum_Ly = lum_C11 * cos(lum_time) + lum_C13;
			lum_Lz = lum_C31 * cos(lum_time) + lum_C33;

			if (fabs(lum_C31) >= EPS) {
				  pom = -lum_C33 / lum_C31;
				  if (fabs(pom) <= 1) {
					pom = acos(pom);
					pom = (pom * 180) / M_PI;
					sunrise_time = (90 - pom) / 15 + 6;
					sunset_time = (pom - 90) / 15 + 18;
					if (!((old_time >= sunrise_time) &&
						   (old_time <= sunset_time))) {
/*		  printf("\n\tsunrise= %dh %dmin   ",  (int)sunrise_time,
		  (int)(60 * (sunrise_time - (int)sunrise_time)));
		  printf("sunset= %dh %dmin\n", (int)sunset_time,
		  (int)(60 * (sunset_time - (int)sunset_time)));*/

/*                              request -= LUMCLINE;*/
						}
			  }
			  else {
			if (pom < 0) {
	  printf("\tSun is ABOVE area all hours in the day\n");
			  sunrise_time = 0;
			  sunset_time = 24;
			  if (fabs(pom) - 1 <= EPS)
			printf("\texcept at midnight is sun ON THE HORIZONT\n");
				}
				else {
		  printf("\tThe sun is BELOW the horizon during the whole day\n");
			  if (fabs(pom) - 1 <= EPS) {
			printf("\texcept at noon is sun ON HORIZONT\n");
			sunrise_time = 12;
			sunset_time = 12;
/*                       if (old_time != sunrise_time)
na toto sa este riadne pozri		  request -= LUMCLINE;*/
							  }
/*				  else
			   request -= LUMCLINE;*/
						}
				  }
			}
			else {
			  if (fabs(lum_Lz) >= EPS) {
				if (lum_Lz > 0) {
			  printf("\tSun is ABOVE area during the whole day\n");
				  sunrise_time = 0;
				  sunset_time = 24;
					}
					else {
			  printf("\tSun is BELLOW area during the whole day");
			 /*	  request -= LUMCLINE;*/
					}
				  }
				  else {
			printf("\tThe Sun is ON HORIZON during the whole day\n");
						sunrise_time = 0;
						sunset_time = 24;
				  }
			}

/*	  printf("\n\tsunrise= %dh %dmin   ",      (int)sunrise_time,
	  (int)(60 * (sunrise_time - (int)sunrise_time)));
	  printf("sunset= %dh %dmin\n", (int)sunset_time,
	  (int)(60 * (sunset_time - (int)sunset_time)));*/


		  h0 = asin (lum_Lz);   /* vertical angle of the sun */
		/*  h0 *= RAD;*/

/*		  printf ("\n\th0 = %.4f,  ", h0*RAD);*/

		  xpom = lum_Lx * lum_Lx;
		  ypom = lum_Ly * lum_Ly;
		  pom = sqrt(xpom + ypom);

		  if (fabs(pom) > EPS) {
			A0 = lum_Ly / pom;
		A0 = acos(A0);     /* horiz. angle of the Sun */
/*			A0 *= RAD;*/
			if (lum_Lx < 0)
			  A0 = M2_PI - A0;
/*			printf ("A0 = %.4f\n", A0*RAD);*/
			  }
			  else {
			A0 = UNDEF;
			if (h0 > 0)
			  printf ("A0 = Zenit\n");
			else
			  printf ("A0 = Nadir\n");
				  }
				  angle = A0;
			}
/**********************************************************/

double lumcline2()
{
	  double q1, s;
	  int r;

	  com_par();
			  point = 0; length = 0;
			  func = mesh_vertex;
		A0 += 0.5 * M2_PI; /* do opacneho smeru !!!!*/

		if (shd == 1) {
			  while((r = (*func)()) == 1)
			   {
			  /*		  go_on();*/
				  }
			}
			  imin0 = in1; imax0 = ix1; jmin0 = jn1; jmax0 = jx1;
                          xp = xp0; yp = yp0;

			  if (r == 2) return 0.;
	if ((zp != UNDEFZ) || (aspect != UNDEF)) {
	  s = lum_C31_l * cos(-lum_time - longit_l) + lum_C33_l; /* Jenco */
/*	  printf("toto je sin dopadu luca %lf \n", s);*/
	}
		else
		{
		s = lum_Lz;
/*        printf("toto je sin dopadu luca %lf \n", s);*/
		}
	  if (s < 0) return 0.;
	  return (s);
}

double joules2()
{

	  int r;
	  double s, s0, pom1, pom2, dfr1, dfr2;
	  double q1, c;
	  int i, i1, i2;

        c = com_sol_const();

                        s = 0.;

/*                        declination = com_declin(dej);*/

			lum_time = 12.;
			lum_time = (lum_time - 12) * 15;
			 /* prerata na stupne */
			/* Jenco (12-lum_time) * 15 */
			if (lum_time < 0)
			lum_time += 360;
			lum_time = M_PI * lum_time / 180;
			/* prerata na radiany */
			com_par();

          i1 = (int)sunrise_time;
          dfr1 = sunrise_time - i1;
          if (dfr1 > 0.) {
          i1 = i1 + 1;
          dfr1 = 1 - dfr1;
          }
          i2 = (int)sunset_time;
          dfr2 = sunset_time - i2;

                        lum_time = (sunrise_time - 12) * 15;
                        if (lum_time < 0)
                        lum_time += 360;
                        lum_time = M_PI * lum_time / 180;
/*                      printf(" radiany %f", lum_time);*/

                  s0 = lumcline2();
      /*  printf("toto je sin dopadu luca %lf \n", s0);*/

                        if (s0 > 0.) {

        lum_R = (6371 + (zp / (1000 * zmult))) / (10 - (zp / (1000 * zmult))); /* pozri na jedno
tky = metre apod. */

                        pom1 = - lum_R * (lum_C31 * cos(lum_time) + lum_C33);
                        pom2 = pom1 * pom1 + 2. * lum_R + 1.;
                        lum_m = pom1 + sqrt(pom2);

                                  s0 *= pow(1., lum_m);
                                  s0 *= (c * dfr1);
                                  }
          else {
                          s0 = 0.;
                          }
			s += s0;

	  for (i = i1; i <= i2; i++) {

                        lum_time = (i - 12) * 15;
                   /*     if (lum_time < 0)
                        lum_time += 360;*/
                        lum_time = M_PI * lum_time / 180;
/*                      printf(" radiany %f", lum_time);*/



                  s0 = lumcline2();
      /*  printf("toto je sin dopadu luca %lf \n", s0);*/

                        if (s0 > 0.) {

                        pom1 = - lum_R * (lum_C31 * cos(lum_time) + lum_C33);
                        pom2 = pom1 * pom1 + 2 * lum_R + 1.;
                        lum_m = pom1 + sqrt(pom2);

                                  s0 *= pow(1., lum_m);
                                  s0 *= c;
                                  }
          else {
                          s0 = 0.;
                          }
                                  s += s0;
                        }

                        lum_time = (sunset_time - 12) * 15;
                        if (lum_time < 0)
                        lum_time += 360;
                        lum_time = M_PI * lum_time / 180;

                  s0 = lumcline2();
     /*   printf("toto je sin dopadu luca %lf \n", s0);*/

                        if (s0 > 0.) {

                        pom1 = - lum_R * (lum_C31 * cos(lum_time) + lum_C33);
                        pom2 = pom1 * pom1 + 2 * lum_R + 1.;
                        lum_m = pom1 + sqrt(pom2);

                                  s0 *= pow(1., lum_m);
                                  s0 *= (c * dfr2);
                                  }
          else {
                          s0 = 0.;
                          }

                                  s += s0;


	  return (s);
}
/*//////////////////////////////////////////////////////////////////////*/

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
	  /*  function calculates the distance of oriented line from origin */
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
	  if (fabs(d) < EPS) {
		  *xl = BIG;
			*yl = BIG;
			*dis = BIG;
			return;
	  }

	  dx = x1 * sin(angle);
	  dy = - dp + x1 * cos(angle);
	  *xl = dx / d;
		*yl = dy / d;

	  pomx = *xl - xp; pomx *= pomx;
	  pomy = *yl - yp; pomy *= pomy;
		pom = pomx + pomy;
		pom = sqrt((double)pom);
	  *dis = pom;
}

void new_point_y (c, xl, yl, dis)
double *xl,*yl,*dis;
int c;
{
	double y1, d, dx, dy, pom, pomx, pomy;

		y1 = - (ymin + c * stepy);

	  d = - cos(angle);
	  if (fabs(d) < EPS) {
		  *xl = BIG;
			*yl = BIG;
			*dis = BIG;
			return;
	  }

	  dx = y1 * sin(angle) + dp;
	  dy = y1 * cos(angle);
		*xl = dx / d;
		*yl = dy / d;

	  pomx = *xl - xp; pomx *= pomx;
	  pomy = *yl - yp; pomy *= pomy;
	  pom = pomx + pomy;
	  pom = sqrt((double)pom);
	  *dis = pom;
}

void which_one ( dis1, dis2, x1,  y1, x2, y2)
double dis1,dis2,x1,y1,x2,y2;
{
	  /*  function decides which point is closer to previous point of flowline */
	  if (dis1 < dis2) {
			xp = x1; yp = y1;
	  }
		else {
				  xp = x2; yp = y2;
	  }
}

int combine_x (k1, l1, k2, l2)
int k1,l1,k2,l2;
{
	double d1, d2, a1, a2, c1, c2, z1;

		if ((l1 < 0) || (l1 > m - 1) ||
				  (l2 < 0) || (l2 > m - 1) ||
				  (k1 < 0) || (k1 > n - 1) ||
				  (k2 < 0) || (k2 > n - 1))
				  return (0);


	d1 = xmin + (double)k1 * stepx;
	d2 = xmin + (double)k2 * stepx;
		a1 = A0; a2 = A0;
		if (!((a1 == UNDEF) || (a2 == UNDEF))) {
						  c1 = (xp - d1) / (d2 - d1);
						  c2 = 1 - c1;
						  angle = A0;

				  a1 = z[l1][k1]; a2 = z[l2][k2];
				  z1 = c1 * a2 + c2 * a1 ;

						  zp = z1;

				  func = mesh_line;
						  return (1);
			}
			else
				  return (0);
}

int combine_y(k1, l1, k2, l2)
int k1,l1,k2,l2;
{
	double d1, d2, a1, a2, c1, c2, z1;

		if ((l1 < 0) || (l1 > m - 1) ||
				  (l2 < 0) || (l2 > m - 1) ||
						  (k1 < 0) || (k1 > n - 1) ||
				  (k2 < 0) || (k2 > n - 1))
				  return (0);

			d1 = ymin + (double)l1 * stepy;
			d2 = ymin + (double)l2 * stepy;
			a1 = A0; a2 = A0;
			if (!((a1 == UNDEF) || (a2 == UNDEF))) {
				  c1 = (yp - d1) / (d2 - d1);
						  c2 = 1 - c1;
						  angle = A0;

				  a1 = z[l1][k1]; a2 = z[l2][k2];
				  z1 = c1 * a2 + c2 * a1 ;

						  zp = z1;

				  func = mesh_line;
				  return (1);
		}
		else
				  return (0);
}

int vertex (k1, l1)
int k1,l1 /*,k2,l2*/;
{
	double z1;

			if ((l1 < 0) || (l1 > m - 1) ||
						  (k1 < 0) || (k1 > n - 1))
				  return (0);

			angle = A0;
			z1 = z[l1][k1];

		if (angle != UNDEF) {

				  zp = z1;

				  func = mesh_vertex;
				  return (1);
		}
			else {
/*                  func = NULL;*/
						  return (0);
		}
}


int mesh_vertex()

{
	double x1, x2, y1, y2, dis, dis1, dis2, z2;
	int q;
	int succes = 0;
	int cond1, cond2;

			dis = 0;
/*      func = NULL;*/
			q = quadrant ();
			dp = coef_of_line ();
			switch (q) {
			  case 1 : {
			  cond1 = (imax0 == n);
			  cond2 = (jmax0 == m);
			  cond1 = cond1 || cond2;
			  if (!cond1) {
			  new_point_x (imax0, &x1, &y1, &dis1);
			  new_point_y (jmax0, &x2, &y2, &dis2);
			  which_one (dis1, dis2, x1, y1, x2, y2);
			  if (dis1 == BIG) {
/* Flowline goes to up vertex*/
/*			  printf("*");*/
			  succes = vertex (imax0 - 1, jmax0);
			  if (succes == 0)  return succes;
			  jmin0++; jmax0++;
			  dis = dis2;
				  }
			  else if (dis2 == BIG) {
/* Flowline goes to right vertex*/
					  printf("*");
						  succes = vertex (imax0, jmax0 - 1);
						if (succes == 0)  return succes;
					  imin0++; imax0++;
					  dis = dis1;
						  }
							  else {
					  if (fabs(dis1 - dis2) > EPS) {
							if (dis1 < dis2) {
						succes = combine_y (imax0, jmax0, imax0, jmax0 - 1);
						if (succes == 0)  return succes;
							imax0++; typ = 0;
							dis = dis1;
									}
						else if (dis1 > dis2) {
						succes = combine_x (imax0, jmax0, imax0 - 1, jmax0);
							if (succes == 0)  return succes;
								jmax0++; typ = 1;
										dis = dis2;
										}
								  }
											  else {
/* !!! flowline goes to diagonal vertex */
					succes = vertex (imax0, jmax0);
					if (succes == 0)  return succes;
							imax0++; jmax0++;
								dis = dis1;
									  }
							  imin0++;
					  jmin0++;
				  }
			  }
		  break;
	  }
		  case 2 : {
		  cond1 = (imax0 == n);
		  cond2 = (jmin0 == -1);
		  cond1 = cond1 || cond2;
			  if (!cond1) {
		  new_point_x (imax0, &x1, &y1, &dis1);
		  new_point_y (jmin0, &x2, &y2, &dis2);
		  which_one (dis1, dis2, x1, y1, x2, y2);
			  if (dis1 == BIG) {
/* Flowline goes to down vertex*/
/*			  printf("*");*/
		  succes = vertex (imax0 - 1, jmin0);
		if (succes == 0)  return succes;
		  jmin0--; jmax0--;
			  dis = dis2;
				 }
		  else {
			  if (fabs(dis1 - dis2) > EPS) {
			  if (dis1 < dis2) {
				succes = combine_y (imax0, jmin0, imax0, jmin0 + 1);
				if (succes == 0)  return succes;
				imax0++; typ = 0;
				dis = dis1;
					}
				else if (dis1 > dis2) {
			succes = combine_x (imax0, jmin0, imax0 - 1, jmin0);
				if (succes == 0)  return succes;
					jmin0--; typ = 1;
					dis = dis2;
					}
					  }
				  else {
/* !!! flowline goes to diagonal vertex */
				succes = vertex (imax0, jmin0);
				if (succes == 0)  return succes;
				imax0++; jmin0--;
				  dis = dis1;
					  }
				imin0++;
				jmax0--;
					}
				  }
						  break;
				  }
				  case 3 : {
					  cond1 = (imin0 == -1);
						  cond2 = (jmin0 == -1);
						  cond1 = cond1 || cond2;
						  if (!cond1) {
					  new_point_x (imin0, &x1, &y1, &dis1);
				  new_point_y (jmin0, &x2, &y2, &dis2);
					  which_one (dis1, dis2, x1, y1, x2, y2);
						  if (dis2 == BIG) {
/* Flowline goes to left vertex*/
					/*	  printf("*");*/
						  succes = vertex (imin0, jmax0 - 1);
						if (succes == 0)  return succes;
						  imin0--; imax0--;
						  dis = dis1;
							  }
						  else {
					  if (fabs(dis1 - dis2) > EPS) {
						if (dis1 < dis2) {
						succes = combine_y (imin0, jmin0, imin0, jmin0 + 1);
						if (succes == 0)  return succes;
						imin0--; typ = 0;
						  dis = dis1;
						  }
						  else if (dis1 > dis2) {
					  succes = combine_x (imin0, jmin0, imin0 + 1, jmin0);
						if (succes == 0)  return succes;
							jmin0--; typ = 1;
							dis = dis2;
							}
							  }
							  else {
/* !!! flowline goes to diagonal vertex */
						succes = vertex (imin0, jmin0);
						if (succes == 0)  return succes;
							imin0--; jmin0--;
							dis = dis1;
							  }
							  imax0--;
							  jmax0--;
						  }
							  }
									  break;
						  }
					  case 4 : {
						  cond1 = (imin0 == -1);
						  cond2 = (jmax0 == m);
						  cond1 = cond1 || cond2;
						  if (!cond1) {
							  new_point_x (imin0, &x1, &y1, &dis1);
							  new_point_y (jmax0, &x2, &y2, &dis2);
							  which_one (dis1, dis2, x1, y1, x2, y2);
					  if (dis1 == BIG) {
/* Flowline goes to up left vertex*/
						 /*	  printf("*");*/
						  succes = vertex (imax0 - 1, jmax0);
						if (succes == 0)  return succes;
						  jmin0++; jmax0++;
							dis = dis2;
							  }
							  else {
					  if (fabs(dis1 - dis2) > EPS) {
						if (dis1 < dis2) {
					succes = combine_y (imin0, jmax0, imin0, jmax0 - 1);
					if (succes == 0)  return succes;
						imin0--; typ = 0;
						dis = dis1;
							}
					else if (dis1 > dis2) {
				succes = combine_x (imin0, jmax0, imin0 + 1, jmax0);
				   if (succes == 0)  return succes;
					  jmax0++; typ = 1;
					dis = dis2;
						  }
				}
					  else {
/* !!! flowline goes to diagonal vertex */
			succes = vertex (imin0, jmax0);
			if (succes == 0)  return succes;
			imin0--; jmax0++;
			dis = dis1;
			 }
			imax0--;
			jmin0++;
			   }
			}
			 break;
				}
			}
		length += dis;
		z2 = z1 + zmult * length * tan(h0);
		/* zase ta 100 len pre vysky v cm (x,y v metroch) - uprav*/
	/*	printf("z2 zp %lf %lf\n", z2, zp);*/
		if (z2 < zp) succes = 2;

		return succes;
}

int mesh_line()

{
	double x1, x2, y1, y2, dis, dis1, dis2, z2;
	int q;
	int succes = 0;
	int cond1, cond2;

			dis = 0;
/*            func = NULL;*/
			q = quadrant ();
			dp = coef_of_line ();

			switch (q) {
			  case 1 : {
			  cond1 = (imax0 > (n - 1));
			  cond2 = (jmax0 > (m - 1));
			  cond1 = cond1 || cond2;
			  if (!cond1) {
			  new_point_x (imax0, &x1, &y1, &dis1);
			  new_point_y (jmax0, &x2, &y2, &dis2);
			  which_one (dis1, dis2, x1, y1, x2, y2);
				  if (fabs(dis1 - dis2) > EPS) {
					  if (dis1 < dis2) {
						dis = dis1;
						if (typ == 1) {
						if (dis2 == BIG) {
/* Flowline goes to right vertex*/
							printf("+");
							succes = vertex (imax0, jmax0 - 1);
/* tu zarataj spadnicu do lavej cell */
								imax0++;
							return succes;
							  }
					else {
			succes = combine_y (imax0, jmin0 + 1, imax0, jmax0);
			  if (succes == 0)  return succes;
				jmin0++;
					 }
				 }
/* typ = 0*/
					else {
			succes = combine_y (imax0, jmin0, imax0, jmax0);
			if (succes == 0)  return succes;
				imin0++;
				}
			imax0++; typ = 0;
				  }
			  else if (dis1 > dis2) {
			dis = dis2;
			if (typ == 1) {
			succes = combine_x (imin0, jmax0, imax0, jmax0);
			if (succes == 0)  return succes;
				jmin0++;
				}
			else {
				if (dis1 == BIG) {
/* Flowline goes to up vertex*/
			printf("+");
			  succes = vertex (imax0 - 1, jmax0);
				jmax0++;
				return succes;
				 }
			else {
		succes = combine_x (imin0 + 1, jmax0, imax0, jmax0);
	  if (succes == 0)  return succes;
			imin0++;
			}
		}
			jmax0++; typ = 1;
			  }
		  }
			  else {
/* !!! flowline goes to diagonal vertex */
		  dis = dis1;
		  succes = vertex (imax0, jmax0);
		  if (succes == 0)  return succes;
		  if (typ == 1) {
				jmin0++;
			   }
		  else {
				imin0++;
					}
		  imax0++; jmax0++;
				  }
			  }
				  break;
		  }
		  case 2 : {
				  cond1 = (imax0 > (n - 1));
				  cond2 = (jmin0 < 0);
				  cond1 = cond1 || cond2;
				  if (!cond1) {
						  new_point_x (imax0, &x1, &y1, &dis1);
						  new_point_y (jmin0, &x2, &y2, &dis2);
						  which_one (dis1, dis2, x1, y1, x2, y2);
						  if (fabs(dis1 - dis2) > EPS) {
							  if (dis1 < dis2) {
									dis = dis1;
								if (typ == 1) {
								if (dis2 == BIG) {
/* Flowline goes to right vertex*/
									printf("+");
							   succes = vertex (imax0, jmax0 - 1);
								imax0++;
								return succes;
								}
							else {
							succes = combine_y (imax0, jmin0, imax0, jmin0 + 1);
							if (succes == 0)  return succes;
								jmax0--;
								}
							}
							else {
					  succes = combine_y (imax0, jmax0, imax0, jmin0);
							imin0++;
							}
						imax0++; typ = 0;
					  }
					  else if (dis1 > dis2) {
						  dis = dis2;
						if (typ == 1) {
			  succes = combine_x (imin0, jmin0, imax0, jmin0);
					if (succes == 0)  return succes;
							jmax0--;
					}
							else {
					if (dis1 == BIG) {
/* Flowline goes to down vertex*/
				printf("+");
				succes = vertex (imax0 - 1, jmin0);
					jmin0--;
						return succes;
					  }
					else {
			succes = combine_x (imin0 + 1, jmin0, imax0, jmin0);
		  if (succes == 0)  return succes;
						imin0++;
						}
						}
				jmin0--; typ = 1;
					}
			  }
				 else {
/* !!! flowline goes to diagonal vertex */
						dis = dis1;
					succes = vertex (imax0, jmin0);
				  if (succes == 0)  return succes;
					if (typ == 1) {
							  jmax0--;
							}
					else {
					  imin0++;
							 }
							  imax0++; jmin0--;
										  }
									  }
									  break;
						  }
		  case 3 : {
				  cond1 = (imin0 < 0);
				  cond2 = (jmin0 < 0);
				  cond1 = cond1 || cond2;
				  if (!cond1) {
					  new_point_x (imin0, &x1, &y1, &dis1);
				  new_point_y (jmin0, &x2, &y2, &dis2);
					  which_one (dis1, dis2, x1, y1, x2, y2);
					  if (fabs(dis1 - dis2) > EPS) {
					  if (dis1 < dis2) {
							   dis = dis1;
								if (typ == 1) {
								if (dis2 == BIG) {
/* Flowline goes to left vertex*/
									printf("+");
							  succes = vertex (imin0, jmax0 - 1);
									imin0--;
								return succes;
									}
								else {
						succes = combine_y (imin0, jmin0, imin0, jmin0 + 1);
						  if (succes == 0)  return succes;
							   jmax0--;
									}
								}
						else {
							succes = combine_y (imin0, jmin0, imin0, jmax0);
							if (succes == 0)  return succes;
							imax0--;
								}
							imin0--; typ = 0;
								  }
						  else if (dis1 > dis2) {
							dis = dis2;
							if (typ == 1) {
							succes = combine_x (imin0, jmin0, imax0, jmin0);
							if (succes == 0)  return succes;
							jmax0--;
								}
							else {
							if (dis1 == BIG) {
/* Flowline goes to down vertex*/
								printf("+");
						  succes = vertex (imax0 - 1, jmin0);
							   jmin0--;
						return succes;
								}
							else {
						succes = combine_x (imin0, jmin0, imin0 + 1, jmin0);
					  if (succes == 0)  return succes;
						   imax0--;
							}
						}
					jmin0--; typ = 1;
							  }
					  }
					  else {
/* !!! flowline goes to diagonal vertex */
						  dis = dis1;
						  succes = vertex (imin0, jmin0);
						if (succes == 0)  return succes;
					  if (typ == 1)
						  jmax0--;
						  else
							imax0--;
						  imin0--; jmin0--;
									  }
									  }
									  break;
						  }
			  case 4 : {
					  cond1 = (imin0 < 0);
					  cond2 = (jmax0 > (m - 1));
					  cond1 = cond1 || cond2;
					  if (!cond1) {
						  new_point_x (imin0, &x1, &y1, &dis1);
						  new_point_y (jmax0, &x2, &y2, &dis2);
						  which_one (dis1, dis2, x1, y1, x2, y2);
						  if (fabs(dis1 - dis2) > EPS) {
								  if (dis1 < dis2) {
									dis = dis1;
									if (typ == 1) {
										if (dis2 == BIG) {
/* Flowline goes to left vertex*/
							  printf("+");
						  succes = vertex (imin0, jmax0 - 1);
						  imin0--;
						  return succes;
							}
					else {
				  succes = combine_y (imin0, jmax0, imin0, jmax0 - 1);
				 if (succes == 0)  return succes;
					 jmin0++;
						 }
					  }
					else {
							 succes = combine_y (imin0, jmax0, imin0, jmin0);
							if (succes == 0)  return succes;
								 imax0--;
									}
							  imin0--; typ = 0;
									  }
							  else if (dis1 > dis2) {
								dis = dis2;
						if (typ == 1) {
						   succes = combine_x (imin0, jmax0, imax0, jmax0);
						   if (succes == 0)  return succes;
							jmin0++;
							}
					else {
					if (dis2 == BIG) {
/* Flowline goes to up vertex*/
							printf("+");
					  succes = vertex (imax0 - 1, jmax0);
						   jmax0++;
					return succes;
						}
				else {
				succes = combine_x (imin0, jmax0, imin0 + 1, jmax0);
			  if (succes == 0)  return succes;
					imax0--;
						}
				   }
			   jmax0++; typ = 1;
					 }
				 }
					else {
/* !!! flowline goes to diagonal vertex */
				  dis = dis1;
				  succes = vertex (imin0, jmax0);
				  if (succes == 0)  return succes;
				   if (typ == 1) {
						   jmin0++;
								 }
				   else {
						  imax0--;
								  }
					  imin0--; jmax0++;
					  }
							 }
						 break;
						  }
			}
			length += dis;
			z2 = z1 + zmult * length * tan(h0); 
/*			printf(" z2 zp %lf %lf\n", z2, zp);*/
/*            uprav to podla jednotiek !!!!!!!!!!!*/
			if (z2 < zp) succes = 2;
			return succes;
}


/*//////////////////////////////////////////////////////////////////////*/

void calculate()
{
			int i, j, l;
/*			double energy;*/
			double lum, q1;

			printf("\n\nWorking... ");

		if (incidout != NULL)
		{
			lumcl = (int **)malloc(sizeof(int)*(m));
			for(l=0;l<m;l++)
			{
				 lumcl[l]   = (int*)malloc(sizeof(int)*(n));
			}
			for (j = 0; j < m; j++)
			{
			  for (i = 0; i < n; i++)
			  lumcl[j][i] = 0;
			}
		 }

		if (energyout != NULL)
		{
			energy = (int **)malloc(sizeof(int)*(m));
			for( l=0; l<m; l++)
			{
			  energy[l] = (int*)malloc(sizeof(int)*(n));
			}

			for (j = 0; j < m; j++)
			{
			  for (i = 0; i < n; i++)
			  energy[j][i] = 0;
			}
		}

			for (j = 0; j < m; j++) {
				  G_percent(j,m,10);
			  for (i = 0; i < n; i++) {
				in1 = imin0 = i - 1; ix1 = imax0 = i + 1;
				jn1 = jmin0 = j - 1; jx1 = jmax0 = j + 1;

				xp0 = xp = xmin + (double)i * stepx;
				yp0 = yp = ymin + (double)j * stepy;
  /*					energy = 0.;*/
				 z1 = zp = (double)z[j][i];
				 aspect = (double)o[j][i] * DEG;
				 slope = (double)s[j][i] * DEG;

				 /* length = undf; */
			  if ((zp != UNDEFZ) || (aspect != UNDEF)) {
          cos_u = cos(M_PI / 2 - slope);
          sin_u = sin(M_PI / 2 - slope);
          cos_v = cos(M_PI / 2 + aspect);
          sin_v = sin(M_PI / 2 + aspect);
        sin_phi_l = -cos(latitude) * cos_u * sin_v + sin(latitude) * sin_u;
        latid_l = asin(sin_phi_l);

        q1 = sin (latitude) * cos_u * sin_v + cos(latitude) * sin_u;
        tan_lam_l = - cos_u * cos_v / q1;
     /*   if (q1 == 0 || (- cos_u * cos_v) == 0) printf("!!!!!!!! cosi je nula %f\n", tan_lam_l);*/
        longit_l = atan (tan_lam_l);
        lum_C31_l = cos(latid_l) * cos(declination);
        lum_C33_l = sin_phi_l * sin(declination);

		
	if (incidout != NULL) {
			lum = lumcline2();
			lum = RAD * asin(lum);
			lumcl[j][i] = (int) lum;
	/*			printf("ukaz sa %d\n", lumcl[j][i]);*/
				}
			if (energyout != NULL) energy[j][i] = (int)joules2();

/*					printf("this is energy %f\n", energy);*/

			} /* undefs*/
		  }
	}
}

double com_sol_const()
{
	double I0;

	/*  v W/(m*m) */

	I0 = 1370.;

	return I0;
}


double com_declin(no_of_day)
int no_of_day;
{
	double decl;

/*	printf(" cislo dna %d\n", no_of_day); */
	decl = 23.45 * sin((0.9856 * (284  + no_of_day) * DEG));
	decl = - decl * DEG;
/*	printf(" deklinacia %lf\n", decl); */

	return (decl);
}
