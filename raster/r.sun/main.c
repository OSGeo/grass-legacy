/*******************************************************************************
r.sun: This program was writen by Jaro Hofierka in Summer 1993 and re-engineered
in 1996-1999. In cooperation with Marcel Suri a new version of r.sun was prepared
using ESRA solar radiation formulas. See manual pages for details.
(C) 2002 Copyright Jaro Hofierka, Gresaka 22, 085 01 Bardejov, Slovakia, 
              and GeoModel, s.r.o., Bratislava, Slovakia
email: hofierka@geomodel.sk,marcel.suri@jrc.it,suri@geomodel.sk
*******************************************************************************/
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
 */

#define M2_PI    2. * M_PI
#define RAD      360. / (2. * M_PI)
#define DEG      (2. * M_PI)/360.
#define UNDEF    0.            /* undefined value for terrain aspect*/
#define UNDEFZ   -9999.        /* undefined value for elevation */
#define SKIP    "1"
#define BIG      1.e20
#define IBIG     32767
#define EPS      1.e-4
#define LINKE    "3.0"
#define ALB      "0.2"
#define STEP     "0.5"
#define BSKY	  1.0
#define DSKY	  1.0
#define DIST     "1.0"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <malloc.h>
#include "gis.h"
#include "Vect.h"
#include "projects.h"

FILE *felevin,*faspin,*fslopein,*flinkein,*falbedo,*flatin;
FILE *fincidout,*fbeam_rad,*finsol_time,*fdiff_rad,*frefl_rad;
FILE *fw;

char *elevin;
char *aspin;
char *slopein;
char *linkein = NULL;
char *albedo = NULL;
char *latin = NULL;
char *coefbh = NULL;
char *coefdh = NULL;
char *incidout = NULL;
char *beam_rad = NULL;
char *insol_time = NULL;
char *diff_rad = NULL;
char *refl_rad = NULL;
char *mapset = NULL;
char *per;
char *shade;
char mapname[1024];

struct Cell_head cellhd;
struct Map_info Map;
struct dig_head Head;
struct Key_value *in_proj_info, *in_unit_info;
struct pj_info iproj;
struct pj_info oproj;


int INPUT(void);
int OUTGR(void);
double amax1(double, double);
double amin1(double, double);
int min(int, int);
int max(int, int);
void com_par(void);
double lumcline2(void);
void joules2(void);

int new_point();
int searching();
void where_is_point();
void vertex(int, int);
void line_x(int, int);
void line_y(int, int);
void cube(int, int);
double distance2(double, double);
void (*func)(int, int);

void calculate(void);
double com_sol_const(int);
double com_declin(int);
double brad(double);
double drad(double);
int test(void);


int     shd, typ, n, m, ip, jp;
float     **z, **o, **s, **li, **a, **la, **cbhr, **cdhr;
double  stepx, stepy, stepxy, xp, yp, op, dp, xg0, xx0, yg0, yy0,deltx,delty;
float     **lumcl, **beam, **insol, **diff, **refl;
double  xmin, xmax, ymin, ymax,zmax=0.;
int     d, day, tlac=0,tien=0;
double  length, zmult=1.0, c, declin, linke, alb,step,dist;
double li_max=0.,li_min=100.,al_max=0.,al_min=1.0,la_max=0.,la_min=90.;
char *tt,*lt;
double z_orig, o_orig,slope,aspect,z1,zp;
double lum_C11, lum_C13, lum_C22, lum_C31, lum_C33, lum_Lx, lum_Ly, lum_Lz;
double lum_p, sunrise_time, sunset_time, h0, A0,angle;
double longitude, latitude, lum_time, ltime, tim, timo, declination;
double longit_l, latid_l, cos_u, cos_v, sin_u, sin_v;
double sin_phi_l, tan_lam_l, lum_C31_l, lum_C33_l;
double beam_e, diff_e, refl_e, bh, dh,rr,insol_t;
double cbh, cdh;
double TOLER;

main(int argc, char *argv[])
{

 struct GModule *module;
 struct
  {
struct Option *elevin,*aspin,*slopein,*linkein,*lin,*albedo,*alb,*latin,*lat,*coefbh, *coefdh, *incidout,*beam_rad,*insol_time,*diff_rad,*refl_rad,*day,*step, *declin,*ltime,*dist;
	  } parm;

 struct
  {     struct Flag   *shade;
        } flag;

	  G_gisinit (argv[0]);
          module = G_define_module();

          module->description =
          "Computes direct (beam), diffuse and reflected solar irradiation raster "
          "maps for given day, latitude, surface and atmospheric conditions. Solar "
          "parameters (e.g. sunrise, sunset times, declination, extraterrestrial "
          "irradiance, daylight length) are saved in a local text file. "
          "Alternatively, a local time can be specified to compute solar "
          "incidence angle and/or irradiance raster maps. The shadowing effect of "
          "the topography is optionally incorporated. ";

		if(G_get_set_window(&cellhd)==-1) exit(0);
		stepx = cellhd.ew_res;
		stepy = cellhd.ns_res;
		n/*n_cols*/ = cellhd.cols;
		m/*n_rows*/ = cellhd.rows;
		xmin = cellhd.west;
		ymin = cellhd.south;
		xmax = cellhd.east;
		ymax = cellhd.north;
        	deltx = fabs(cellhd.east - cellhd.west);
        	delty = fabs(cellhd.north - cellhd.south);

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

	  parm.slopein = G_define_option();
	  parm.slopein->key = "slopein";
	  parm.slopein->type = TYPE_STRING;
	  parm.slopein->required = YES;
	  parm.slopein->gisprompt = "old,cell,raster";
	  parm.slopein->description = "Name of the slope raster file";

          parm.linkein = G_define_option();
          parm.linkein->key = "linkein";
          parm.linkein->type = TYPE_STRING;
          parm.linkein->required = NO;
          parm.linkein->gisprompt = "old,cell,raster";
          parm.linkein->description = "Name of the Linke's turbidity coefficient raster file";
	
	if (parm.linkein->answer == NULL) {
          parm.lin = G_define_option();
          parm.lin->key = "lin";
          parm.lin->type = TYPE_DOUBLE;
          parm.lin->answer = LINKE;	
          parm.lin->required = NO;
          parm.lin->description = "A single value of the Linke's turbidity coefficient";
	}	

          parm.albedo = G_define_option();
          parm.albedo->key = "albedo";
          parm.albedo->type = TYPE_STRING;
          parm.albedo->required = NO;
          parm.albedo->gisprompt = "old,cell,raster";
          parm.albedo->description = "Name of the albedo coefficient raster file";

        if (parm.albedo->answer == NULL) {
          parm.alb = G_define_option();
          parm.alb->key = "alb";
          parm.alb->type = TYPE_DOUBLE;
          parm.alb->answer = ALB;
          parm.alb->required = NO;
          parm.alb->description = "A single value of the albedo coefficient";
        }    

          parm.latin = G_define_option();
          parm.latin->key = "latin";
          parm.latin->type = TYPE_STRING;
          parm.latin->required = NO;
          parm.latin->gisprompt = "old,cell,raster";
          parm.latin->description = "Name of the latitude raster file";

        if (parm.latin->answer == NULL) {
          parm.lat = G_define_option();
          parm.lat->key = "lat";
          parm.lat->type = TYPE_DOUBLE;
          parm.lat->required = NO;
          parm.lat->description = "A single value of latitude";
        } 

          parm.coefbh = G_define_option();
          parm.coefbh->key = "coefbh";
          parm.coefbh->type = TYPE_STRING;
          parm.coefbh->required = NO;
          parm.coefbh->gisprompt = "new,cell,raster";
          parm.coefbh->description = "The real-sky beam radiation coefficient file";

	  parm.coefdh = G_define_option();
	  parm.coefdh->key = "coefdh";
	  parm.coefdh->type = TYPE_STRING;
	  parm.coefdh->required = NO;
	  parm.coefdh->gisprompt = "new,cell,raster";
	  parm.coefdh->description = "The real-sky diffuse radiation coefficient file";

	  parm.incidout = G_define_option();
	  parm.incidout->key = "incidout";
	  parm.incidout->type = TYPE_STRING;
	  parm.incidout->required = NO;
          parm.incidout->gisprompt = "new,cell,raster";
	  parm.incidout->description = "Output incidence angle file (raster)";

	  parm.beam_rad = G_define_option();
	  parm.beam_rad->key = "beam_rad";
	  parm.beam_rad->type = TYPE_STRING;
	  parm.beam_rad->required = NO;
          parm.beam_rad->gisprompt = "new,cell,raster";
	  parm.beam_rad->description = "Output direct (beam) irradiance/irradiation file (raster)";

          parm.insol_time = G_define_option();
          parm.insol_time->key = "insol_time";
          parm.insol_time->type = TYPE_STRING;
          parm.insol_time->required = NO;
          parm.insol_time->gisprompt = "new,cell,raster";
          parm.insol_time->description = "Output insolation time file (raster)";

          parm.diff_rad = G_define_option();
          parm.diff_rad->key = "diff_rad";
          parm.diff_rad->type = TYPE_STRING;
          parm.diff_rad->required = NO;
          parm.diff_rad->gisprompt = "new,cell,raster";
          parm.diff_rad->description = "Output diffuse irradiance/irradiation file (raster)";

          parm.refl_rad = G_define_option();
          parm.refl_rad->key = "refl_rad";
          parm.refl_rad->type = TYPE_STRING;
          parm.refl_rad->required = NO;
          parm.refl_rad->gisprompt = "new,cell,raster";
          parm.refl_rad->description = "Output reflected irradiance/irradiation file (raster)";

          parm.day = G_define_option();
          parm.day->key = "day";
          parm.day->type = TYPE_INTEGER;
          parm.day->required = YES;
          parm.day->description = "No. of day of the year (1-365)";

          parm.step = G_define_option();
          parm.step->key = "step";
          parm.step->type = TYPE_DOUBLE;
          parm.step->answer = STEP;
          parm.step->required = NO;
          parm.step->description = "Time step computing all-day radiation";

          parm.declin = G_define_option();
          parm.declin->key = "declin";
          parm.declin->type = TYPE_DOUBLE;
          parm.declin->required = NO;
          parm.declin->description = "Required declination value (overriding the internal value)";

          parm.ltime = G_define_option();
          parm.ltime->key = "time";
          parm.ltime->type = TYPE_DOUBLE;
/*          parm.ltime->answer = TIME;*/
          parm.ltime->required = NO;
          parm.ltime->description = "Local time";

          parm.dist = G_define_option();
          parm.dist->key = "dist";
          parm.dist->type = TYPE_DOUBLE;
          parm.dist->answer = DIST;
          parm.dist->required = NO;
          parm.dist->description = "Sampling distance step coefficient (0.5-1.5)";

          flag.shade = G_define_flag();
          flag.shade->key = 's';
          flag.shade->description = "Incorporate the shadowing effect of terrain";
		
		if(G_parser(argc,argv)) exit(1);

		shd=flag.shade->answer;

        elevin = parm.elevin->answer;
        aspin = parm.aspin->answer;
        slopein = parm.slopein->answer;
	linkein = parm.linkein->answer;
        albedo = parm.albedo->answer;
	latin = parm.latin->answer;
        coefbh = parm.coefbh->answer;
        coefdh = parm.coefdh->answer;
        incidout = parm.incidout->answer;
        beam_rad = parm.beam_rad->answer;
        insol_time = parm.insol_time->answer;
        diff_rad = parm.diff_rad->answer;
        refl_rad = parm.refl_rad->answer;

        sscanf(parm.day->answer, "%d", &day);
        sscanf(parm.step->answer, "%lf", &step);
        tt=parm.ltime->answer;
        if (parm.ltime->answer != NULL) sscanf(parm.ltime->answer, "%lf", &timo);
        if (parm.linkein->answer == NULL) sscanf(parm.lin->answer, "%lf", &linke);	
        if (parm.albedo->answer == NULL) sscanf(parm.alb->answer, "%lf", &alb);
	lt = parm.lat->answer;
        if (parm.lat->answer != NULL) sscanf(parm.lat->answer, "%lf", &latitude);
        if (parm.coefbh->answer == NULL) cbh = BSKY;
        if (parm.coefdh->answer == NULL) cdh = DSKY;
        sscanf(parm.dist->answer, "%lf", &dist);

                stepxy = dist * 0.5 * (stepx + stepy);
/*              stepxy = 700.;*/
                TOLER = stepxy * EPS;

	if (parm.declin->answer == NULL)
        declination = com_declin(day);
	else {
		sscanf(parm.declin->answer, "%lf", &declin);
		declination = -declin;
/*		if(!test()) {
		declination = com_declin(day); 
		fprintf(stderr,"\nWarning: declination was assigned to the incorrect day!");
		fprintf(stderr,"\nUser-set declination: %f",declin);
		fprintf(stderr,"\nInternally calculated declination: %f",declination);
		fprintf(stderr,"\nUser setting applied in the calculation..."); 
                declination = -declin;
		} ****** not finished yet */
	}

	if(lt != NULL) latitude = - latitude * DEG;

		if(tt != NULL)
	       {

			tim = (timo - 12) * 15;
			/* converting to degrees */
			/* Jenco (12-lum_time) * 15 */
			if (tim < 0)
			tim += 360;
			tim = M_PI * tim / 180;
			/* conv. to radians */
		}

/**********end of parser - ******************************/

	   INPUT();
	   calculate();
	   OUTGR();

   return 1;
}


int INPUT(void)

{
	FCELL    *cell1, *cell2;
	FCELL    *cell3, *cell4, *cell5,*cell6;
	FCELL    *rast1, *rast2;
	int     fd1, fd2, fd3, fd4, fd5, fd6, row, row_rev;
	int     fr1, fr2;
	int     l,i,j;

	cell1=G_allocate_f_raster_buf();
	cell2=G_allocate_f_raster_buf();
	cell3=G_allocate_f_raster_buf();

	z = (float **)malloc(sizeof(float)*(m));
	o = (float **)malloc(sizeof(float)*(m));
	s = (float **)malloc(sizeof(float)*(m));

  for(l=0;l<m;l++)
   {
	z[l]   = (float*)malloc(sizeof(float)*(n));
	o[l]   = (float*)malloc(sizeof(float)*(n));
	s[l]   = (float*)malloc(sizeof(float)*(n));

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

if (linkein != NULL)
{
        cell4=G_allocate_f_raster_buf();
        li = (float **)malloc(sizeof(float)*(m));
  for(l=0;l<m;l++)
        li[l]   = (float*)malloc(sizeof(float)*(n));

  if((mapset=G_find_cell(linkein,""))==NULL)
  printf("cell file not found\n");

  fd4 = G_open_cell_old(linkein,mapset);
}

if (albedo != NULL)
{
        cell5=G_allocate_f_raster_buf();
        a = (float **)malloc(sizeof(float)*(m));
  for(l=0;l<m;l++)
        a[l]   = (float*)malloc(sizeof(float)*(n));

  if((mapset=G_find_cell(albedo,""))==NULL)
  printf("cell file not found\n");

  fd5 = G_open_cell_old(albedo,mapset);
}

if (latin != NULL)
{
        cell6=G_allocate_f_raster_buf();
        la = (float **)malloc(sizeof(float)*(m));
  for(l=0;l<m;l++)
        la[l]   = (float*)malloc(sizeof(float)*(n));

  if((mapset=G_find_cell(latin,""))==NULL)
  printf("cell file not found\n");

  fd6 = G_open_cell_old(latin,mapset);
}

if (coefbh != NULL)
{
        rast1=G_allocate_f_raster_buf();
        cbhr = (float **)malloc(sizeof(float)*(m));
  for(l=0;l<m;l++)
        cbhr[l]   = (float*)malloc(sizeof(float)*(n));

  if((mapset=G_find_cell(coefbh,""))==NULL)
  printf("cell file not found\n");

  fr1 = G_open_cell_old(coefbh,mapset);
}

if (coefdh != NULL)
{
        rast2=G_allocate_f_raster_buf();
        cdhr = (float **)malloc(sizeof(float)*(m));
  for(l=0;l<m;l++)
        cdhr[l]   = (float*)malloc(sizeof(float)*(n));

  if((mapset=G_find_cell(coefdh,""))==NULL)
  printf("cell file not found\n");

  fr2 = G_open_cell_old(coefdh,mapset);
}


  for (row=0; row<m; row++)
  {
	  G_get_f_raster_row(fd1,cell1,row);
	  G_get_f_raster_row(fd2,cell2,row);
	  G_get_f_raster_row(fd3,cell3,row);
if(linkein != NULL) G_get_f_raster_row(fd4,cell4,row);
if(albedo != NULL) G_get_f_raster_row(fd5,cell5,row);
if(latin != NULL) G_get_f_raster_row(fd6,cell6,row);
if(coefbh != NULL) G_get_f_raster_row(fr1,rast1,row);
if(coefdh != NULL) G_get_f_raster_row(fr2,rast2,row);

	for (j=0; j<n; j++)
	{
	   row_rev = m - row - 1;
	   z[row_rev][j] = (float ) cell1[j];
	   o[row_rev][j] = (float ) cell2[j];
	   s[row_rev][j] = (float ) cell3[j];
if(linkein != NULL) li[row_rev][j] = (float ) cell4[j];
if(albedo != NULL) a[row_rev][j] = (float ) cell5[j];
if(latin != NULL) la[row_rev][j] = (float ) cell6[j];
if(coefbh != NULL) cbhr[row_rev][j] = (float ) rast1[j];
if(coefdh != NULL) cdhr[row_rev][j] = (float ) rast2[j];

	 }
   }
  G_close_cell(fd1);
  G_close_cell(fd2);
  G_close_cell(fd3);
if(linkein != NULL)  G_close_cell(fd4);
if(albedo != NULL) G_close_cell(fd5);
if(latin != NULL) G_close_cell(fd6);
if(coefbh != NULL) G_close_cell(fr1);
if(coefdh != NULL) G_close_cell(fr2);


/*******transformation of angles from 0 to east counterclock
		to 0 to north clocwise, for ori=0 upslope flowlines
		turn the orientation 2*M_PI ************/

  /*for (i = 0; i < m; ++i)*/
  for (i = 0; i < m; i++)
  {
	  for (j = 0; j < n; j++)
	  {
		zmax = amax1(zmax,z[i][j]);
		if ( o[i][j] != 0. ) {
		   if( o[i][j] < 90. )
			   o[i][j] = 90. - o[i][j];
		   else
			  o[i][j] = 450.  - o[i][j];
	  /*   printf("o,z = %d  %d i,j, %d %d \n", o[i][j],z[i][j],i,j);*/
		}

	  }
   }

	return 1;
}

int OUTGR(void)
{
   FCELL *cell7,*cell8,*cell9,*cell10,*cell11;
   int      fd7, fd8,fd9,fd10,fd11;
   int      i,iarc,j;
   int       dsmax=0;
   char     msg[100];


		if (incidout != NULL)
		{
		cell7 = G_allocate_f_raster_buf();
		fd7 = G_open_fp_cell_new (incidout);
		if (fd7 < 0)
		{
			sprintf (msg, "unable to create raster map %s", incidout);
			G_fatal_error (msg);
			exit(1);
		}
		}

		if (beam_rad != NULL)
		{
		cell8 = G_allocate_f_raster_buf();
		fd8 = G_open_fp_cell_new (beam_rad);
		if (fd8 < 0)
		{
			sprintf (msg, "unable to create raster map %s", beam_rad);
			G_fatal_error (msg);
			exit(1);
		}
		}

                if (insol_time != NULL)
                {
                cell11 = G_allocate_f_raster_buf();
                fd11 = G_open_fp_cell_new (insol_time);
                if (fd11 < 0)
                {
                        sprintf (msg, "unable to create raster map %s", insol_time);
                        G_fatal_error (msg);
                        exit(1);
                }
                }


                if (diff_rad != NULL)
                {
                cell9 = G_allocate_f_raster_buf();
                fd9 = G_open_fp_cell_new (diff_rad);
                if (fd9 < 0)
                {
                        sprintf (msg, "unable to create raster map %s", diff_rad);
                        G_fatal_error (msg);
                        exit(1);
                }
                }

                if (refl_rad != NULL)
                {
                cell10 = G_allocate_f_raster_buf();
                fd10 = G_open_fp_cell_new (refl_rad);
                if (fd10 < 0)
                {
                        sprintf (msg, "unable to create raster map %s", refl_rad);
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
			cell7[j]=(FCELL) lumcl[i][j];
		  }
		  G_put_f_raster_row (fd7, cell7);
		}

		if(beam_rad!=NULL)
		{
		  for(j=0;j<n;j++)
		  {
			cell8[j]=(FCELL)beam[i][j];
			dsmax = max(dsmax, beam[i][j]);
		  }
		  G_put_f_raster_row (fd8, cell8);
		}

                if(insol_time!=NULL)
                {
                  for(j=0;j<n;j++)
                  {
                        cell11[j]=(FCELL)insol[i][j];
                        dsmax = max(dsmax, insol[i][j]);
                  }
                  G_put_f_raster_row (fd11, cell11);
                }


                if(diff_rad!=NULL)
                {
                  for(j=0;j<n;j++)
                  {
                        cell9[j]=(FCELL)diff[i][j];
                        dsmax = max(dsmax, diff[i][j]);
                  }
                  G_put_f_raster_row (fd9, cell9);
                }

                if(refl_rad!=NULL)
                {
                  for(j=0;j<n;j++)
                  {
                        cell10[j]=(FCELL)refl[i][j];
                        dsmax = max(dsmax, refl[i][j]);
                  }
                  G_put_f_raster_row (fd10, cell10);
                }

	  }

		if(incidout!=NULL)
	  G_close_cell (fd7);
		if(beam_rad!=NULL)
	  G_close_cell (fd8);
                if(diff_rad!=NULL)
          G_close_cell (fd9);
                if(refl_rad!=NULL)
          G_close_cell (fd10);
               if(insol_time!=NULL)
          G_close_cell (fd11);

		return 1;
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


void com_par(void)
{
	double old_time, pom, xpom, ypom;
	double sr_min=24.,sr_max=0.,ss_min=24.,ss_max=0.;


			old_time = lum_time;

			lum_C11 = sin(latitude) * cos(declination);
			lum_C13 = -cos(latitude) * sin(declination);
			lum_C22 = cos(declination);
			lum_C31 = cos(latitude) * cos(declination);
			lum_C33 = sin(latitude) * sin(declination);

			lum_Lx = -lum_C22 * sin(lum_time);
			lum_Ly = lum_C11 * cos(lum_time) + lum_C13;
			lum_Lz = lum_C31 * cos(lum_time) + lum_C33;

			if (fabs(lum_C31) >= EPS) {
				  pom = -lum_C33 / lum_C31;
				  if (fabs(pom) <= 1) {
					pom = acos(pom);
					pom = (pom * 180) / M_PI;
					sunrise_time = (90 - pom) / 15 + 6;
					sunset_time = (pom - 90) / 15 + 18;
			  }
			  else {
			if (pom < 0) {
/*	  printf("\n Sun is ABOVE the surface during the whole day\n");*/
			  sunrise_time = 0;
			  sunset_time = 24;
			  if (fabs(pom) - 1 <= EPS)
			printf("\texcept at midnight is sun ON THE HORIZONT\n");
				}
				else {
/*		  printf("\n The sun is BELOW the surface during the whole day\n");*/
			  if (fabs(pom) - 1 <= EPS) {
			printf("\texcept at noon is sun ON HORIZONT\n");
			sunrise_time = 12;
			sunset_time = 12;
			  }
				}
			  }
			}
			else {
			  if (fabs(lum_Lz) >= EPS) {
				if (lum_Lz > 0) {
/*			  printf("\tSun is ABOVE area during the whole day\n");*/
				  sunrise_time = 0;
				  sunset_time = 24;
					}
					else {
					h0=0.;A0 = UNDEF; return;
					}
				  }
				  else {
/*			printf("\tThe Sun is ON HORIZON during the whole day\n");*/
						sunrise_time = 0;
						sunset_time = 24;
				  }
			}

		  h0 = asin (lum_Lz);   /* vertical angle of the sun */
					/* lum_Lz is sin(h0) */

		  xpom = lum_Lx * lum_Lx;
		  ypom = lum_Ly * lum_Ly;
		  pom = sqrt(xpom + ypom);

		sr_min = amin1(sr_min,sunrise_time);
		sr_max = amax1(sr_max,sunrise_time);
		ss_min = amin1(ss_min,sunset_time);
                ss_max = amax1(ss_max,sunset_time);

		  if (fabs(pom) > EPS) {
			A0 = lum_Ly / pom;
		A0 = acos(A0);     /* horiz. angle of the Sun */
/*			A0 *= RAD;*/
			if (lum_Lx < 0)
			  A0 = M2_PI - A0;
			  }
			  else {
			A0 = UNDEF;
			if (h0 > 0)
			  printf ("A0 = Zenit\n");
			else
			  printf ("A0 = Nadir\n");
				  }

                   if( A0 < 0.5 * M_PI )
                           angle = 0.5 * M_PI  - A0;
                   else
                          angle = 2.5 * M_PI  - A0;

            if ( tlac == 1) {
		fw=fopen("r.sun_out.txt","w");

		fprintf(fw,"\n\n ----------------------------------------------------------------");
                fprintf(fw,"\n Day [1-365]: 		                   %d",day);
		fprintf(fw,"\n Solar constant (W/m^2):                   1367");
		fprintf(fw,"\n Extraterrestrial irradiance (W/m^2):      %f", c);
		fprintf(fw,"\n Declination (rad):                        %f", -declination);  
if(lt != NULL)       fprintf(fw,"\n Latitude (deg):                           %.4f", -latitude*RAD);
		else fprintf(fw,"\n Latitude min-max(deg):                    %.4f-%.4f", la_min,la_max);
if (tt != NULL) {
       		fprintf(fw,"\n Sunrise time (hr.):                       %.2f", sunrise_time);
    		fprintf(fw,"\n Sunset time (hr.):                        %.2f", sunset_time);
                fprintf(fw,"\n Daylight time (hr.):                      %.2f", sunset_time-sunrise_time);
		} else {
		fprintf(fw,"\n Sunrise time min-max (hr.):               %.2f-%.2f", sr_min,sr_max);
                fprintf(fw,"\n Sunset time min-max (hr.):                %.2f-%.2f", ss_min,ss_max);
		fprintf(fw,"\n Time step (hr.):		 	   %f",step);
		}
if(incidout != NULL || tt != NULL) fprintf (fw,"\n Solar altitude (deg):                     %.4f", h0*RAD);
if(incidout != NULL || tt != NULL) fprintf (fw,"\n Solar azimuth (deg):                      %.4f", A0*RAD); 
if(linkein == NULL) fprintf(fw,"\n Linke turbidity factor:                   %.1f", linke);
                      else fprintf(fw,"\n Linke turbidity factor min-max:           %.1f-%.1f", li_min,li_max);
if(albedo == NULL) fprintf(fw,"\n Ground albedo:                            %.3f", alb);
                   else fprintf(fw,"\n Ground albedo min-max:                    %.3f-%.3f", al_min,al_max);
		fprintf(fw,"\n -----------------------------------------------------------------\n\n");
		fclose(fw);
	tlac = 0; 
                   }   
	}
/**********************************************************/

double lumcline2(void)
{
	  double s=0;
	  int r=0;

	  func = cube;
	  tien = 0;

	  if (shd == 1) {
	  length = 0;
		
	  while((r = searching()) == 1)
	   {
  		if (r == 3) break; /* no test is needed */
	   }
	  }

          xx0 = xg0; yy0 = yg0;

          if (r == 2) {
	     tien = 1; /* shadow */
          } else
		{

		if (z_orig != UNDEFZ) {
		  s = lum_C31_l * cos(-lum_time - longit_l) + lum_C33_l; /* Jenco */
		}
		else
		{
		s = lum_Lz;
		}
	   }
	  if (s < 0) return 0.;
	  return (s);
}

void joules2(void)
{

	  double s0, dfr, dfr_rad, dfr1_rad, dfr2_rad, fr1, fr2, dfr1, dfr2;
	  double ra, dra, ss_rad=0.,sr_rad;
	  int i1, i2,  ss;

                        beam_e = 0.; 
			diff_e = 0.;
			refl_e = 0.;
			insol_t = 0.;
			tien = 0;

	if (tt == NULL) lum_time=0.;

			com_par();

        if (tt != NULL) {/*irradiance*/

                  	s0 = lumcline2();

                 if (h0 > 0.) {
			if (tien != 1 && s0 > 0.) {
                 	ra = brad(s0); /* beam radiation*/
               	        beam_e += ra;
			}
				else {
				beam_e = 0.;
				bh = 0.;
				}

                if(diff_rad != NULL) {
                        dra = drad(s0); /* diffuse rad. */
                        diff_e += dra;
                        }
                if(refl_rad != NULL) {
                        if (diff_rad == NULL) drad(s0);
                        refl_e += rr; /* reflected rad. */
                        }
		} /* h0 */
	}
			else {
/* all-day radiation*/


          i1 = (int)sunrise_time;
          fr1 = sunrise_time - i1;
          if (fr1 > 0.) fr1 = 1 - fr1;
          else fr1 = -fr1;
	
	dfr1 = fr1;
        while( dfr1 > step) {
	dfr1 = dfr1 - step;
	}

          i2 = (int)sunset_time;
          fr2 = sunset_time - i2;

	dfr2 = fr2;
        while( dfr2 > step) {
        dfr2 = dfr2 - step;
        }

	sr_rad = (sunrise_time - 12.) * 15.;
                        if (ss_rad < 0)
                        sr_rad += 360;
                        sr_rad = sr_rad * DEG;
	ss_rad = (sunset_time - 12.) * 15.;
                        if (ss_rad < 0)
                        ss_rad += 360;
                        ss_rad = ss_rad * DEG;

	dfr1_rad = dfr1 * 15. * DEG;
        dfr2_rad = dfr2 * 15. * DEG;
        dfr_rad = step * 15. * DEG;

	lum_time = sr_rad + dfr1_rad / 2.; 
	dfr = dfr1;

	ss = 1;
	while (ss == 1)
	{
	
	com_par();
       	s0 = lumcline2();

             if (h0 > 0.) {
			
			if (tien != 1 && s0 > 0.) {	
                        insol_t += dfr;
			ra = brad(s0);
                        beam_e += dfr * ra;
			ra=0.;
			}
			else  bh = 0.;
		if(diff_rad != NULL) {
			dra = drad(s0);
			diff_e += dfr * dra;
			dra=0.;
			}
		if(refl_rad != NULL) {
			if (diff_rad == NULL) drad(s0);	
                        refl_e += dfr * rr;
			rr = 0.;
			}
                 } /* illuminated */


                if (dfr < step) {
			dfr = step;
			lum_time = lum_time + dfr1_rad / 2. + dfr_rad / 2.;
		} 
		else {
		lum_time = lum_time + dfr_rad;
		}
		if (lum_time > ss_rad - dfr2_rad / 2.) {
			dfr = dfr2;
			lum_time = ss_rad - dfr2_rad / 2.;
		 ss = 0; /* we've got the sunset */
			}
		tien = 0;
	} /* end of while */
	} /* all-day radiation */

}
/*//////////////////////////////////////////////////////////////////////*/


int new_point()
{

        if (!(zp == UNDEFZ)) {
                  yy0 += stepxy * sin(angle);
                  xx0 += stepxy * cos(angle);
/*printf("\nxx0, yy0 angle %f %f %f %f",xx0,yy0,A0,angle);*/
         if ((xx0 < 0) || (xx0 > deltx) || (yy0 < 0) || (yy0 > delty))
                          return (3);
                  else
                          return(1);
        }
        return(0);
}

void where_is_point()
{
        double sx, sy;
        double dx, dy;
        int i, j;

        sx = xx0/stepx; sy = yy0/stepy;
        sx += TOLER; sy += TOLER;

        i = (int)sx; j = (int)sy;
	if (i < n -1  && j < m-1) {
 /*       zp  = z[j][i]; */

        dx = xx0 - (double)i * stepx; dy = yy0 - (double)j * stepy;

        if ((fabs(dx) < TOLER) && (fabs(dy) < TOLER))
                  func = vertex;
        if ((fabs(dx) > TOLER) && (fabs(dy) < TOLER))
                  func = line_x;
        if ((fabs(dx) < TOLER) && (fabs(dy) > TOLER))
            func = line_y;
        if ((fabs(dx) > TOLER) && (fabs(dy) > TOLER))
                  func = cube;

        func(j, i);
	} else
		{
		func = NULL;
		}
}

void vertex(jmin, imin)
int jmin, imin;
{
        zp = z[jmin][imin];
        if ((zp == UNDEFZ))
                  func = NULL;
}

void line_x(jmin, imin)
int jmin, imin;
{
        double c1, c2;
        double d1, d2, e1, e2;
        e1 = (double)imin * stepx;
        e2 = (double)(imin + 1) * stepx;

        c1 = z[jmin][imin]; c2 = z[jmin][imin + 1];
        if (!((c1 == UNDEFZ) || (c2 == UNDEFZ))) {

        /*        if(dist < 0.5) {
                  d1 = (xx0 - e1) / (e2 - e1);
                  d2 = 1 - d1;
                  zp = d1 * c2 + d2 * c1;
		}

                if (dist >=0.5 && dist <= 1.0) { */


                if (dist <= 1.0) {
                  d1 = (xx0 - e1) / (e2 - e1);
                  d2 = 1 - d1;
                  if(d1 < d2) zp = c1;
                        else
                        zp = c2;
                }

                if (dist > 1.0)
                zp = amax1(c1,c2);
        }
        else
                  func = NULL;
}


void line_y(jmin, imin)
int jmin, imin;
{
        double c1, c2;
        double d1, d2, e1, e2;
        e1 = (double)jmin * stepy;
        e2 = (double)(jmin + 1) * stepy;

        c1 = z[jmin][imin]; c2 = z[jmin + 1][imin];
        if (!((c1 == UNDEFZ) || (c2 == UNDEFZ))) {

/*		if(dist < 0.5) {
                  d1 = (yy0 - e1) / (e2 - e1);
                  d2 = 1 - d1;

                  zp = d1 * c2 + d2 * c1;
		}

		if (dist >=0.5 && dist <= 1.0) { */

                if (dist <= 1.0) {
                  d1 = (yy0 - e1) / (e2 - e1);
                  d2 = 1 - d1;
		  if(d1 < d2) zp = c1;
			else
			zp = c2;
		}

		if (dist > 1.0) 
		zp = amax1(c1,c2);

        }
        else
                  func = NULL;

}

double distance2(x00, y00)
double x00, y00;
{
        double dx, dy;

        dx = xx0 - x00; dx *= dx;
        dy = yy0 - y00; dy *= dy;
	
/*        return (sqrt(dx + dy));*/
        return (dx + dy);
}

void cube(jmin, imin)
int jmin, imin;
{
        int i, j, ig;
        double x1, x2, y1, y2;
        double v0, v[4],vmin=BIG;
        double  c[4],cmax=-BIG;

        x1 = (double)imin * stepx;
        x2 = (double)(imin + 1) * stepx;

        y1 = (double)jmin * stepy;
        y2 = (double)(jmin + 1) * stepy;

        v[0] = distance2(x1, y1); 
	vmin = amin1(vmin,v[0]);
	v[1] = distance2(x2, y1);
	vmin = amin1(vmin,v[1]);
        v[2] = distance2(x2, y2);
	vmin = amin1(vmin,v[2]);
	v[3] = distance2(x1, y2);
	vmin = amin1(vmin,v[3]);

        c[0] = z[jmin][imin];
        c[1] = z[jmin][imin + 1];
        c[2] = z[jmin + 1][imin + 1];
        c[3] = z[jmin + 1][imin];

/*        if (dist >= 0.5 && dist <= 1.0) {*/

	if (dist <= 1.0) {
		ig = 0;
	        for (i = 0; i < 4; i++)
        	if(vmin == v[i]) ig = i;

                if (c[ig] != UNDEFZ) zp = c[ig];
			else
			  func = NULL;
		}

/*	if (dist < 0.5) {
        for (i = 0; i < 4; i++)
                  v[i] *= v[i]; 

        v0 = 0;
        for (i = 0; i < 4; i++) {
                  v[i] = 1. / v[i];
                  v0 += v[i];
        }


        j = 0;
        while ((c[j] != UNDEFZ) && (j < 4)) j++;
        if (j == 4) {

                  zp = 0;
                  for (i = 0; i < 4; i++){
                          zp += v[i] * c[i];
                        }
                  zp /= v0;
        }
        else {
                  func = NULL;
           }
	} */

	if (dist > 1.0) {
		for (i = 0; i < 4; i++) {
		if (c[i] != UNDEFZ) {
			cmax = amax1(cmax,c[i]);
			zp = cmax;
			}
			else
				func = NULL;
		}
	}
}

int searching()
{
	double z2;
        int succes = 0;

        succes = new_point();
	if(succes == 1) {
	where_is_point();
        if (func == NULL) return (3); 
        length += stepxy;
        z2 = z_orig + length * tan(h0);
        if (z2 < zp) succes = 2; /* shadow*/
        if( z2 > zmax) succes = 3; /* no test needed all visible*/
	} 

        return (succes);
}



/*//////////////////////////////////////////////////////////////////////*/

void calculate(void)
{
			int i, j, l;
/*			double energy;*/
			double lum, q1;

			fprintf(stderr,"\n\n");

		if (incidout != NULL)
		{
			lumcl = (float **)malloc(sizeof(float)*(m));
			for(l=0;l<m;l++)
			{
				 lumcl[l]   = (float*)malloc(sizeof(float)*(n));
			}
			for (j = 0; j < m; j++)
			{
			  for (i = 0; i < n; i++)
			  lumcl[j][i] = 0.;
			}
		 }

		if (beam_rad != NULL)
		{
			beam = (float **)malloc(sizeof(float)*(m));
			for( l=0; l<m; l++)
			{
			  beam[l] = (float*)malloc(sizeof(float)*(n));
			}

			for (j = 0; j < m; j++)
			{
			  for (i = 0; i < n; i++)
			  beam[j][i] = 0.;
			}
		}

                if (insol_time != NULL)
                {
                        insol = (float **)malloc(sizeof(float)*(m));
                        for( l=0; l<m; l++)
                        {
                          insol[l] = (float*)malloc(sizeof(float)*(n));
                        }

                        for (j = 0; j < m; j++)
                        {
                          for (i = 0; i < n; i++)
                          insol[j][i] = 0.;
                        }
                }

                if (diff_rad != NULL)
                {
                        diff = (float **)malloc(sizeof(float)*(m));
                        for( l=0; l<m; l++)
                        {
                          diff[l] = (float*)malloc(sizeof(float)*(n));
                        }

                        for (j = 0; j < m; j++)
                        {
                          for (i = 0; i < n; i++)
                          diff[j][i] = 0.;
                        }
                }

                if (refl_rad != NULL)
                {
                        refl = (float **)malloc(sizeof(float)*(m));
                        for( l=0; l<m; l++)
                        {
                          refl[l] = (float*)malloc(sizeof(float)*(n));
                        }

                        for (j = 0; j < m; j++)
                        {
                          for (i = 0; i < n; i++)
                          refl[j][i] = 0.;
                        }
                }

  		      c = com_sol_const(day);

			for (j = 0; j < m; j++) {
				  G_percent(j,m-1,2);
			  for (i = 0; i < n; i++) {
/*				in1 = imin0 = i - 1; ix1 = imax0 = i + 1;
				jn1 = jmin0 = j - 1; jx1 = jmax0 = j + 1;
*/
		if (j==m-1 && i==n-1) tlac=1; /* prints a text file with parameters at the end of the region */
		
				xg0 = xx0 = (double)i * stepx;
				xp = xmin + xx0;
				yg0 = yy0 = (double)j * stepy;
				yp = ymin + yy0;
				func=NULL;length=0;

				 z_orig= z1 = zp = z[j][i];
                               o_orig = o[j][i];
                          if (z_orig != UNDEFZ) {
			if(o[j][i]!=0.) aspect=o[j][i] * DEG;
				else aspect = UNDEF;
				 slope = s[j][i] * DEG;
			if(linkein!=NULL) {
				linke = li[j][i];
				li_max = amax1(li_max,linke);
				li_min = amin1(li_min,linke);
				}
			if (albedo != NULL) {
				alb = a[j][i];
				al_max = amax1(al_max,alb);
				al_min = amin1(al_min,alb);
				}
                        if (latin != NULL) {
			latitude = la[j][i];
                        la_max = amax1(la_max,latitude);
                        la_min = amin1(la_min,latitude);
                        latitude = - latitude * DEG;
			}
	if (latin == NULL && lt == NULL) {
             if ((G_projection() != PROJECTION_LL))
             {

                if((in_proj_info = G_get_projinfo()) == NULL)
                G_fatal_error("Can't get projection info of current location: 
				please set latitude via 'lat' or 'latin' option!");
	fprintf(stderr,"\n AAA: %d",in_proj_info);

                if((in_unit_info = G_get_projunits()) == NULL)
                G_fatal_error("Can't get projection units of current location");

                if(pj_get_kv(&iproj,in_proj_info,in_unit_info) < 0)
                G_fatal_error("Can't get projection key values of current location");

                pj_zero_proj(&oproj);
                sprintf(oproj.proj,"%s","ll");

		longitude = xp;
		latitude = yp;

	        if(pj_do_proj(&longitude,&latitude,&iproj,&oproj) < 0)
	        {
	                fprintf(stderr,"Error in pj_do_proj\n");
	                exit(0);
	        }

                        la_max = amax1(la_max,latitude);
                        la_min = amin1(la_min,latitude);
                        latitude = - latitude * DEG;
	     } else
		{ /* ll projection */
			latitude = yp;
		        la_max = amax1(la_max,latitude);
                        la_min = amin1(la_min,latitude);
                        latitude = - latitude * DEG;
		}
	}

			if (coefbh != NULL) {
			cbh = cbhr[j][i];
			}
                        if (coefdh != NULL) {
                        cdh = cdhr[j][i];
                        }
          cos_u = cos(M_PI / 2 - slope);
          sin_u = sin(M_PI / 2 - slope);
          cos_v = cos(M_PI / 2 + aspect);
          sin_v = sin(M_PI / 2 + aspect);

	if(tt != NULL) lum_time = tim;

        sin_phi_l = -cos(latitude) * cos_u * sin_v + sin(latitude) * sin_u;
        latid_l = asin(sin_phi_l);

        q1 = sin (latitude) * cos_u * sin_v + cos(latitude) * sin_u;
        tan_lam_l = - cos_u * cos_v / q1;
        longit_l = atan (tan_lam_l);
        lum_C31_l = cos(latid_l) * cos(declination);
        lum_C33_l = sin_phi_l * sin(declination);

		
	if (incidout != NULL) {
			com_par();
			lum = lumcline2();
			lum = RAD * asin(lum);
			lumcl[j][i] = (float) lum;
				}
	if ((beam_rad != NULL) || (insol_time != NULL) || (diff_rad != NULL) || (refl_rad != NULL)) {
		joules2();
		if (beam_rad != NULL)	beam[j][i] = (float) beam_e;
                if (insol_time != NULL)   insol[j][i] = (float) insol_t;
/*	printf("\n %f",insol[j][i]);*/
		if (diff_rad != NULL) diff[j][i] = (float) diff_e;
		if (refl_rad != NULL) refl[j][i] = (float) refl_e;
		}

			} /* undefs*/
		  }
	}
                        fprintf(stderr,"\n");

}

double com_sol_const(int no_of_day)
{
	double I0,d1;

	/*  v W/(m*m) */
        d1 = M2_PI * no_of_day / 365.25;
	I0 = 1367. * (1 + 0.03344 * cos(d1 - 0.048869));

	return I0;
}


double com_declin(int no_of_day)
{
	double d1,decl;

	d1 = M2_PI * no_of_day / 365.25;
	decl = asin(0.3978 * sin(d1 - 1.4 + 0.0355 * sin(d1 - 0.0489)));
	decl = - decl;
/*	printf(" declination : %lf\n", decl); */

	return (decl);
}

double brad(double sh)
{
	double p, lm, tl, rayl, br;
	double drefract, temp1, temp2, h0refract;
 
	p = exp(-z_orig/8434.5);
	temp1 = 0.1594 + 1.123 * h0 + 0.065656 * h0 * h0;
	temp2 = 1. + 28.9344 * h0 + 277.3971 * h0 * h0;
	drefract = 0.061359 * temp1 / temp2; /* in radians */
	h0refract = h0 + drefract;
	lm = p / (sin(h0refract) + 0.50572 * pow(h0refract*RAD + 6.07995,-1.6364));
	tl = 0.8662 * linke;
if (lm <= 20.) rayl=1./(6.6296+lm*(1.7513+lm*(-0.1202+lm*(0.0065-lm*0.00013))));
	else
	rayl = 1./(10.4 + 0.718 * lm);
	bh = cbh * c * lum_Lz * exp(-rayl * lm * tl);
	if(aspect != UNDEF && slope != 0.) 
	br = bh * sh / lum_Lz;
	else br = bh;

	return (br);
}

double drad(double sh)
{
        double tn, fd, fx=0., A1, A2, A3,A1b;
	double  r_sky,kb,dr,gh,a_ln,ln,fg;

	tn = -0.015843 + 0.030543 * linke + 0.0003797 * linke * linke;
	A1b = 0.26463 - 0.061581 * linke  + 0.0031408 * linke * linke; 
	if (A1b * tn < 0.0022) A1 = 0.0022 / tn;
		else
		A1 = A1b;
	A2 = 2.04020 + 0.018945 * linke - 0.011161 * linke * linke;
	A3 = -1.3025 + 0.039231 * linke  + 0.0085079 * linke * linke;
	
	fd = A1 + A2 * lum_Lz + A3 * lum_Lz * lum_Lz;
	dh = cdh * c * fd * tn;
        gh = bh + dh;
	if (aspect != UNDEF && slope != 0.) {
	kb = bh / (c * lum_Lz);
	r_sky = (1. + cos(slope)) / 2.;
	a_ln = A0 - aspect;
	ln=a_ln;
	if(a_ln > M_PI) ln = a_ln - M2_PI;
		else if (a_ln < -M_PI) ln = a_ln + M2_PI;
	a_ln=ln;
	fg = sin(slope) - slope * cos(slope) - M_PI * sin(slope /2.) * sin(slope/2.);
	if (tien == 1 || sh <= 0.) fx = r_sky + fg * 0.252271;
	else if	
	(h0 >= 0.1) {
	fx = ((0.00263 - kb *(0.712 + 0.6883 * kb)) * fg + r_sky) * (1. - kb) + kb * sh / lum_Lz;
	}
	else if 
	(h0 < 0.1) 
	fx = ((0.00263 - 0.712 * kb - 0.6883 * kb * kb) * fg + r_sky)* (1. - kb) + kb * sin(slope) * cos(a_ln) / (0.1 - 0.008 * h0);
	dr = dh * fx;
/* refl. rad */
        rr = alb * gh * (1 - cos(slope)) / 2.;
	} 
		else { /* plane */
		dr = dh;
		rr = 0.;
		}
        return (dr);
}

int test(void) 
{
/* not finshed yet */
	int dej;

	printf("\n ddd: %f",declin);
	dej = asin(-declin/0.4093) * 365. / M2_PI + 81;
/*        dej = asin(-declin/23.35 * DEG) / 0.9856 - 284;*/
/*	dej = dej - 365;*/
	printf("\n d: %d ",dej);
	if (dej < day-5 || dej > day+5 ) return 0;
		else return 1;
}
