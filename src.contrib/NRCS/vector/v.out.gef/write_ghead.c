/* %W% %G% */
/*
**  Written by R.L.Glenn 2/26/90
**  USDA, Soil COnservation Service, CGIS Division
*/
#include <string.h>
#include "Vect.h"
#include "gis.h"
#include "export_gef.h"

#define LINE1_FMT  "%-30.30s%-8d%-7.7s%-24.24s\n"
#define LINE2_FMT  "%-30.30s%-15.15s%-18.18s%-9.9s\n"
#define LINE3_FMT "%2s %12.3lf %12.3lf %9s%5d%3d%3d  %8s%5d%3d%3d\n"

#define XTRA_PERC  0.05

#define DIG_UNITS	0.001

/********************* PARMS FOR PROJ **********************/
# define RAD_TO_DEG     57.29577951308232
typedef struct { double u, v; }		UV;

/**** routines for projection support ********/
static UV (*proj1)();      /* projection function 1*/
static UV (*proj2)();      /* projection function 2*/
  
do_INV(u,v) double *u, *v;
{
 UV data;    /*        data structure supplied to proj conv. routines */
 /*DEBUG fprintf(stderr,"IN1 %lf %lf\n",*u,*v);*/
	data.u = *u; data.v = *v;
	data = (*proj1)(data);
	*u = data.u * RAD_TO_DEG; *v = data.v * RAD_TO_DEG;
/*DEBUG fprintf(stderr,"OUT %lf %lf\n",*u,*v);*/
}

static double resolution;
static int units, misc, proj;

write_gef_head (ghead, fp)
    struct ghead *ghead;
    FILE *fp;
{
    line1 (ghead, fp);
    line2 (ghead, fp);
    line3_6 (ghead, fp);
}

line1 (ghead, fp)
    struct ghead *ghead;
    FILE *fp;
{
    int in_stat;
    char buf[100], ipath[200], *answer ;

    switch (G_projection()) {
	case 0:				/* XY   */
            strcpy(ghead->proj_name,"Table  ");
            strcpy(ghead->coord_sys,"Inches");
	    break;
	case 3:				/* Lat/Long   */
            strcpy(ghead->proj_name,"Lat/Lon");
            strcpy(ghead->coord_sys,"Degrees");
            proj = PROJECTION_LL;
	    break;
	default:                 /* get input projection parameters */
            G__file_name (ipath, "", PROJECTION_FILE, G_mapset());
            in_proj_keys = G_read_key_value_file(ipath,&in_stat);
            if (in_stat != 0)
               {
               sprintf(buf,"Current mapset PROJ_INFO not correct\n");
               G_fatal_error(buf) ;
               }
            answer = G_find_key_value("proj",in_proj_keys);
	    sprintf(buf,"%s%4d",answer,ghead->plani_zone);
            strcpy(ghead->proj_name,buf);
            strcpy(ghead->coord_sys,G_database_unit_name());
            proj = PROJECTION_OTHER;
	    break;
    }

    fprintf (fp, LINE1_FMT, 
       ghead->map_name, ghead->orig_scale, ghead->proj_name, ghead->area_name);
}

line2 (ghead, fp)
    struct ghead *ghead;
    FILE *fp;
{
    fprintf (fp, LINE2_FMT,
   ghead->organization, ghead->source_date, ghead->data_typ, ghead->coord_sys);
}

line3_6 (ghead, fp)
    struct ghead *ghead;
    FILE *fp;
{
    register int i, cmd_cnt;
    char buff[100], cmnd[100];
    double latSE, latSW, latNE, latNW;
    double lonSE, lonSW, lonNE, lonNW;
    int D1, D2, M1, M2, S1, S2;
    double N, S, E, W;
/********************** FOR USGS PROJ CALLS ***********************/
extern UV (*ProjSetup())();
static char *oform = (char *)0; /* output format */
char *p;

    latSE = latSW = latNE = latNW = 0.0;
    lonSE = lonSW = lonNE = lonNW = 0.0;
    get_ubox(&N, &S, &E, &W);
    oform = "%.8f";

    if (proj == PROJECTION_OTHER)
       {
       cmnd[0] = '\0';
       for (i=1; i<in_proj_keys->nitems-1; i++)
         {
         sprintf(buff,"+%s=%s\t", in_proj_keys->key[i],in_proj_keys->value[i]);
         strcat(cmnd,buff);
         }
       i = in_proj_keys->nitems - 1;
       sprintf(buff,"+%s=%s\0", in_proj_keys->key[i],in_proj_keys->value[i]);
       strcat(cmnd,buff);
       set_proj(cmnd, 1);

       lonNE = E; latNE = N;
       do_INV(&lonNE,&latNE);

       lonSW = W; latSW = S;
       do_INV(&lonSW,&latSW);

       lonNW = W; latNW = N;
       do_INV(&lonNW,&latNW);

       lonSE = E; latSE = S;
       do_INV(&lonSE,&latSE);
       }
    if (proj == PROJECTION_LL)
       {
       latNE = N; lonNE = E;
       latSW = S; lonSW = W;
       latNW = N; lonNW = W;
       latSE = S; lonSE = E;
       }

    DMS(&lonSW, &latSW, &D1,&M1,&S1,&D2,&M2,&S2);
    fprintf (fp, LINE3_FMT, "SW", 
      (double)lonSW, (double)latSW, "LONGITUDE", D1,M1,S1,"LATITUDE", D2,M2,S2);

    DMS(&lonNW, &latNW, &D1,&M1,&S1,&D2,&M2,&S2);
    fprintf (fp, LINE3_FMT, "NW",
      (double)lonNW, (double)latNW, "LONGITUDE", D1,M1,S1,"LATITUDE", D2,M2,S2);

    DMS(&lonNE, &latNE, &D1,&M1,&S1,&D2,&M2,&S2);
    fprintf (fp, LINE3_FMT, "NE", 
      (double)lonNE, (double)latNE, "LONGITUDE", D1,M1,S1,"LATITUDE", D2,M2,S2);

    DMS(&lonSE, &latSE, &D1,&M1,&S1,&D2,&M2,&S2);
    fprintf (fp, LINE3_FMT, "SE", 
      (double)lonSE, (double)latSE, "LONGITUDE", D1,M1,S1,"LATITUDE", D2,M2,S2);
}

get_ubox(rN, rS, rE, rW)
    double *rN, *rS, *rE, *rW;
{
    register int i;
    double N, S, E, W;
    double xtra;
    double ytra;
    P_LINE *Line;

    Line = &(Map.Line[1]);
    N = Line->N; S = Line->S;  /* set init values */
    E = Line->E; W = Line->W;

    for (i = 1 ; i <= Map.n_lines ; i++)
    {
	Line = &(Map.Line[i]);
	if (Line->N > N)
	    N = Line->N;
	if (Line->S < S)
	    S = Line->S;
	if (Line->E > E)
	    E = Line->E;
	if (Line->W < W)
	    W = Line->W;
    }

    xtra = (E - W) * XTRA_PERC;
    ytra = (N - S) * XTRA_PERC;
    *rN = N + ytra;
    *rS = S - ytra;
    *rE = E + xtra;
    *rW = W - ytra;
}

/*  convert lat/lon to dd mm ss.ss */
DMS(LON,LAT,d1,m1,s1,d2,m2,s2) 
double *LAT, *LON;
int *d1, *m1, *d2, *m2, *s1, *s2;
{
      int DEG, MIN, SEC;
      double Q;

      if (*LON < 0.0) *LON = *LON * -1.0;
      DEG = *LON;
      Q = DEG;
      Q = (*LON-Q)*60.;
      MIN = Q;
      SEC = (Q-MIN)*60.;
      if (SEC >= 60)
	 {
	 MIN++; SEC = SEC - 60;
	 }
      if (MIN >= 60)
	 {
	 DEG++; MIN = MIN - 60;
	 }
      *d1 = DEG;
      *m1 = MIN;
      *s1 = SEC;

      DEG = *LAT;
      Q = DEG;
      Q = (*LAT-Q)*60.;
      MIN = Q;
      SEC = (Q-MIN)*60.;
      if (SEC >= 60)
	 {
	 MIN++; SEC = SEC - 60;
	 }
      if (MIN >= 60)
	 {
	 DEG++; MIN = MIN - 60;
	 }
      *d2 = DEG;
      *m2 = MIN;
      *s2 = SEC;
}

set_proj(cmnd,n)
char *cmnd;
int n;
{
char *p;
/*DEBUG fprintf(stderr,"\nSET_UP __ %s\n",cmnd);*/
	p = strtok(cmnd," \t");
        load_opt(p, 0);
        while ((p = strtok(NULL," \t")) != NULL)
                load_opt(p, 0);
        proj1 = ProjSetup(1);
/*      if (n == 1) proj1 = ProjSetup(1);
        else proj2 = ProjSetup(0);*/
}
