/* %W% %G% */
/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/
#include <string.h>
#include "gis.h"
#include "digit.h"
#include "export_dlg.h"

#define LINE1_FMT  "%-30.30s %-20.20s %-20.20s\n"
#define LINE2_FMT  "%-40.40s %-10.10s%c%8ld            \n"
#define LINE3_FMT  "Produced by GRASS v.export-DLG                                          \n"
#define LINE4_FMT  "%6d%6d%6d%6d%-18s%6d%6d%6d%6d      \n"
#define LINE5_FMT  "%-24s%-24s%-24s\n"
#define LINE10_FMT "%18s%18s%18s%18s\n"
#define LINE11_FMT "%2s    %12.6lf%12.6lf      %12.2lf%12.2lf            \n"
#define LINE15_FMT "%20s%4d%6d%6d %c%c %6d%6d %c%c%c%6d%6d   %c\n"

#define XTRA_PERC  0.05

/********************* PARMS FOR PROJ **********************/
# define RAD_TO_DEG     57.29577951308232
typedef struct { double u, v; }  UV;

/**** routines for projection support ********/
static UV (*proj1)();      /* projection function 1*/
static UV (*proj2)();      /* projection function 2*/

do_INV(u,v) double *u, *v;
{
 UV data;    /*         data structure supplied to proj conv. routines */
  /*DEBUG   fprintf(stderr,"IN1 %lf %lf\n",*u,*v); */
	 data.u = *u; data.v = *v;
	 data = (*proj1)(data);
	 *u = data.u * RAD_TO_DEG; *v = data.v * RAD_TO_DEG;
/*DEBUG   fprintf(stderr,"OUT %lf %lf\n",*u,*v);*/
}

static double resolution;
static int units, misc, proj;
struct Key_Value *proj_keys;

char *dtype ();
write_dlg_head (map, d_head, fp)
    struct Map_info *map;
    struct dig_head *d_head;
    FILE *fp;
{
    line1 (map, d_head, fp);
    line2 (map, d_head, fp);
    line3 (map, d_head, fp);
    line4 (map, d_head, fp);
    line5_9 (map, d_head, fp);
    line10 (map, d_head, fp);
    line11_14 (map, d_head, fp);
    line15 (map, d_head, fp);
}

line1 (map, d_head, fp)
    struct Map_info *map;
    struct dig_head *d_head;
    FILE *fp;
{
    fprintf (fp, LINE1_FMT, d_head->organization, d_head->source_date, d_head->your_name);
}

line2 (map, d_head, fp)
    struct Map_info *map;
    struct dig_head *d_head;
    FILE *fp;
{
    if (G_projection() != 0 && G_projection() != 3 )
       fprintf (fp, LINE2_FMT, d_head->map_name, d_head->date, ' ', d_head->orig_scale);
    else
       fprintf (fp, LINE2_FMT, d_head->map_name, d_head->date, ' ', 1);
}

#define UNIT_FILE "PROJ_UNITS"
#define PROJECTION_FILE "PROJ_INFO"

line3 (map, d_head, fp)
    struct Map_info *map;
    struct dig_head *d_head;
    FILE *fp;
{
    int i, status;
    char path[100], buf[50];
    /*
    fprintf (fp, LINE3_FMT);
    */
    /*
    if (strlen (d_head->line_3) > 72)
	d_head->line_3[73] = 0;
    fprintf (fp, "%-72.72s\n", d_head->line_3);
    */
    if (G_projection() != 0 && G_projection() != 3 )
       {
           /* get input projection parameters */
       G__file_name (path, "", PROJECTION_FILE, G_mapset());
       if (access(path,0) != 0)
          {
          sprintf(path,"Mapset %s file not found, run g.setproj\n",
				   PROJECTION_FILE);
          G_fatal_error(path) ;
          }
       proj_keys = G_read_key_value_file(path,&status);
       if (status != 0)
          {
          sprintf(path,"Current mapset %s not found\n",PROJECTION_FILE);
          G_fatal_error(path) ;
          }

           /* read values and create line_3 proj info */
       path[0] = '\0';
       for (i=1; i<=proj_keys->nitems-1; i++)
	  {
	  sprintf(buf,"%s=%s ",
	  proj_keys->key[i],proj_keys->value[i]);
	  strcat(path,buf);
	  if (strlen(path) > 72)
	     {
	     sprintf(path,"%s",proj_keys->value[0]);
	     break;
	     }
	  }
       fprintf (fp, "%-72.72s\n", path);
       }
    else
       {
       if (strlen (d_head->line_3) > 72)
	   d_head->line_3[73] = 0;
       fprintf (fp, "%-72.72s\n", d_head->line_3);
       }
}

#define DLG3_CODE 	3

#define NUM_TRANSFORMS	4
#define NUM_SIDES	4
#define NUM_CATS	1
#define DIG_UNITS	0.001


line4 (map, d_head, fp)
    struct Map_info *map;
    struct dig_head *d_head;
    FILE *fp;
{
    int status, line4_proj;
    char mesg[30], *unit_type;
    double factr, G_database_units_to_meters_factor(); 

    switch (G_projection()) {
	case 0:				/* XY   */
	    proj = line4_proj = PROJECTION_XY;
	    units = 0 ;
            resolution = 1.0; 
	    break;
	case 3:				/* Lat/Long   */
	    proj = line4_proj = PROJECTION_LL;
	    units = DEGREES;
            resolution = 1.0; 
	    break;
	default:			/* other projections */
	    line4_proj = 0;
	    if (strcmp(proj_keys->value[2],"utm") == 0) line4_proj = 1;
	    if (strcmp(proj_keys->value[2],"aea") == 0) line4_proj = 3;
	    if (strcmp(proj_keys->value[2],"lcc") == 0) line4_proj = 5;
	    if (strcmp(proj_keys->value[2],"merc") == 0) line4_proj = 6;
	    if (strcmp(proj_keys->value[2],"tmerc") == 0) line4_proj = 7;
	    if (strncmp(proj_keys->value[0],"State",5) == 0) line4_proj = 2;
	    if (line4_proj == 0) line4_proj = PROJECTION_OTHER;
	    proj = PROJECTION_OTHER;
	    unit_type = G_database_unit_name(0);
	    if ((*unit_type == 'u') || (*unit_type == 'U'))
	        unit_type = G_database_unit_name(1);

		       /* USGS spec, says meters is code 2, 
			  GRASS code says meters are code 1 */
	    if ((*unit_type == 'm') || (*unit_type == 'M'))
	         units = 2;    /* meters */
            else units = 1;    /* feet   */
            factr = G_database_units_to_meters_factor();
            if (factr == 0.0)
	       {
	       if (units == 1) factr = .3048;
	       else factr = 1.0;
	       }
            resolution = d_head->orig_scale * factr * DIG_UNITS; 
	    break;
    }

    misc = 0;
/*  resolution = d_head->orig_scale * dig_unit_conversion () * DIG_UNITS; */

    fprintf (fp, LINE4_FMT, 
	DLG3_CODE, 		/* DLG-3 format */
	line4_proj, 		/* Projection  */
	d_head->plani_zone,	/* ZONE */
	units, 			/* Units */
	dtype (resolution, 18, 11), 		/* resolution */
	NUM_TRANSFORMS, 	/* # of tranformation params */
	misc, 			/* # of accuracy/misc records */
	NUM_SIDES, 		/* # of sides to window polygon */
	NUM_CATS);		/* # of DLG categories */
}

line5_9 (map, d_head, fp)
    struct Map_info *map;
    struct dig_head *d_head;
    FILE *fp;
{
    register int i;

    for (i = 5 ; i <= 9 ; i++)
    {
	switch (i) {
	    case 5:
		fprintf (fp, LINE5_FMT, dtype(0.0, 14, 15), dtype(0.0, 14, 15), dtype(0.0, 14, 15));
		break;
	    default:
		fprintf (fp, LINE5_FMT, dtype(0.0, 14, 15), dtype(0.0, 14, 15), dtype(0.0, 14, 15));
		break;
	}
    }
}

line10 (map, d_head, fp)
    struct Map_info *map;
    struct dig_head *d_head;
    FILE *fp;
{
    fprintf (fp, LINE10_FMT, dtype(1.0, 18, 11), dtype(0.0, 18, 11), dtype(0.0, 18, 11), dtype(0.0, 18, 11));
}

line11_14 (map, d_head, fp)
    struct Map_info *map;
    struct dig_head *d_head;
    FILE *fp;
{
    register int i, cmd_cnt;
    char buff[100], cmnd[256];
    double latNE, latNW, lonNE, lonNW;
    double latSE, latSW, lonSE, lonSW;
    double N, S, E, W;

/******************** FOR USGS PROJ CALLS ****************************/
extern UV (*ProjSetup())();
static char *oform = (char *)0; /* output format */
char *p;

    oform = "%.8f";
    latSE = latSW = latNE = latNW = 0.0;
    lonSE = lonSW = lonNE = lonNW = 0.0;
    get_ubox(map, &N, &S, &E, &W);

           /* build the proj command from data in PROJ_INFO file */
    if (proj == PROJECTION_OTHER)
       {
       cmnd[0] = '\0';
       for (i=1; i<proj_keys->nitems-1; i++)
         {
         sprintf(buff,"+%s=%s\t", proj_keys->key[i],proj_keys->value[i]);
         strcat(cmnd,buff);
         }
       i = proj_keys->nitems - 1;
       sprintf(buff,"+%s=%s\0", proj_keys->key[i],proj_keys->value[i]);
       strcat(cmnd,buff);

             /* compute the lat/long for each corner */
       set_proj(cmnd,1);

       lonNE = E; latNE = N;
       do_INV(&lonNE,&latNE);

       lonSW = W; latSW = S;
       do_INV(&lonSW,&latSW);

       lonNW = W; latNW = N;
       do_INV(&lonNW,&latNW);

       lonSE = E; latSE = S;
       do_INV(&lonSE,&latSE);
       G_free_key_value(proj_keys);
       }
    else
       {
       lonNE = N; latNE = E;
       lonSW = S; latSW = W;
       lonNW = N; latNW = W;
       lonSE = S; latSE = E;
       }
    fprintf (fp, LINE11_FMT, "SW", (double)latSW, (double)lonSW, W, S);
    fprintf (fp, LINE11_FMT, "NW", (double)latNW, (double)lonNW, W, N);
    fprintf (fp, LINE11_FMT, "NE", (double)latNE, (double)lonNE, E, N);
    fprintf (fp, LINE11_FMT, "SE", (double)latSE, (double)lonSE, E, S);
}

line15 (map, d_head, fp)
    struct Map_info *map;
    struct dig_head *d_head;
    FILE *fp;
{
    fprintf (fp, LINE15_FMT, 
	"",			/* Category name */
	0, 			/* Default formating Maj/Min pairs */
	map->n_nodes + 1,	/* # of nodes in file */
	map->n_nodes + 1,	/* # of nodes in file */
	'0',			/* node-area links */
	'1',			/* node-line links */
	map->n_areas + 2,	/* # of areas in file */
	map->n_areas + 2,	/* # of areas in file */
	'0',			/* area-node links */
	'1',			/* area-line links */
	'0',			/* area-coord lists */
	map->n_lines + 1,	/* # of lines in file */
	map->n_lines + 1,	/* # of lines in file */
	'1');			/* line-coord lists */
}

/* uses a circular buffer of 200 bytes */
char *
dtype(value, width, deci)
        double value ;
        int width ;
        int deci ;
{
    static char buff[200];
    static char *p = NULL;
    char *ret;
    char form[64] ;
    char *strchr() ;

    if (p == NULL)
	p = buff;
    if ((buff+200-1) - p < width+1)
	p = buff;
    
    {
	sprintf(form,"%%%d.%dle",width, deci) ;
	sprintf(p,form,value) ;
	*(strchr(p,'e')) = 'D' ;
    }
    ret = p;
    p += strlen (p) + 1;
    return (ret);
}

get_ubox(map, rN, rS, rE, rW)
    struct Map_info *map;
    double *rN, *rS, *rE, *rW;
{
    register int i;
    double N, S, E, W;
    double xtra;
    double ytra;
    P_LINE *Line;

    Line = &(map->Line[1]);
    N = Line->N; S = Line->S;  /* get init values */
    E = Line->E; W = Line->W;

    for (i = 1 ; i <= map->n_lines ; i++)
    {
	Line = &(map->Line[i]);
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
