/*
* $Id$
*
* Copyright (C) 2000 by the GRASS Development Team
* Author: Bob Covill <bcovill@tekmap.ns.ca>
* 
* This Program is free software under the GPL (>=v2)
* Read the file COPYING coming with GRASS for details
*
*
*/

#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "display.h"
#include <math.h>

static int move(int,int);
static int cont(int,int);
static int min_range[5], max_range[5];
static int which_range;
static int change_range;
static double dist, e=0, n=0;

main(argc, argv) 
int   argc;
char *argv[];
{ 
    char *name, *outfile, *mapset, msg[256] ;
    int fd, projection;
    char buf[50], hed[256];
    FILE *fp;
    int screen_x, screen_y, screen_x1, screen_y1, button;
    double res;
    char errbuf[256];
    int coords=0, p, i, k;
    int cnt=1;
    double e1, e2, n1, n2;
    double A, E;
    RASTER_MAP_TYPE data_type;
    CELL *cell;
    FCELL *fcell;
    DCELL *dcell;
    struct Cell_head window;
    struct {
	struct Option *opt1, *profile, *res, *output;
	struct Flag *i, *g ;
	} parm;

G_gisinit (argv[0]);
    parm.opt1 = G_define_option() ;
    parm.opt1->key        = "input" ;
    parm.opt1->type       = TYPE_STRING ;
    parm.opt1->required   = YES ;
    parm.opt1->multiple   = NO;
    parm.opt1->gisprompt  = "old,cell,raster" ;
    parm.opt1->description= "Name of existing raster map" ;

    parm.output = G_define_option() ;
    parm.output->key        = "output";
    parm.output->type       = TYPE_STRING;
    parm.output->required   = YES;
    parm.output->gisprompt  = "old,cell,raster" ;
    parm.output->description= "Name of file for output (use out=- for stdout)" ;

    parm.profile= G_define_option() ;
    parm.profile->key        = "profile" ;
    parm.profile->type       = TYPE_STRING ;
    parm.profile->required   = NO;
    parm.profile->multiple   = YES ;
    parm.profile->key_desc = "east,north";
    parm.profile->description= "Profile Coordinate Pairs" ;

    parm.res= G_define_option() ;
    parm.res->key        = "res" ;
    parm.res->type       = TYPE_DOUBLE;
    parm.res->required   = NO;
    parm.res->description= "Resolution along profile (default = current region resolution)" ;


    parm.i= G_define_flag() ;
    parm.i->key         = 'i' ;
    parm.i->description = "Interactively select End-Points" ;

    parm.g = G_define_flag() ;
    parm.g->key         = 'g' ;
    parm.g->description = "Output Geographic Coordinates" ;


    if (G_parser(argc, argv))
        exit(-1);

if ((!parm.i->answer) && (!parm.profile->answer))
{
 sprintf(msg, "Either -i flag and/or profile parameter must be used.");
 G_fatal_error (msg);
}

G_get_window(&window);
projection = G_projection();
if (parm.res->answer) {
 res = atof(parm.res->answer);
 /* Catch bad resolution ? */
 if (res == 0) {
  sprintf(msg, "ILLEGAL Resolution!\n");
  G_fatal_error (msg);
 }
} else {
 /* Do average of EW and NS res */
 res = (window.ew_res + window.ns_res) / 2;
}
screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;

fprintf(stderr, "Using Resolution %f\n", res);

G_begin_distance_calculations();

/* Open Input File for reading */
/* Get Input Name */
name = parm.opt1->answer;
if(parm.g->answer) coords=1;

/* Open Raster File*/
if(NULL == (mapset = G_find_cell2 (name, ""))) {
sprintf(msg, "Cannot find mapset for %s \n", name);
G_fatal_error (msg);
}
if(0 > (fd = G_open_cell_old (name, mapset))) {
sprintf(msg, "Cannot open File %s\n", name);
G_fatal_error (msg);
}

/* Open ASCII file for output or stdout */
outfile =  parm.output->answer;

if((strcmp("-", outfile)) == 0) {
fp = stdout; 
} else if (NULL == (fp = fopen(outfile, "w"))) {
sprintf(errbuf,"Not able to open file for [%s]", outfile);
G_fatal_error(errbuf);
}

/* Get Raster Type */
data_type=G_raster_map_type(name, mapset);
/* Done with file */

/* Get coords */
if ( parm.i->answer) {
R_open_driver();
D_setup(0);

G_setup_plot ( D_get_d_north(),
                   D_get_d_south(),
                   D_get_d_west(),
                   D_get_d_east(),
                   move, cont);
}

/* Show message giving output format */
fprintf(stderr, "Output Format:\n");
if ( coords == 1) {
fprintf(stderr, "[Easting] [Northing] [Along Track Dist.(m)] [Elevation]\n\n");
} else {
fprintf(stderr, "[Along Track Dist.(m)] [Elevation]\n\n");
}

/* Get Profile Start Coords */
if (parm.i->answer) {
/* Select points interactively*/

dist = 0;

fprintf(stderr, "\n\n");
fprintf(stderr, "Use mouse to select Start Point\n");
R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
	e1 = D_d_to_u_col((double)screen_x)  ;
	n1 = D_d_to_u_row((double)screen_y) ;

fprintf(stderr, "\nUse mouse to draw profile line\n");
fprintf(stderr, "Buttons:\n") ;
fprintf(stderr, "Left:   Mark next point\n");
fprintf(stderr, "Middle: Mark next point\n");
fprintf(stderr, "Right:  Finish profile and exit\n\n");

while (button != 3) {
R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
if (button == 1 || button == 2 ) {
        e2 = D_d_to_u_col((double)screen_x) ;
        n2 = D_d_to_u_row((double)screen_y) ;
} else {
break;
}

/* draw line from p1 to p2 */
G_plot_line (e1, n1, e2, n2);

/* Get profile info */
do_profile(e1,e2,n1,n2,name,coords,res,fd,data_type,fp);

n1 = n2;
e1 = e2;
}

R_close_driver();
} else {
/* Coords from Command Line */
for (i=0; parm.profile->answers[i]; i+=2) {
/* Test for number coordinate pairs */
k = i;
}

if (k == 0) {
/* Only one coordinate pair supplied */
sscanf(parm.profile->answers[0], "%lf",&e1);
sscanf(parm.profile->answers[1], "%lf",&n1);
e2 = e1;
n2 = n1;
/* Get profile info */
do_profile(e1,e2,n1,n2,name,coords,res,fd,data_type,fp);
} else {
for (i=0; i <= k-2 ; i+=2) {
sscanf(parm.profile->answers[i], "%lf",&e1);
sscanf(parm.profile->answers[i+1], "%lf",&n1);
sscanf(parm.profile->answers[i+2], "%lf",&e2);
sscanf(parm.profile->answers[i+3], "%lf",&n2) ;
/* Get profile info */
do_profile(e1,e2,n1,n2,name,coords,res,fd,data_type,fp);

/* Ge last coord */
if (i == k-2)
do_profile(e2,e2,n2,n2,name,coords,res,fd,data_type,fp);
} }
}

G_close_cell(fd);
fclose(fp);

} /* Done with main */

/* Claculate the Profile Now */
/* Establish parameters */
int do_profile 
(double e1, 
double e2, 
double n1, 
double n2, 
char *name, 
int coords, 
double res,
int fd,
int data_type,
FILE *fp)
{
float rows, cols, LEN;
double Y, X, AZI;

cols = e1 - e2;
rows = n1 - n2;

LEN = G_distance(e1,n1,e2,n2);
fprintf(stderr, "Approx. transect length %f m.\n", LEN);

/* Calculate Azimuth of Line */
if (rows == 0 && cols == 0) {
/* Special case for no movement */
e = e1;
n = n1;
read_rast (e, n, dist, fd, coords, data_type, fp);
}

if (rows >= 0 && cols < 0) {
/* SE Quad or due east */
AZI = atan((rows/cols));
Y = res * sin(AZI ) ;
X = res * cos(AZI );
if (Y < 0) Y = Y*-1.;
if (X < 0) X = X*-1.;
if ( e != 0.0 && (e != e1 || n != n1)) {
dist -= G_distance(e,n,e1,n1) ;
}
for (e = e1, n = n1; e < e2 || n > n2; e+=X, n-=Y) {
read_rast (e, n, dist, fd, coords, data_type, fp);
/* d+=res; */
dist+=G_distance(e-X, n+Y, e, n);
}
}

if (rows < 0 && cols <= 0) {
/* NE Quad  or due north */
AZI = atan((cols/rows));
X = res * sin(AZI ) ;
Y = res * cos(AZI );
if (Y < 0) Y = Y*-1.;
if (X < 0) X = X*-1.;
if ( e != 0.0 && (e != e1 || n != n1)) {
dist -= G_distance(e,n,e1,n1) ;
/*
read_rast (e1, n1, dist, fd, coords, data_type, fp);
*/
}
for (e = e1, n = n1; e < e2 || n < n2; e+=X, n+=Y) {
read_rast (e, n, dist, fd, coords, data_type, fp);
/* d+=res; */
dist+=G_distance(e-X, n-Y, e, n);
}
}

if (rows > 0 && cols >= 0) {
/* SW Quad or due south */
AZI = atan((rows/cols));
X = res * cos(AZI ) ;
Y = res * sin(AZI );
if (Y < 0) Y = Y*-1.;
if (X < 0) X = X*-1.;
if ( e != 0.0 && (e != e1 || n != n1)) {
dist -= G_distance(e,n,e1,n1) ;
}
for (e = e1, n = n1; e > e2 || n > n2; e-=X, n-=Y) {
read_rast (e, n, dist, fd, coords, data_type, fp);
/* d+=res; */
dist+=G_distance(e+X, n+Y, e, n);
}
}

if (rows <= 0 && cols > 0) {
/* NW Quad  or due west */
AZI = atan((rows/cols));
X = res * cos(AZI ) ;
Y = res * sin(AZI );
if (Y < 0) Y = Y*-1.;
if (X < 0) X = X*-1.;
if ( e != 0.0 && (e != e1 || n != n1)) {
dist -= G_distance(e,n,e1,n1) ;
}
for (e = e1, n = n1; e > e2 || n < n2; e-=X, n+=Y) {
read_rast (e, n, dist, fd, coords, data_type, fp);
/* d+=res; */
dist+=G_distance(e+X, n-Y, e, n);
}
}
/*
return dist;
*/
} /* done with do_profile */

static int move(int x,int y)
{
    D_move_abs(x,y);

    return 0;
}

static int cont(int x,int y)
{
    if(D_cont_abs (x, y)) /* clipped */
    {
        change_range = 1;
    }
    else  /* keep track of left,right x for lines drawn in window */
    {
        if (change_range)
        {
            which_range++;
            min_range[which_range] = max_range[which_range] = x;
            change_range = 0;
        }
        else
        {
            if (x < min_range[which_range])
                min_range[which_range] = x;
            else if (x > max_range[which_range])
                max_range[which_range] = x;
        }
    }

    return 0;
}
