/*
**  Original program written by Lubos Mitas, 1989, Bratislava
**  Written by H. Mitasova, I. Kosinovsky, D. Gerdes  Spring 1992
**  US Army Construction Engineering Research Lab
**  Copyright  L. Mitas, H. Mitasova, I. Kosinovsky, D.Gerdes 
*/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include "gis.h"
#include "userglobs.h"
#include "userextern.h"
#include "linkm.h"
#include "bitmap.h"

#include "quad.h"
#include "surf.h"
#include "user.h"



/*
        PROGRAM cs22

   flexible,
   normalized segmented processing, more complete surface analysis
   program for use of STPcr with tension and smoothing

december 1991 running version
control prints in POINT
5-1.91 GRADPAR zmenene
8-1.91 abs in SECPAR 
	curh - is now tangential curvature
*/




double /* pargr */ ns_res, ew_res;
double          dmin,ertre;
int             nsizr, nsizc; 
int             KMAX2, KMIN, KMAX;

double /* datgr */ *az, *adx, *ady, *adxx, *adyy, *adxy;
double /* error */ ertot, ertre,zminac,zmaxac,zmult;
struct quadtree *root;

int             total   = 0;
int             NPOINT = 0;
int             OUTRANGE = 0;
int             NPT = 0;

double          DETERM;
int             NERROR, cond1, cond2;
char            fncdsm[32];
char            filnam[10];

FILE           *fdinp, *fdredinp, *fdzout, *fddxout, *fddyout, *fdxxout,
               *fdyyout, *fd4, *fxyout;
/*
       x,y,z - input data
       npoint - number of input data
       fi - tension parameter
       b - coef. of int. function
       a - matrix of system of linear equations
       az- interpolated values z for output grid
       adx,ady, ... - estimation of derivatives for output grid
       nsizr,nsizc - number of rows and columns for output grid
       xmin ... - coordinates of corners of output grid

       subroutines
       INPUT - input of data x,y,z (test function or measured data)
       SEGMEN - divides region on segments
       COGRR1 - interpolation of z-values and derivatives to grid
       SECPAR- computation of secondary(morphometric) parameters
       OUTGR - output of gridded data and derivatives/sec.parameters
       POINT - interpolation of z-values to given point x,y
*/

char           *input;
char           *mapset = NULL;
char           *elev = NULL;
char           *slope = NULL;
char           *aspect = NULL;
char           *pcurv = NULL;
char           *tcurv = NULL;
char           *mcurv = NULL;
char           *maskmap = NULL;
char           *redinp = NULL;
int            which_att;
int            sdisk,disk;
FILE *Tmp_fd_z = NULL;
char *Tmp_file_z = NULL;
FILE *Tmp_fd_dx = NULL;
char *Tmp_file_dx = NULL;
FILE *Tmp_fd_dy = NULL;
char *Tmp_file_dy = NULL;
FILE *Tmp_fd_xx = NULL;
char *Tmp_file_xx = NULL;
FILE *Tmp_fd_yy = NULL;
char *Tmp_file_yy = NULL;
FILE *Tmp_fd_xy = NULL;
char *Tmp_file_xy = NULL;

struct BM *bitmask;
struct Cell_head cellhd;


int 
main (int argc, char *argv[])
{
    int             per,npmin;
    int             ii, n_rows, n_cols ;
    double          x_orig, y_orig;
    char            dminchar[200];
    struct quaddata *data;
    struct quadfunc *functions;
    struct quadtree *tree;

    struct
    {
	struct Option  *input, *elev, *slope, *aspect, *pcurv, *tcurv, 
		       *mcurv, *which_att, *maskmap, *dmin1, *zmult, 
		       *fi, *rsm, *segmax, *npmin; 
    }               parm;
/*
 struct
  {     struct Flag   *comptop;
        } flag;
    struct
    {
	struct Flag    *per;
    }               flag;
*/

    G_gisinit (argv[0]);

    if (G_get_set_window (&cellhd) == -1)
	exit (0);
    ew_res = cellhd.ew_res;
    ns_res = cellhd.ns_res;
    n_cols = cellhd.cols;
    n_rows = cellhd.rows;
    x_orig = cellhd.west;
    y_orig = cellhd.south;
    dmin = amin1 (ew_res, ns_res) / 2;
    disk = n_rows*n_cols*sizeof(int);
    sdisk = n_rows*n_cols*sizeof(short int);
    sprintf (dminchar, "%f", dmin);
    
/*
    fprintf (stdout,"x0,  y0, %lf,  %lf \n", x_orig, y_orig);
fprintf (stdout,"x0,y0, %lf, %lf \n", cellhd.west, cellhd.south);
*/
   fprintf (stderr, "Version: GRASS4.1, update: 1995, integer output\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "for floating point output use s.surf.rst\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "Authors: original version - L.Mitas, H.Mitasova\n");
    fprintf (stderr, "GRASS implementation: I.Kosinovsky, D.P. Gerdes\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "Please use the following references in publications:\n");
    fprintf (stderr, "Mitasova, H., and  Mitas, L., 1993,\n");
    fprintf (stderr, "Interpolation by Regularized Spline with Tension:\n");
    fprintf (stderr, "I.Theory and implementation. Mathematical Geology, 25, 641 -55.\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "Mitasova, H., and Hofierka, L., 1993\n");
    fprintf (stderr, "Interpolation by Regularized Spline with Tension:\n");
    fprintf (stderr, "II. Application to terrain modeling and surface geometry a nalysis.\n");
    fprintf (stderr, "Mathematical Geology, 25, 657-69.\n");
    fprintf (stderr, "\n");

    parm.input = G_define_option ();
    parm.input->key = "input";
    parm.input->type = TYPE_STRING;
    parm.input->required = YES;
    parm.input->gisprompt = "old,site_lists,sites";
    parm.input->description = "Name of the site file with input x,y,z";


    parm.elev = G_define_option ();
    parm.elev->key = "elev";
    parm.elev->type = TYPE_STRING;
    parm.elev->required = NO;
    parm.elev->gisprompt = "new,cell,raster";
    parm.elev->description = "Output z-file (elevation)";

    parm.slope = G_define_option ();
    parm.slope->key = "slope";
    parm.slope->type = TYPE_STRING;
    parm.slope->required = NO;
    parm.slope->gisprompt = "new,cell,raster";
    parm.slope->description = "Slope";

    parm.aspect = G_define_option ();
    parm.aspect->key = "aspect";
    parm.aspect->type = TYPE_STRING;
    parm.aspect->required = NO;
    parm.aspect->gisprompt = "new,cell,raster";
    parm.aspect->description = "Aspect";


    parm.pcurv = G_define_option ();
    parm.pcurv->key = "pcurv";
    parm.pcurv->type = TYPE_STRING;
    parm.pcurv->required = NO;
    parm.pcurv->gisprompt = "new,cell,raster";
    parm.pcurv->description = "Profile curvature";


    parm.tcurv = G_define_option ();
    parm.tcurv->key = "tcurv";
    parm.tcurv->type = TYPE_STRING;
    parm.tcurv->required = NO;
    parm.tcurv->gisprompt = "new,cell,raster";
    parm.tcurv->description = "Tangential curvature";

    parm.mcurv = G_define_option ();
    parm.mcurv->key = "mcurv";
    parm.mcurv->type = TYPE_STRING;
    parm.mcurv->required = NO;
    parm.mcurv->gisprompt = "new,cell,raster";
    parm.mcurv->description = "Mean curvature";

    parm.maskmap = G_define_option ();
    parm.maskmap->key = "maskmap";
    parm.maskmap->type = TYPE_STRING;
    parm.maskmap->required = NO;
    parm.maskmap->gisprompt = "old,cell,raster";
    parm.maskmap->description = "Name of the raster file used as mask";

    parm.dmin1 = G_define_option ();
    parm.dmin1->key = "dmin";
    parm.dmin1->type = TYPE_DOUBLE;
    parm.dmin1->answer = dminchar;
    parm.dmin1->required = NO;
    parm.dmin1->description = "Min distance between points (extra points ignored)";

    parm.zmult = G_define_option ();
    parm.zmult->key = "zmult";
    parm.zmult->type = TYPE_DOUBLE;
    parm.zmult->answer = ZMULT;
    parm.zmult->required = NO;
    parm.zmult->description = "Conversion factor for z-values";

    parm.fi = G_define_option ();
    parm.fi->key = "tension";
    parm.fi->type = TYPE_DOUBLE;
    parm.fi->answer = TENSION;
    parm.fi->required = NO;
    parm.fi->description = "Tension";

    parm.rsm = G_define_option ();
    parm.rsm->key = "smooth";
    parm.rsm->type = TYPE_DOUBLE;
    parm.rsm->answer = SMOOTH;
    parm.rsm->required = NO;
    parm.rsm->description = "Smoothing parameter";

    parm.segmax = G_define_option ();
    parm.segmax->key = "segmax";
    parm.segmax->type = TYPE_INTEGER;
    parm.segmax->answer = MAXSEGM;
    parm.segmax->required = NO;
    parm.segmax->description = "Max number of points in segment";

    parm.npmin = G_define_option ();
    parm.npmin->key = "npmin";
    parm.npmin->type = TYPE_INTEGER;
    parm.npmin->answer = MINPOINTS;
    parm.npmin->required = NO;
    parm.npmin->description = "Min number of points for interpolation(>segmax)";

    parm.which_att = G_define_option();
    parm.which_att->key = "attribute";
    parm.which_att->type = TYPE_INTEGER;
    parm.which_att->multiple = NO;
    parm.which_att->required = NO;
    parm.which_att->description = "Attribute in sites file to interpolate from";
    parm.which_att->options = "1-100000";
    

/*
	flag.comptop = G_define_flag();
	flag.comptop->key = 'c';
        flag.comptop->description = "Compute topographic parameters";
*/
    /*
    flag.per = G_define_flag ();
    flag.per->key = 'p';
    flag.answer = 1;
    flag.per->description = "Display percentage completed";
    */
/*****************end of text changes********************/

    if (G_parser (argc, argv))
	exit (1);
/*
        iw2=flag.comptop->answer;
*/
    per = 1;  /*flag.per->answer; */
    iw2 = 1;
    input = parm.input->answer;
    maskmap = parm.maskmap->answer;
/*
        redinp = parm.redinp->answer;
*/
    elev = parm.elev->answer;
    slope = parm.slope->answer;
    aspect = parm.aspect->answer;
    pcurv = parm.pcurv->answer;
    tcurv = parm.tcurv->answer;
    mcurv = parm.mcurv->answer;
    if (parm.which_att->answer)
	sscanf (parm.which_att->answer, "%d", &which_att);
    else 
	which_att = -1;  
	/* defaults to use Z dim if there is one, else cat, else first att */
    

    cond2 = ((pcurv != NULL) || (tcurv != NULL) || (mcurv != NULL) );
    cond1 = ((slope != NULL) || (aspect != NULL) ||  cond2);

    ertre = 0.1;
    sscanf (parm.dmin1->answer, "%lf", &dmin);
    sscanf (parm.fi->answer, "%lf", &fi);
    sscanf (parm.rsm->answer, "%lf", &rsm);
    sscanf (parm.segmax->answer, "%d", &KMAX);
    sscanf (parm.npmin->answer, "%d", &npmin);
    sscanf (parm.zmult->answer, "%lf", &zmult);
    KMAX2 = MAXPOINTS;

/************************************/
    KMIN = npmin;
/***************        KMAX2 = GRADPARAM1*npmax;***************/
    az = (double *) malloc (sizeof (double) * (n_cols + 1));
    if (! az) {
      G_fatal_error ("Not enough memory for az");
    }
    if (cond1) {
      adx = (double *) malloc (sizeof (double) * (n_cols + 1));
      if (! adx) {
        G_fatal_error ("Not enough memory for adx");
      }
      ady = (double *) malloc (sizeof (double) * (n_cols + 1));
      if (! ady) {
        G_fatal_error ("Not enough memory for ady");
      }
      if (cond2) {
        adxx = (double *) malloc (sizeof (double) * (n_cols + 1));
        if (! adxx) {
          G_fatal_error ("Not enough memory for adxx");
        }
        adyy = (double *) malloc (sizeof (double) * (n_cols + 1));
        if (! adyy) {
          G_fatal_error ("Not enough memory for adyy");
        }
        adxy = (double *) malloc (sizeof (double) * (n_cols + 1));
        if (! adxy) {
          G_fatal_error ("Not enough memory for adxy");
        }
      }
    }
    if ((data = data_new (x_orig, y_orig, n_rows, n_cols, 0)) == NULL)
    {
	fprintf (stderr, "cannot create quaddata\n");
	exit (0);
    }
    if ((functions = QT_functions_new (quad_compare,quad_divide_data,quad_add_data,quad_intersect,quad_division_check,quad_get_points)) == NULL)
    {
	fprintf (stderr, "cannot create quadfunc\n");
	exit (0);
    }
    if ((tree = QT_tree_new (data, NULL, NULL, NULL, NULL, NULL, functions, 0)) == NULL)
    {
	fprintf (stderr, "cannot create quadtree\n");
    }
    root = tree;


/*    if (TESTOUT)
    {
	if ((fd4 = fopen ("testout", "w")) == NULL)
	{
	    G_fatal_error ("Cannot open testout");
	}
    }
*/
    mapset = G_find_file ("site_lists", input, "");
    if (mapset == NULL)
    {
	sprintf (msg, "file [%s] not found", input);
	G_fatal_error (msg);
    }
    if ((fdinp = G_fopen_sites_old (input, mapset)) == NULL)
    {
	sprintf (msg, "Cannot open %s", input);
	G_fatal_error (msg);
    }

/**************tables for faster function*****************/
     lntab();
     s_table();
/************************************************************/

    ii=INPUT();
    if (ii>0)
    {
	ertot = 0.;
	if (per)
	    fprintf (stderr, "Percent complete: ");

        if (elev != NULL) {
	  Tmp_file_z = G_tempfile ();
	  if (NULL == (Tmp_fd_z = fopen (Tmp_file_z, "w+"))) {
	    sprintf (msg, "Can't open temp file [%s] ", Tmp_file_z);
	    G_fatal_error (msg);
          }
        }
        if (slope != NULL) {
	  Tmp_file_dx = G_tempfile ();
	  if (NULL == (Tmp_fd_dx = fopen (Tmp_file_dx, "w+"))) {
	    sprintf (msg, "Can't open temp file [%s] ", Tmp_file_dx);
	    G_fatal_error (msg);
          }
        }
        if (aspect != NULL) {
	  Tmp_file_dy = G_tempfile ();
	  if (NULL == (Tmp_fd_dy = fopen (Tmp_file_dy, "w+"))) {
	    sprintf (msg, "Can't open temp file [%s] ", Tmp_file_dy);
	    G_fatal_error (msg);
          }
        }
        if (pcurv != NULL) {
          Tmp_file_xx = G_tempfile ();
	  if (NULL == (Tmp_fd_xx = fopen (Tmp_file_xx, "w+"))) {
	    sprintf (msg, "Can't open temp file [%s] ", Tmp_file_xx);
	    G_fatal_error (msg);
          }
        }
        if (tcurv != NULL) {
          Tmp_file_yy = G_tempfile ();
	  if (NULL == (Tmp_fd_yy = fopen (Tmp_file_yy, "w+"))) {
	    sprintf (msg, "Can't open temp file [%s] ", Tmp_file_yy);
	    G_fatal_error (msg);
          }
        }
        if (mcurv != NULL) {
          Tmp_file_xy = G_tempfile ();
	  if (NULL == (Tmp_fd_xy = fopen (Tmp_file_xy, "w+"))) {
	    sprintf (msg, "Can't open temp file [%s] ", Tmp_file_xy);
	    G_fatal_error (msg);
          }
        }
        cursegm = 0;
	if (interp_call (root,root))
	{
	    if (fd4 != NULL)
		fprintf (fd4, "max. error found = %f \n", ertot);

	    free (root);
            free(az);
            if (cond1) {
              free(adx);
              free(ady);
              if(cond2) {
                free(adxx);
                free(adyy);
                free(adxy);
              }
            }
            if (elev != NULL) fclose(Tmp_fd_z);
            if (slope != NULL) fclose(Tmp_fd_dx);
            if (aspect != NULL) fclose(Tmp_fd_dy);
            if (pcurv != NULL) fclose(Tmp_fd_xx);
            if (tcurv != NULL) fclose(Tmp_fd_yy);
            if (mcurv != NULL) fclose(Tmp_fd_xy);
	    if(OUTGR () < 0) {
               G_fatal_error("Cannot write cell files -- try to increase resolution"); 
            }
            if (elev != NULL) unlink(Tmp_file_z);
            if (slope != NULL) unlink(Tmp_file_dx);
            if (aspect != NULL) unlink(Tmp_file_dy);
            if (pcurv != NULL) unlink(Tmp_file_xx);
            if (tcurv != NULL) unlink(Tmp_file_yy);
            if (mcurv != NULL) unlink(Tmp_file_xy);

/*       QT_print_tree(root,y_orig+n_rows,y_orig,x_orig+n_cols,x_orig); */
 

	    fprintf (stderr, "\n");
	    fprintf (stderr, "The number of points in sites file is %d\n", NPT);
	    fprintf (stderr, "The number of points outside of region %d\n", OUTRANGE);
	    fprintf (stderr, "The number of points used (after reduction) is %d\n", NPOINT);

	}
	else
	    exit (0);
    }
    else
	exit (0);
    fclose (fd4);
    fclose (fdinp);

    return 0;
}
