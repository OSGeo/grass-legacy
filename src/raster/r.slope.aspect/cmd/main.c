#include <stdlib.h>
#include <math.h>
#include "gis.h"
#include "local_proto.h"

/* 10/99 from GMSL, updated to new GRASS 5 code style , changed default "prec" to float*/

#define abs(x) ((x)<0?-(x):(x))

/**************************************************************************
 * input is from command line.
 * arguments are elevation file, slope file, aspect file, profile curvature
 * file and tangential curvature file
 * elevation filename required
 * either slope filename or aspect filename or profile curvature filename
 * or tangential curvature filename required
 * usage: r.slope.aspect [-av] elevation=input slope=output1 aspect=output2
 *	pcurv=output3 tcurv=output4 format=name prec=name zfactor=value
 *	min_slp_allowed=value dx=output5 dy=output6 dxx=output7 
 *      dyy=output8 dxy=output9
 * -a don't align window
 * -q quiet
 **************************************************************************/

/*  some changes made to code to retrieve correct distances when using
    lat/lon projection.  changes involve recalculating H and V. see
    comments within code.                                           */


int main (int argc, char *argv[])
{
    struct Categories cats;
    int verbose;
    int align;
    int elevation_fd;
    int aspect_fd ;
    int slope_fd ;
    int pcurv_fd ;
    int tcurv_fd ;
    int dx_fd ;
    int dy_fd ;
    int dxx_fd ;
    int dyy_fd ;
    int dxy_fd ;
    DCELL *elev_cell[3], *temp;
    DCELL *c1, *c2, *c3, *c4, *c5, *c6, *c7, *c8, *c9;
    DCELL tmp1, tmp2;
    void * asp_raster, *asp_ptr;
    void * slp_raster, *slp_ptr;
    void * pcurv_raster, *pcurv_ptr;
    void * tcurv_raster, *tcurv_ptr;
    void * dx_raster, *dx_ptr ;
    void * dy_raster, *dy_ptr ;
    void * dxx_raster, *dxx_ptr ;
    void * dyy_raster, *dyy_ptr ;
    void * dxy_raster, *dxy_ptr ;
    int i;
    RASTER_MAP_TYPE out_type, data_type;
    int Wrap;  /* global wraparound */
    struct Cell_head window, cellhd;
    struct History  hist;
    double ceil();

    char *elev_name;
    char *aspect_name;
    char *slope_name;
    char *pcurv_name;
    char *tcurv_name;
    char *dx_name;
    char *dy_name;
    char *dxx_name;
    char *dyy_name;
    char *dxy_name;
    char buf[300];
    char *mapset;
    int nrows, row;
    int ncols, col;

    double G_distance();
    double G_row_to_northing();
    double G_col_to_easting();
    double north, east, south, west, ns_med;

    double radians_to_degrees;
    double degrees_to_radians;
    double sqrt(), tan(), atan2(), atan();
    double H,V;
    double dx;              /* slope in ew direction */
    double dy;              /* slope in ns direction */
    double dxx, dxy, dyy;
    double s3, s4, s5, s6;
    double pcurv, tcurv;
    double scik1 = 100000.;
    double zfactor;
    double aspect, min_asp=360., max_asp=0.;
    double dnorm1, ro, dx2, dy2, grad2, grad, disk, cur1, cur2, dxy2;
    double gradmin = 0.001;
    int    j, got;

    double answer[92];
    double degrees;
    double tan_ans;
    double key;
    double slp_in_perc, slp_in_deg;
    double min_slp=900., max_slp=0., min_slp_allowed;
    int low, hi, test;
    int deg=0;
    int perc=0;
    char *slope_fmt;
    char *str;
	struct GModule *module;
    struct
    {
	struct Option *elevation, *slope_fmt, *slope, *aspect, *pcurv, *tcurv,
		*zfactor, *min_slp_allowed, *out_precision,
		*dx, *dy, *dxx, *dyy, *dxy;
    } parm;
    struct
    {
	struct Flag *a,*q;
    } flag;

	module = G_define_module();
    module->description =
		"Generates raster map layers of slope, aspect, "
		"curvatures and partial derivatives from a raster "
		"map layer of true elevation values.";

    parm.elevation = G_define_option() ;
    parm.elevation->key        = "elevation" ;
    parm.elevation->type       = TYPE_STRING ;
    parm.elevation->required   = YES ;
    parm.elevation->gisprompt  = "old,cell,raster" ;
    parm.elevation->description= "Raster elevation file name";

    parm.slope = G_define_option() ;
    parm.slope->key        = "slope" ;
    parm.slope->type       = TYPE_STRING ;
    parm.slope->required   = NO ;
    parm.slope->answer     = NULL ;
    parm.slope->gisprompt  = "any,cell,raster" ;
    parm.slope->description= "Output slope filename" ;

    parm.slope_fmt = G_define_option() ;
    parm.slope_fmt->key        = "format" ;
    parm.slope_fmt->type       = TYPE_STRING ;
    parm.slope_fmt->required   = NO ;
    parm.slope_fmt->answer     = "degrees";
    parm.slope_fmt->options  = "degrees,percent";
    parm.slope_fmt->description= "format for reporting the slope" ;

    parm.out_precision = G_define_option() ;
    parm.out_precision->key        = "prec";
    parm.out_precision->type       = TYPE_STRING ;
    parm.out_precision->required   = NO ;
    parm.out_precision->answer     = "float";
    parm.out_precision->options  = "default,double,float,int";
    parm.out_precision->description= "type of output aspect and slope maps" ;

    parm.aspect = G_define_option() ;
    parm.aspect->key        = "aspect" ;
    parm.aspect->type       = TYPE_STRING ;
    parm.aspect->required   = NO ;
    parm.aspect->answer     = NULL ;
    parm.aspect->gisprompt  = "any,cell,raster" ;
    parm.aspect->description= "Output aspect filename" ;

    parm.pcurv = G_define_option() ;
    parm.pcurv->key        = "pcurv" ;
    parm.pcurv->type       = TYPE_STRING ;
    parm.pcurv->required   = NO ;
    parm.pcurv->answer     = NULL ;
    parm.pcurv->gisprompt  = "any,cell,raster" ;
    parm.pcurv->description= "Output profile curvature filename" ;

    parm.tcurv = G_define_option() ;
    parm.tcurv->key        = "tcurv" ;
    parm.tcurv->type       = TYPE_STRING ;
    parm.tcurv->required   = NO ;
    parm.tcurv->answer     = NULL ;
    parm.tcurv->gisprompt  = "any,cell,raster" ;
    parm.tcurv->description= "Output tangential curvature filename" ;

    parm.dx = G_define_option() ;
    parm.dx->key        = "dx" ;
    parm.dx->type       = TYPE_STRING ;
    parm.dx->required   = NO ;
    parm.dx->answer     = NULL ;
    parm.dx->gisprompt  = "any,cell,raster" ;
    parm.dx->description= "Output E-W slope filename" ;

    parm.dy = G_define_option() ;
    parm.dy->key        = "dy" ;
    parm.dy->type       = TYPE_STRING ;
    parm.dy->required   = NO ;
    parm.dy->answer     = NULL ;
    parm.dy->gisprompt  = "any,cell,raster" ;
    parm.dy->description= "Output N-S slope filename" ;

    parm.dxx = G_define_option() ;
    parm.dxx->key        = "dxx" ;
    parm.dxx->type       = TYPE_STRING ;
    parm.dxx->required   = NO ;
    parm.dxx->answer     = NULL ;
    parm.dxx->gisprompt  = "any,cell,raster" ;
    parm.dxx->description= "Output partial derivative dxx filename" ;

    parm.dyy = G_define_option() ;
    parm.dyy->key        = "dyy" ;
    parm.dyy->type       = TYPE_STRING ;
    parm.dyy->required   = NO ;
    parm.dyy->answer     = NULL ;
    parm.dyy->gisprompt  = "any,cell,raster" ;
    parm.dyy->description= "Output partial derivative dyy filename" ;

    parm.dxy = G_define_option() ;
    parm.dxy->key        = "dxy" ;
    parm.dxy->type       = TYPE_STRING ;
    parm.dxy->required   = NO ;
    parm.dxy->answer     = NULL ;
    parm.dxy->gisprompt  = "any,cell,raster" ;
    parm.dxy->description= "Output partial derivative dxy filename" ;

    parm.zfactor = G_define_option();
    parm.zfactor->key         = "zfactor";
    parm.zfactor->description = "Multiplicative factor to convert elevation units to meters";
    parm.zfactor->type        = TYPE_DOUBLE;
    parm.zfactor->required    = NO;
    parm.zfactor->answer      = "1.0";

    parm.min_slp_allowed = G_define_option();
    parm.min_slp_allowed->key         = "min_slp_allowed";
    parm.min_slp_allowed->description = "Minimum slope val. (in percent) for which aspect is computed";
    parm.min_slp_allowed->type        = TYPE_DOUBLE;
    parm.min_slp_allowed->required    = NO;
    parm.min_slp_allowed->answer      = "0.0";

    flag.a = G_define_flag() ;
    flag.a->key         = 'a' ;
    flag.a->description = "Do not align the current region to the elevation layer" ;

    flag.q = G_define_flag() ;
    flag.q->key         = 'q' ;
    flag.q->description = "Quiet" ;

    G_gisinit (argv[0]);

    radians_to_degrees = 180.0 / 3.14159 ;
    degrees_to_radians = 3.14159 / 180.0 ;

/* INC BY ONE
    answer[0] = 0.0;
    answer[91] = 15000.0;

    for (i = 1; i < 91; i++)
    {
        degrees = i - .5;
        tan_ans = tan ( degrees  / radians_to_degrees );
        answer[i] = tan_ans * tan_ans;
    }
    */
    answer[0] = 0.0;
    answer[90] = 15000.0;

    for (i = 0; i < 90; i++)
    {
        degrees = i + .5;
        tan_ans = tan ( degrees  / radians_to_degrees );
        answer[i] = tan_ans * tan_ans;
    }

    if (G_parser(argc, argv))
        exit(-1);

    verbose = (!flag.q->answer);
    align   = (!flag.a->answer);

    elev_name = parm.elevation->answer;
    slope_name = parm.slope->answer;
    aspect_name = parm.aspect->answer;
    pcurv_name = parm.pcurv->answer;
    tcurv_name = parm.tcurv->answer;
    dx_name = parm.dx->answer;
    dy_name = parm.dy->answer;
    dxx_name = parm.dxx->answer;
    dyy_name = parm.dyy->answer;
    dxy_name = parm.dxy->answer;
    if (sscanf (parm.zfactor->answer, "%lf", &zfactor) != 1 || zfactor <= 0.0)
    {
        fprintf (stderr, "ERROR: %s=%s - must be a postive number\n",
                       parm.zfactor->key, parm.zfactor->answer);
        G_usage();
        exit(1);
    }

    if (sscanf (parm.min_slp_allowed->answer, "%lf", &min_slp_allowed) != 1 || min_slp_allowed < 0.0)
    {
        fprintf (stderr, "ERROR: %s=%s - must be a non_negative number\n",
                       parm.min_slp_allowed->key, parm.min_slp_allowed->answer);
        G_usage();
        exit(1);
    }

    slope_fmt = parm.slope_fmt->answer;
    if(strcmp(slope_fmt,"percent")==0)perc=1;
    else if(strcmp(slope_fmt,"degrees")==0)deg=1;

    if (slope_name == NULL && aspect_name == NULL
        && pcurv_name == NULL && tcurv_name == NULL
	&& dx_name == NULL && dy_name == NULL 
	&& dxx_name == NULL && dyy_name == NULL && dxy_name == NULL)
    {
	fprintf(stderr, "\nYou must specify at least one of the parameters:");
	fprintf(stderr, 
		"\n<%s>, <%s>, <%s>, <%s>, <%s>, <%s>, <%s>, <%s>,  or <%s>\n", 
		parm.slope->key, parm.aspect->key, parm.pcurv->key, 
		parm.tcurv->key, parm.dx->key, parm.dy->key, 
		parm.dxx->key, parm.dyy->key, parm.dxy->key);
	G_usage();
	exit(1);
    }

    /* check elevation file existence */
    mapset = G_find_cell2(elev_name, "");
    if (!mapset)
    {
        sprintf (buf, "elevation file [%s] not found\n", elev_name);
        G_fatal_error (buf);
        exit(1);
    }
/* set the window from the header for the elevation file */
    if (align)
    {
	G_get_window (&window);
	if (G_get_cellhd (elev_name, mapset, &cellhd) >= 0)
	{
	    G_align_window (&window, &cellhd);
	    G_set_window (&window);
	}
    }
    
   str = parm.out_precision->answer;
   if(strcmp(str, "double")==0)
       out_type = DCELL_TYPE;
   else if(strcmp(str, "float")==0)
       out_type = FCELL_TYPE;
   else if(strcmp(str, "int")==0)
       out_type = CELL_TYPE;
   else if(strcmp(str, "default")==0)
       out_type = -1;
   else
   {
        sprintf(buf, "wrong type: %s", str);
        G_fatal_error(buf);
   }
   data_type = out_type;
   if(data_type < 0) data_type = DCELL_TYPE;
   /* data type is the type of data being processed,
      out_type is type of map being created */

    G_get_set_window (&window);

    nrows = G_window_rows();
    ncols = G_window_cols();

    if (((window.west==(window.east-360.)) 
          ||(window.east==(window.west-360.)))&&
	  (G_projection()==PROJECTION_LL))
    {
       Wrap = 1;
       ncols+=2; 
    }
    else Wrap = 0;

    /* H = window.ew_res * 4 * 2/ zfactor;*/  /* horizontal (east-west) run 
                                   times 4 for weighted difference */
    /* V = window.ns_res * 4 * 2/ zfactor;*/  /* vertical (north-south) run 
                                   times 4 for weighted difference */

    G_begin_distance_calculations();
    north = G_row_to_northing(0.5, &window);
    ns_med = G_row_to_northing(1.5, &window);
    south = G_row_to_northing(2.5, &window);
    east =  G_col_to_easting(2.5, &window);
    west =  G_col_to_easting(0.5, &window);
    V = G_distance(east, north, east, south) * 4 / zfactor;
    H = G_distance(east, ns_med, west, ns_med) * 4 / zfactor;
    /*    ____________________________
	  |c1      |c2      |c3      |
	  |        |        |        |
	  |        |  north |        |        
	  |        |        |        |
	  |________|________|________|          
	  |c4      |c5      |c6      |
	  |        |        |        |
	  |  east  | ns_med |  west  |
	  |        |        |        |
	  |________|________|________|
	  |c7      |c8      |c9      |
	  |        |        |        |
	  |        |  south |        |
	  |        |        |        |
	  |________|________|________|
    */

    /* open the elevation file for reading */
    elevation_fd = G_open_cell_old (elev_name, mapset);
    if (elevation_fd < 0) exit(1);
    elev_cell[0] = (DCELL *) G_calloc (ncols + 1, sizeof(DCELL));
    G_set_d_null_value(elev_cell[0], ncols);
    elev_cell[1] = (DCELL *) G_calloc (ncols + 1, sizeof(DCELL));
    G_set_d_null_value(elev_cell[1], ncols);
    elev_cell[2] = (DCELL *) G_calloc (ncols + 1, sizeof(DCELL));
    G_set_d_null_value(elev_cell[2], ncols);

    if (slope_name != NULL)
    {
        slope_fd = opennew (slope_name, out_type);
	slp_raster = G_allocate_raster_buf(data_type);
	G_set_null_value(slp_raster, G_window_cols(), data_type);
	G_put_raster_row(slope_fd, slp_raster, data_type);
    }
    else
    {
	slp_raster = NULL;
	slope_fd = -1;
    }

    if (aspect_name != NULL)
    {
        aspect_fd = opennew (aspect_name, out_type);
	asp_raster = G_allocate_raster_buf(data_type);
	G_set_null_value(asp_raster, G_window_cols(), data_type);
	G_put_raster_row(aspect_fd, asp_raster, data_type);
    }
    else
    {
	asp_raster = NULL;
	aspect_fd = -1;
    }

    if (pcurv_name != NULL)
    {
        pcurv_fd = opennew (pcurv_name, out_type);
        pcurv_raster = G_allocate_raster_buf(data_type);
        G_set_null_value (pcurv_raster, G_window_cols(), data_type);
        G_put_raster_row (pcurv_fd, pcurv_raster, data_type);
    }
    else
    {
        pcurv_raster = NULL;
        pcurv_fd = -1;
    }

    if (tcurv_name != NULL)
    {
        tcurv_fd = opennew (tcurv_name, out_type);
        tcurv_raster = G_allocate_raster_buf(data_type);
        G_set_null_value (tcurv_raster, G_window_cols(), data_type);
        G_put_raster_row (tcurv_fd, tcurv_raster, data_type);
    }
    else
    {
        tcurv_raster = NULL;
        tcurv_fd = -1;
    }

    if (dx_name != NULL)
    {
        dx_fd = opennew (dx_name, out_type);
        dx_raster = G_allocate_raster_buf(data_type);
        G_set_null_value (dx_raster, G_window_cols(), data_type);
        G_put_raster_row (dx_fd, dx_raster, data_type);
    }
    else
    {
        dx_raster = NULL;
        dx_fd = -1;
    }

    if (dy_name != NULL)
    {
        dy_fd = opennew (dy_name, out_type);
        dy_raster = G_allocate_raster_buf(data_type);
        G_set_null_value (dy_raster, G_window_cols(), data_type);
        G_put_raster_row (dy_fd, dy_raster, data_type);
    }
    else
    {
        dy_raster = NULL;
        dy_fd = -1;
    }

    if (dxx_name != NULL)
    {
        dxx_fd = opennew (dxx_name, out_type);
        dxx_raster = G_allocate_raster_buf(data_type);
        G_set_null_value (dxx_raster, G_window_cols(), data_type);
        G_put_raster_row (dxx_fd, dxx_raster, data_type);
    }
    else
    {
        dxx_raster = NULL;
        dxx_fd = -1;
    }

    if (dyy_name != NULL)
    {
        dyy_fd = opennew (dyy_name, out_type);
        dyy_raster = G_allocate_raster_buf(data_type);
        G_set_null_value (dyy_raster, G_window_cols(), data_type);
        G_put_raster_row (dyy_fd, dyy_raster, data_type);
    }
    else
    {
        dyy_raster = NULL;
        dyy_fd = -1;
    }

    if (dxy_name != NULL)
    {
        dxy_fd = opennew (dxy_name, out_type);
        dxy_raster = G_allocate_raster_buf(data_type);
        G_set_null_value (dxy_raster, G_window_cols(), data_type);
        G_put_raster_row (dxy_fd, dxy_raster, data_type);
    }
    else
    {
        dxy_raster = NULL;
        dxy_fd = -1;
    }

    if (aspect_fd < 0 && slope_fd < 0 && pcurv_fd < 0 && tcurv_fd < 0
	&& dx_fd < 0 && dy_fd < 0 && dxx_fd < 0 && dyy_fd < 0 && dxy_fd < 0)
        exit(1);

    if(Wrap)
    {
       G_get_d_raster_row_nomask (elevation_fd, elev_cell[1]+1,0);
       elev_cell[1][0] = elev_cell[1][G_window_cols()-1];
       elev_cell[1][G_window_cols()+1]=elev_cell[1][2];
    }
    else G_get_d_raster_row_nomask (elevation_fd, elev_cell[1],0);

    if(Wrap)
    {
       G_get_d_raster_row_nomask (elevation_fd, elev_cell[2]+1,1);
       elev_cell[2][0] = elev_cell[2][G_window_cols()-1];
       elev_cell[2][G_window_cols()+1]=elev_cell[2][2];
    }
    else G_get_d_raster_row_nomask (elevation_fd, elev_cell[2],1);

    if (verbose) fprintf (stderr, "percent complete: ");
    for (row = 2; row < nrows; row++)
    {

/*  if projection is Lat/Lon, recalculate  V and H   */

	if (G_projection()==PROJECTION_LL)
	{
          north = G_row_to_northing((row-2 + 0.5), &window);
          ns_med = G_row_to_northing((row-1 + 0.5), &window);
          south = G_row_to_northing((row + 0.5), &window);
          east =  G_col_to_easting(2.5, &window);
          west =  G_col_to_easting(0.5, &window);
          V = G_distance(east, north, east, south) * 4 / zfactor;
          H = G_distance(east, ns_med, west, ns_med) * 4 / zfactor;
/*        ____________________________
	  |c1      |c2      |c3      |
	  |        |        |        |
	  |        |  north |        |        
	  |        |        |        |
	  |________|________|________|          
	  |c4      |c5      |c6      |
	  |        |        |        |
	  |  east  | ns_med |  west  |
	  |        |        |        |
	  |________|________|________|
	  |c7      |c8      |c9      |
	  |        |        |        |
	  |        |  south |        |
	  |        |        |        |
	  |________|________|________|
*/
	}

        if (verbose) G_percent (row, nrows, 2);
        temp = elev_cell[0];
        elev_cell[0] = elev_cell[1];
        elev_cell[1] = elev_cell[2];
	elev_cell[2] = temp;
        if(Wrap)
        {
           G_get_d_raster_row_nomask (elevation_fd, elev_cell[2] + 1, row);
           elev_cell[2][0] = elev_cell[2][G_window_cols()-1];
           elev_cell[2][G_window_cols()+1]=elev_cell[2][2];
        }
        else G_get_d_raster_row_nomask (elevation_fd, elev_cell[2], row);

        c1 = elev_cell[0];
        c2 = c1+1;
        c3 = c1+2;
        c4 = elev_cell[1];
        c5 = c4+1;
        c6 = c4+2;
        c7 = elev_cell[2];
        c8 = c7+1;
        c9 = c7+2;

	if (aspect_fd >= 0)
	{
	    if(Wrap)
	       asp_ptr = asp_raster;
            else 
	       asp_ptr = G_incr_void_ptr(asp_raster, 
				  G_raster_size(data_type));
        }
	if (slope_fd >= 0)
	{
	    if(Wrap)
	       slp_ptr = slp_raster;
            else 
	       slp_ptr = G_incr_void_ptr(slp_raster, 
				  G_raster_size(data_type));
        }

        if (pcurv_fd >= 0)
        {
            if(Wrap)
               pcurv_ptr = pcurv_raster;
            else
               pcurv_ptr = G_incr_void_ptr(pcurv_raster,
				G_raster_size(data_type));
        }

        if (tcurv_fd >= 0)
        {
            if(Wrap)
               tcurv_ptr = tcurv_raster;
            else
               tcurv_ptr = G_incr_void_ptr(tcurv_raster,
                                G_raster_size(data_type));
        }

        if (dx_fd >= 0)
        {
            if(Wrap)
               dx_ptr = dx_raster;
            else
               dx_ptr = G_incr_void_ptr(dx_raster,
                                G_raster_size(data_type));
        }

        if (dy_fd >= 0)
        {
            if(Wrap)
               dy_ptr = dy_raster;
            else
               dy_ptr = G_incr_void_ptr(dy_raster,
                                G_raster_size(data_type));
        }

        if (dxx_fd >= 0)
        {
            if(Wrap)
               dxx_ptr = dxx_raster;
            else
               dxx_ptr = G_incr_void_ptr(dxx_raster,
                                G_raster_size(data_type));
        }

        if (dyy_fd >= 0)
        {
            if(Wrap)
               dyy_ptr = dyy_raster;
            else
               dyy_ptr = G_incr_void_ptr(dyy_raster,
                                G_raster_size(data_type));
        }

        if (dxy_fd >= 0)
        {
            if(Wrap)
               dxy_ptr = dxy_raster;
            else
               dxy_ptr = G_incr_void_ptr(dxy_raster,
                                G_raster_size(data_type));
        }


        /*skip first cell of the row*/

        for (col = ncols-2; col-- > 0; c1++,c2++,c3++,c4++,c5++,c6++,c7++,c8++,c9++)
        {
            /*  DEBUG:
        fprintf(stdout, "\n%.0f %.0f %.0f\n%.0f %.0f %.0f\n%.0f %.0f %.0f\n",
                 *c1, *c2, *c3, *c4, *c5, *c6, *c7, *c8, *c9);
            */

             if(G_is_d_null_value(c1) || G_is_d_null_value(c2) ||
                G_is_d_null_value(c3) || G_is_d_null_value(c4) || 
                G_is_d_null_value(c5) || G_is_d_null_value(c6) || 
                G_is_d_null_value(c7) || G_is_d_null_value(c8) || 
                G_is_d_null_value(c9))
	    {
		if(slope_fd > 0)
                {
                   G_set_null_value(slp_ptr, 1, data_type);
		   slp_ptr = G_incr_void_ptr(slp_ptr, G_raster_size(data_type));
                }
		if (aspect_fd > 0)
                {
                   G_set_null_value(asp_ptr, 1, data_type);
		   asp_ptr = G_incr_void_ptr(asp_ptr, G_raster_size(data_type));
                }
                if(pcurv_fd > 0)
                {
                   G_set_null_value(pcurv_ptr, 1, data_type);
                   pcurv_ptr = G_incr_void_ptr(pcurv_ptr, G_raster_size(data_type));
                }
                if (tcurv_fd > 0)
                {
                   G_set_null_value(tcurv_ptr, 1, data_type);
                   tcurv_ptr = G_incr_void_ptr(tcurv_ptr, G_raster_size(data_type));
                }
                if (dx_fd > 0)
                {
                   G_set_null_value(dx_ptr, 1, data_type);
                   dx_ptr = G_incr_void_ptr(dx_ptr, G_raster_size(data_type));
                }
                if (dy_fd > 0)
                {
                   G_set_null_value(dy_ptr, 1, data_type);
                   dy_ptr = G_incr_void_ptr(dy_ptr, G_raster_size(data_type));
                }
                if (dxx_fd > 0)
                {
                   G_set_null_value(dxx_ptr, 1, data_type);
                   dxx_ptr = G_incr_void_ptr(dxx_ptr, G_raster_size(data_type));
                }
                if (dyy_fd > 0)
                {
                   G_set_null_value(dyy_ptr, 1, data_type);
                   dyy_ptr = G_incr_void_ptr(dyy_ptr, G_raster_size(data_type));
                }
                if (dxy_fd > 0)
                {
                   G_set_null_value(dxy_ptr, 1, data_type);
                   dxy_ptr = G_incr_void_ptr(dxy_ptr, G_raster_size(data_type));
                }
		continue;
	    } /* no data */

	    dx = ((*c1 + *c4 + *c4 + *c7) - (*c3 + *c6 + *c6 + *c9)) / H;
	    dy = ((*c7 + *c8 + *c8 + *c9) - (*c1 + *c2 + *c2 + *c3)) / V;

	    /* compute topographic parameters */
            key = dx*dx + dy*dy;
	    slp_in_perc = 100*sqrt(key);  
            slp_in_deg = atan(sqrt(key)) * radians_to_degrees;
	    /* now update min and max */
	    if(deg)
	    {
	       if(min_slp > slp_in_deg) min_slp = slp_in_deg;
	       if(max_slp < slp_in_deg) max_slp = slp_in_deg;
            }
	    else
	    {
	       if(min_slp > slp_in_perc) min_slp = slp_in_perc;
	       if(max_slp < slp_in_perc) max_slp = slp_in_perc;
            }
	    if(slp_in_perc < min_slp_allowed) slp_in_perc = 0.;

	    if(deg && out_type == CELL_TYPE)
	    {
	    /* INC BY ONE
               low = 1;
               hi = 91;
             */
               low = 0;
               hi = 90;
               test = 20;

               while (hi >= low)
               {
                   if ( key >= answer[test] )
                       low = test + 1;
                   else if ( key < answer[test-1] )
                       hi = test - 1;
                   else
                       break;
                   test = (low + hi) / 2;
               }
            }
	    else if(perc && out_type == CELL_TYPE) 
	/* INCR_BY_ONE*/
                   /* test = slp_in_perc + 1.5;*/  /* All the slope categories are
						        incremented by 1 */
                   test = slp_in_perc + .5;

            if (slope_fd > 0)
            {
               if(data_type == CELL_TYPE)
                   *((CELL *) slp_ptr) = (CELL) test;
               else 
               {
                   if(deg) G_set_raster_value_d(slp_ptr, 
					   (DCELL)slp_in_deg, data_type);
                   else    G_set_raster_value_d(slp_ptr,
					   (DCELL ) slp_in_perc, data_type);
               }
	       slp_ptr = G_incr_void_ptr(slp_ptr, G_raster_size(data_type));
            } /* computing slope */

            if (aspect_fd > 0)
            {
                if (key == 0.) aspect = 0.;  
                else if (dx == 0)
                {
                    if (dy > 0) aspect = 90.;  
                    else aspect = 270.;   
                }
                else 
		{
		   aspect = (atan2(dy,dx)/degrees_to_radians);
		   if((aspect<=0.5)&&(aspect>0)&& out_type == CELL_TYPE) 
                                               aspect=360.;
		   if(aspect<=0.)aspect=360.+aspect;
                }

		/* if it's not the case that the slope for this cell 
		is below specified minimum */
                if(!((slope_fd > 0)&&(slp_in_perc < min_slp_allowed)))
		{
                    if(out_type == CELL_TYPE)
                        *((CELL *) asp_ptr) = (CELL) (aspect + .5);
                    else
                        G_set_raster_value_d(asp_ptr,
					   (DCELL ) aspect, data_type);
                }
		else
		    G_set_null_value(asp_ptr, 1, data_type);
	        asp_ptr = G_incr_void_ptr(asp_ptr, G_raster_size(data_type));

		/* now update min and max */
		if(min_asp > aspect) min_asp = aspect;
		if(max_asp < aspect) max_asp = aspect;
             } /* computing aspect */

	     /* compute second order derivatives */
	     s4 = *c1 + *c3 + *c7 + *c9 - *c5 * 8.;
             s5 = *c4 * 4. + *c6 * 4. - *c8 * 2. - *c2 * 2.;
             s6 = *c8 * 4. + *c2 * 4. - *c4 * 2. - *c6 * 2.;
             s3 = *c7 - *c9 + *c3 - *c1;

             dxx = (s4 + s5) / ((3./32.)*H*H);
             dyy = (s4 + s6) / ((3./32.)*V*V);
             dxy = s3 / ((1./16.)*H*V);

	     if(dx_fd > 0)
	     {
                if (out_type == CELL_TYPE)
                    *((CELL *) dx_ptr) = (CELL) dx;
                else
                    G_set_raster_value_d(dx_ptr, (DCELL) dx, data_type);
                dx_ptr= G_incr_void_ptr(dx_ptr, G_raster_size(data_type));
	     }

	     if(dy_fd > 0)
	     {
                if (out_type == CELL_TYPE)
                    *((CELL *) dy_ptr) = (CELL) dy;
                else
                    G_set_raster_value_d(dy_ptr, (DCELL) dy, data_type);
                dy_ptr= G_incr_void_ptr(dy_ptr, G_raster_size(data_type));
	     }

	     if(dxx_fd > 0)
	     {
                if (out_type == CELL_TYPE)
                    *((CELL *) dxx_ptr) = (CELL) dxx;
                else
                    G_set_raster_value_d(dxx_ptr, (DCELL) dxx, data_type);
                dxx_ptr= G_incr_void_ptr(dxx_ptr, G_raster_size(data_type));
	     }

	     if(dyy_fd > 0)
	     {
                if (out_type == CELL_TYPE)
                    *((CELL *) dyy_ptr) = (CELL) dyy;
                else
                    G_set_raster_value_d(dyy_ptr, (DCELL) dyy, data_type);
                dyy_ptr= G_incr_void_ptr(dyy_ptr, G_raster_size(data_type));
	     }

	     if(dxy_fd > 0)
	     {
                if (out_type == CELL_TYPE)
                    *((CELL *) dxy_ptr) = (CELL) dxy;
                else
                    G_set_raster_value_d(dxy_ptr, (DCELL) dxy, data_type);
                dxy_ptr= G_incr_void_ptr(dxy_ptr, G_raster_size(data_type));
	     }

	     /* compute curvature */
             if(pcurv_fd > 0 || tcurv_fd > 0 )
      	     {
        	grad2 = key;         /*dx2 + dy2*/
        	grad = sqrt (grad2);
        	if (grad <= gradmin)
        	{
            		pcurv = 0.;
            		tcurv = 0.;
        	}
        	else
        	{
            		dnorm1 =  sqrt (grad2 + 1.);
            		dxy2 = 2. * dxy * dx * dy;
            		dx2 = dx * dx;
            		dy2 = dy * dy;
            		pcurv = (dxx * dx2 + dxy2 + dyy * dy2) / 
				(grad2 * dnorm1*dnorm1*dnorm1);
            		tcurv = (dxx * dy2 - dxy2 + dyy * dx2) / 
				(grad2 * dnorm1);
        	}
		if (pcurv_fd > 0) {
		  if (out_type == CELL_TYPE)
		    *((CELL *) pcurv_ptr) = (CELL) (scik1 * pcurv);
		  else
		    G_set_raster_value_d(pcurv_ptr, (DCELL) pcurv, data_type);
		}
		else
		    G_set_null_value (pcurv_ptr, 1, data_type);
		pcurv_ptr= G_incr_void_ptr(pcurv_ptr, G_raster_size(data_type));
                if (tcurv_fd > 0) {
                  if (out_type == CELL_TYPE)
                    *((CELL *) tcurv_ptr) = (CELL) (scik1 * tcurv);
                  else
                    G_set_raster_value_d(tcurv_ptr, (DCELL) tcurv, data_type);
                }
                else
                    G_set_null_value (tcurv_ptr, 1, data_type);
                tcurv_ptr= G_incr_void_ptr(tcurv_ptr, G_raster_size(data_type));
	      } /* curvature end */

        } /* column for loop */
        if (aspect_fd > 0)
	    G_put_raster_row(aspect_fd, asp_raster, data_type);

        if (slope_fd > 0)
	    G_put_raster_row(slope_fd, slp_raster, data_type);

        if (pcurv_fd > 0)
            G_put_raster_row(pcurv_fd, pcurv_raster, data_type);

        if (tcurv_fd > 0)
            G_put_raster_row(tcurv_fd, tcurv_raster, data_type);

        if (dx_fd > 0)
            G_put_raster_row(dx_fd, dx_raster, data_type);

        if (dy_fd > 0)
            G_put_raster_row(dy_fd, dy_raster, data_type);

        if (dxx_fd > 0)
            G_put_raster_row(dxx_fd, dxx_raster, data_type);

        if (dyy_fd > 0)
            G_put_raster_row(dyy_fd, dyy_raster, data_type);

        if (dxy_fd > 0)
            G_put_raster_row(dxy_fd, dxy_raster, data_type);

    } /* row loop */
    if (verbose) G_percent (row, nrows, 2);

    G_close_cell (elevation_fd);
    if (verbose)
        fprintf (stderr,"CREATING SUPPORT FILES\n");

    fprintf (stdout, "ELEVATION PRODUCTS for mapset [%s] in [%s]\n",
        G_mapset(), G_location());

    if (aspect_fd >= 0)
    {
        G_set_null_value(asp_raster, G_window_cols(), data_type);
        G_put_raster_row (aspect_fd, asp_raster, data_type);
        G_close_cell (aspect_fd);

        if(out_type != CELL_TYPE)
           G_quantize_fp_map_range(aspect_name, G_mapset(), 0., 360., 0, 360);

        G_read_raster_cats (aspect_name, G_mapset(), &cats);
        G_set_raster_cats_title ("aspect in degrees from east", &cats);

	fprintf(stdout, "min computed aspect %.4f  max computed aspect %.4f\n", min_asp, max_asp);
	/* the categries quant intervals are 1.0 long, plus
	   we are using reverse order so that the label looked up
	   for i-.5 is not the one defined for i-.5, i+.5 interval, but
	   the one defile for i-1.5, i-.5 interval which is added later */
	for(i=ceil(max_asp);i>=1;i--)
	{
	       if(i==360)sprintf(buf,"east");
	       else if(i==360)sprintf(buf,"east");
	       else if(i==45)sprintf(buf,"north of east");
	       else if(i==90)sprintf(buf,"north");
	       else if(i==135)sprintf(buf,"north of west");
	       else if(i==180)sprintf(buf,"west");
	       else if(i==225)sprintf(buf,"south of west");
	       else if(i==270)sprintf(buf,"south");
	       else if(i==315)sprintf(buf,"south of east");
               else sprintf (buf, "%d degree%s from east", i, i==1?"":"s");
	       if(data_type==CELL_TYPE) 
	       {
		  G_set_cat(i, buf, &cats);
		  continue;
               }
	       tmp1 = (double) i - .5;
	       tmp2 = (double) i + .5;
               G_set_d_raster_cat (&tmp1, &tmp2, buf, &cats);
	}
	if(data_type==CELL_TYPE)
	       G_set_cat(0, "no aspect", &cats);
        else
	{
	   tmp1 = 0.;
	   tmp2 = .5;
           G_set_d_raster_cat (&tmp1, &tmp2, "no aspect", &cats);
	}
        G_write_raster_cats (aspect_name, &cats);
        G_free_raster_cats (&cats);

        sprintf(buf, "r.colors map='%s' c=aspect",
		G_fully_qualified_name (aspect_name, G_mapset()));
	system(buf);

        /* writing history file */
        G_short_history(aspect_name, "raster", &hist);
        sprintf(hist.edhist[0], "aspect map elev = %s", elev_name); 
        sprintf(hist.edhist[1], "zfactor = %.2f", zfactor); 
        sprintf(hist.edhist[2], "min_slp_allowed = %f", min_slp_allowed); 
        sprintf(hist.datsrc_1,"raster elevation file %s", elev_name);
        hist.edlinecnt = 3;
        G_write_history (aspect_name, &hist);

        fprintf (stdout, "ASPECT [%s] COMPLETE\n", aspect_name);
    }

    if (slope_fd >= 0)
    {
        G_set_null_value(slp_raster, G_window_cols(), data_type);
        G_put_raster_row (slope_fd, slp_raster, data_type);
        G_close_cell (slope_fd);

        if (out_type != CELL_TYPE)
        {
	/* INCR_BY_ONE
           if(deg)
               G_quantize_fp_map_range(slope_name, G_mapset(), 0., 90., 1, 91);
           else
               G_quantize_fp_map_range(slope_name, G_mapset(), min_slp, max_slp, 
				  (CELL) min_slp + 1, (CELL) ceil(max_slp) + 1);
        */
           if(deg)
               G_quantize_fp_map_range(slope_name, G_mapset(), 0., 90., 0, 90);
           else /* percent */
               G_quantize_fp_map_range(slope_name, G_mapset(), min_slp, max_slp, 
				  (CELL) min_slp, (CELL) ceil(max_slp));
        }

        G_read_raster_cats (slope_name, G_mapset(), &cats);
        if(deg) G_set_raster_cats_title ("slope in degrees", &cats);
        else if(perc) G_set_raster_cats_title ("percent slope", &cats);

	fprintf(stdout, "min computed slope %.4f  max computed slope %.4f\n", min_slp, max_slp);
	/* the categries quant intervals are 1.0 long, plus
	   we are using reverse order so that the label looked up
	   for i-.5 is not the one defined for i-.5, i+.5 interval, but
	   the one defined for i-1.5, i-.5 interval which is added later */
        for (i = ceil(max_slp); i>/* INC BY ONE >= */ 0; i--)
        {
            if(deg)sprintf (buf, "%d degree%s", i, i==1?"":"s");
            else if(perc)sprintf (buf, "%d percent", i);
	    if(data_type==CELL_TYPE)
	    {
	/* INCR_BY_ONE
		G_set_cat(i+1, buf, &cats);
		*/
		G_set_cat(i, buf, &cats);
		continue;
            }
	/* INCR_BY_ONE
	    tmp1 = (DCELL) i+.5;
	    tmp2 = (DCELL) i+1.5;
        */
	    tmp1 = (DCELL) i-.5;
	    tmp2 = (DCELL) i+.5;
            G_set_d_raster_cat (&tmp1, &tmp2, buf, &cats);
        }
	if(data_type==CELL_TYPE)
	       G_set_cat(0, "zero slope", &cats);
	/* INCR_BY_ONE
	       G_set_cat(0, "no data", &cats);
	       */
        else
        {
	   tmp1 = 0;
	   tmp2 = 0.5;
           G_set_d_raster_cat (&tmp1, &tmp2, "zero slope", &cats);
	}
	/* INCR_BY_ONE
        G_set_d_raster_cat (&tmp1, &tmp1, "no data", &cats);
	*/
        G_write_raster_cats (slope_name, &cats);

        /* writing history file */
        G_short_history(slope_name, "raster", &hist);
        sprintf(hist.edhist[0], "slope map elev = %s", elev_name); 
        sprintf(hist.edhist[1], "zfactor = %.2f format = %s", zfactor, parm.slope_fmt->answer); 
        sprintf(hist.edhist[2], "min_slp_allowed = %f", min_slp_allowed); 
        sprintf(hist.datsrc_1,"raster elevation file %s", elev_name);
        hist.edlinecnt = 3;
        G_write_history (slope_name, &hist);

        fprintf (stdout, "SLOPE [%s] COMPLETE\n", slope_name);
    }

    if (pcurv_fd >= 0)
    {
        G_set_null_value(pcurv_raster, G_window_cols(), data_type);
        G_put_raster_row (pcurv_fd, pcurv_raster, data_type);
        G_close_cell (pcurv_fd);

	if (out_type != CELL_TYPE)
           G_round_fp_map(pcurv_name, G_mapset());

	G_read_cats (pcurv_name, G_mapset(), &cats);
        G_set_cats_title ("profile curvature", &cats);
	G_set_cat ((CELL)0, "no profile curve", &cats);

        /* writing history file */
        G_short_history(pcurv_name, "raster", &hist);
        sprintf(hist.edhist[0], "profile curve map elev = %s", elev_name);
        sprintf(hist.edhist[1], "zfactor = %.2f", zfactor);
        sprintf(hist.edhist[2], "min_slp_allowed = %f", min_slp_allowed);
        sprintf(hist.datsrc_1,"raster elevation file %s", elev_name);
        hist.edlinecnt = 3;
        G_write_history (pcurv_name, &hist);

        fprintf (stdout, "PROFILE CURVE [%s] COMPLETE\n", pcurv_name);
    }

    if (tcurv_fd >= 0)
    {
        G_set_null_value(tcurv_raster, G_window_cols(), data_type);
        G_put_raster_row (tcurv_fd, tcurv_raster, data_type);
        G_close_cell (tcurv_fd);

	if (out_type != CELL_TYPE)
           G_round_fp_map(tcurv_name, G_mapset());

	G_read_cats (tcurv_name, G_mapset(), &cats);
        G_set_cats_title ("tangential curvature", &cats);
	G_set_cat ((CELL)0, "no tangential curve", &cats);

        /* writing history file */
        G_short_history(tcurv_name, "raster", &hist);
        sprintf(hist.edhist[0], "tangential curve map elev = %s", elev_name);
        sprintf(hist.edhist[1], "zfactor = %.2f", zfactor);
        sprintf(hist.edhist[2], "min_slp_allowed = %f", min_slp_allowed);
        sprintf(hist.datsrc_1,"raster elevation file %s", elev_name);
        hist.edlinecnt = 3;
        G_write_history (tcurv_name, &hist);

        fprintf (stdout, "TANGENTIAL CURVE [%s] COMPLETE\n", tcurv_name);
    }   

    if (dx_fd >= 0)
    {
        G_set_null_value(dx_raster, G_window_cols(), data_type);
        G_put_raster_row (dx_fd, dx_raster, data_type);
        G_close_cell (dx_fd);

	if (out_type != CELL_TYPE)
           G_round_fp_map(dx_name, G_mapset());

	G_read_cats (dx_name, G_mapset(), &cats);
        G_set_cats_title ("E-W slope", &cats);
	G_set_cat ((CELL)0, "no E-W slope", &cats);

        /* writing history file */
        G_short_history(dx_name, "raster", &hist);
        sprintf(hist.edhist[0], "E-W slope map elev = %s", elev_name);
        sprintf(hist.edhist[1], "zfactor = %.2f", zfactor);
        sprintf(hist.edhist[2], "min_slp_allowed = %f", min_slp_allowed);
        sprintf(hist.datsrc_1,"raster elevation file %s", elev_name);
        hist.edlinecnt = 3;
        G_write_history (dx_name, &hist);

        fprintf (stdout, "E-W SLOPE [%s] COMPLETE\n", dx_name);
    }   

    if (dy_fd >= 0)
    {
        G_set_null_value(dy_raster, G_window_cols(), data_type);
        G_put_raster_row (dy_fd, dy_raster, data_type);
        G_close_cell (dy_fd);

	if (out_type != CELL_TYPE)
           G_round_fp_map(dy_name, G_mapset());

	G_read_cats (dy_name, G_mapset(), &cats);
        G_set_cats_title ("N-S slope", &cats);
	G_set_cat ((CELL)0, "no N-S slope", &cats);

        /* writing history file */
        G_short_history(dy_name, "raster", &hist);
        sprintf(hist.edhist[0], "N-S slope map elev = %s", elev_name);
        sprintf(hist.edhist[1], "zfactor = %.2f", zfactor);
        sprintf(hist.edhist[2], "min_slp_allowed = %f", min_slp_allowed);
        sprintf(hist.datsrc_1,"raster elevation file %s", elev_name);
        hist.edlinecnt = 3;
        G_write_history (dy_name, &hist);

        fprintf (stdout, "N-S SLOPE [%s] COMPLETE\n", dy_name);
    }   

    if (dxx_fd >= 0)
    {
        G_set_null_value(dxx_raster, G_window_cols(), data_type);
        G_put_raster_row (dxx_fd, dxx_raster, data_type);
        G_close_cell (dxx_fd);

	if (out_type != CELL_TYPE)
           G_round_fp_map(dxx_name, G_mapset());

	G_read_cats (dxx_name, G_mapset(), &cats);
        G_set_cats_title ("DXX", &cats);
	G_set_cat ((CELL)0, "DXX", &cats);

        /* writing history file */
        G_short_history(dxx_name, "raster", &hist);
        sprintf(hist.edhist[0], "DXX map elev = %s", elev_name);
        sprintf(hist.edhist[1], "zfactor = %.2f", zfactor);
        sprintf(hist.edhist[2], "min_slp_allowed = %f", min_slp_allowed);
        sprintf(hist.datsrc_1,"raster elevation file %s", elev_name);
        hist.edlinecnt = 3;
        G_write_history (dxx_name, &hist);

        fprintf (stdout, "DXX [%s] COMPLETE\n", dxx_name);
    }   

    if (dyy_fd >= 0)
    {
        G_set_null_value(dyy_raster, G_window_cols(), data_type);
        G_put_raster_row (dyy_fd, dyy_raster, data_type);
        G_close_cell (dyy_fd);

	if (out_type != CELL_TYPE)
           G_round_fp_map(dyy_name, G_mapset());

	G_read_cats (dyy_name, G_mapset(), &cats);
        G_set_cats_title ("DYY", &cats);
	G_set_cat ((CELL)0, "DYY", &cats);

        /* writing history file */
        G_short_history(dyy_name, "raster", &hist);
        sprintf(hist.edhist[0], "DYY map elev = %s", elev_name);
        sprintf(hist.edhist[1], "zfactor = %.2f", zfactor);
        sprintf(hist.edhist[2], "min_slp_allowed = %f", min_slp_allowed);
        sprintf(hist.datsrc_1,"raster elevation file %s", elev_name);
        hist.edlinecnt = 3;
        G_write_history (dyy_name, &hist);

        fprintf (stdout, "DYY [%s] COMPLETE\n", dyy_name);
    }   

    if (dxy_fd >= 0)
    {
        G_set_null_value(dxy_raster, G_window_cols(), data_type);
        G_put_raster_row (dxy_fd, dxy_raster, data_type);
        G_close_cell (dxy_fd);

	if (out_type != CELL_TYPE)
           G_round_fp_map(dxy_name, G_mapset());

	G_read_cats (dxy_name, G_mapset(), &cats);
        G_set_cats_title ("DXY", &cats);
	G_set_cat ((CELL)0, "DXY", &cats);

        /* writing history file */
        G_short_history(dxy_name, "raster", &hist);
        sprintf(hist.edhist[0], "DXY map elev = %s", elev_name);
        sprintf(hist.edhist[1], "zfactor = %.2f", zfactor);
        sprintf(hist.edhist[2], "min_slp_allowed = %f", min_slp_allowed);
        sprintf(hist.datsrc_1,"raster elevation file %s", elev_name);
        hist.edlinecnt = 3;
        G_write_history (dxy_name, &hist);

        fprintf (stdout, "DXY [%s] COMPLETE\n", dxy_name);
    }   

    exit(0);
}
