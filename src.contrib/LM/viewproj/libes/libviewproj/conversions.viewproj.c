/*
****************************************************************************
*
* MODULE:       conversions.viewproj.c
* AUTHOR(S):    Sharif Razzaque, LMMS, June 1995 (Original author)
*               Tuan Tran, LMMS (fixed aspect ratio)
*               Bev Wallace, beverly.t.wallace@lmco.com (Grass 5.0)
* PURPOSE:      Library for converting between coordinate systems.
* COPYRIGHT:    (C) 1995 by Lockheed Martin Missiles & Space, Sunnyvale, CA, USA
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

/*
*************************************************************************
conversions.viewproj.c

This file takes care of converting between four  coordinate systems:
1) display window coordinates
2) lat/lon
3) map rasterfile coordinates (the row & column of the cell to read in)
4) cartPROJ (the cartesian coordinate system the PROJ uses by default)

This file contains the routines to do these conversions and the routine to 
set-up these conversions. 

See coord_systems.viewproj.h for info on useing these routines. 

Sharif Razzaque June 1995
*************************************************************************
*/

#include <math.h>

#include "config.h"	/* For Grass 5.0 - Bev Wallace */
#include "gis.h"
#include "display.h"	/* For D_* - Bev Wallace */
#include "raster.h"	/* For R_* - Bev Wallace */
#include "projects.h" 	/* PROJ utility header file  */ 

#include "coord_systems.viewproj.h" /* contains prototype & coordinate types */

struct Cell_head earth_wind; /* contains map & earth region info */

PJ *proj_ref;

typedef projUV UV;	/* Needed for Grass 5.0.0 - Bev Wallace */
UV data;


/*
*******************************************************************************
  COORD CONVERSION GLOBALS
*******************************************************************************
*/


typedef struct
{
        double disp_x;
        double disp_y;
        double scale_x;
        double scale_y;
} linear_trans_double_type;

linear_trans_double_type cP_to_scrn_info;
linear_trans_double_type scrn_to_cP_info;

/* the rest of these are straight from conversions.c */

/* UTM coordinates.          (0,0) towards SW */
        static double U_west   ;  /*  western edge  (UTM/meters)  */
        static double U_north  ;
        static double U_south  ;  /*  southern edge (UTM/meters)  */
        /* others */
        static double ew_resolution ;
        static double ns_resolution ;
        static double ARRAY_ROWS ;


/*
*******************************************************************************
  COORD CONVERSION FUNCTIONS
*******************************************************************************
*/


cartPROJ_coord_type latlon_to_cartPROJ (latlon_coord_type from)
{
        cartPROJ_coord_type to;

        data.u=from.lon;  /*from p.24 of PROJ.4 man*/
        data.v=from.lat;

        data.u *=DEG_TO_RAD;
        data.v *=DEG_TO_RAD;

        data=pj_fwd(data,proj_ref);
        
        if(data.u==HUGE_VAL)
        {
                char buff[128];

                sprintf(buff,
                        "latlon_to_cartPROJ cannot map  %f,%f in cart_PROJ\n",
			from.lon, from.lat);
                G_fatal_error(buff);
        }
        
        to.x=data.u;
        to.y=data.v;
        return(to);
}


latlon_coord_type cartPROJ_to_latlon (cartPROJ_coord_type from)
{
        latlon_coord_type to;

        data.u=from.x;  /*from p.24 of PROJ.4 man*/
        data.v=from.y;


        data=pj_inv(data,proj_ref);
        
        if(data.u==HUGE_VAL)
        {
                to.valid=INVALID_COORD;
        }
        else
        {
                data.u *=RAD_TO_DEG;
                data.v *=RAD_TO_DEG;
                if ( ((data.u>180) || (data.u<-180)) ||
                   ((data.v>90) || (data.v<-90)) )
                {
                        to.valid=INVALID_COORD;
                }
                else
                {
                        to.lon=data.u;
                        to.lat=data.v;
                        to.valid=VALID_COORD;
                }
        }
        return(to);
}


screen_coord_type cartPROJ_to_screen (cartPROJ_coord_type from)
{
        screen_coord_type to;

        to.x= (short)(from.x*cP_to_scrn_info.scale_x - cP_to_scrn_info.disp_x);
        to.y= (short)(from.y*cP_to_scrn_info.scale_y - cP_to_scrn_info.disp_y);

        return (to);
}


cartPROJ_coord_type screen_to_cartPROJ (screen_coord_type from)
{
        cartPROJ_coord_type to;

        to.x= (((double)from.x)*scrn_to_cP_info.scale_x -
                 scrn_to_cP_info.disp_x);
        to.y= (((double)from.y)*scrn_to_cP_info.scale_y -
                 scrn_to_cP_info.disp_y);

        return (to);
}


map_coord_type latlon_to_map (latlon_coord_type from)
{
        map_coord_type to;
	double adj_lon;

        /* check if it's a special case */
        if (from.valid!=VALID_COORD)
        {
                to.row=INVALID_COORD;
                return(to);
        }
        else
        {
		/* Bev Wallace adds G_adjust_east_longitude for wrap-around */
		adj_lon = G_adjust_east_longitude (from.lon, earth_wind.west);	
                /*  check if coord is in selected region */
                if ( 	(from.lat > earth_wind.north) ||
			(from.lat < earth_wind.south) ||
			(adj_lon > earth_wind.east) ||
			(adj_lon < earth_wind.west) )
                {
                        /* it's out of region to draw */
                                        to.row=INVALID_COORD;
                                        return(to);
                }
                else 
                {
                        /* it's O.K. so convert! */
                        to.row=(int)
                      ( ARRAY_ROWS - ((from.lat - U_south)/ns_resolution) );
                        to.col=(int)
                                 ((adj_lon - U_west )/ew_resolution ) ;
                        return(to);
                }
        }
}


/*____________________________________________________________________________
the following functions first convert the coordinates to one or more intermediate
coordinate systems. They are made from pieces of the above functions. They do
not call those above functions in order to avoid the over-head of function-calling
*/


screen_coord_type latlon_to_screen (latlon_coord_type from)
{
        screen_coord_type to;

        data.u=from.lon;  /*from p.24 of PROJ.4 man*/
        data.v=from.lat;

        data.u *=DEG_TO_RAD;
        data.v *=DEG_TO_RAD;

        data=pj_fwd(data,proj_ref);
        
        if(data.u==HUGE_VAL)
        {
                char buff[128];

                sprintf(buff,
                        "latlon_to_screen cannot map  %f,%f in cart_PROJ\n",
			from.lon,from.lat);
                G_fatal_error(buff);
        }
        

        to.x= (short)(data.u*cP_to_scrn_info.scale_x - cP_to_scrn_info.disp_x);
        to.y= (short)(data.v*cP_to_scrn_info.scale_y - cP_to_scrn_info.disp_y);

        return (to);
}



latlon_coord_type screen_to_latlon (screen_coord_type from)
{
        latlon_coord_type ll;

        /* convert from screen to cartPROJ */

        data.u= (((double)from.x)*scrn_to_cP_info.scale_x -
                 scrn_to_cP_info.disp_x);
        data.v= (((double)from.y)*scrn_to_cP_info.scale_y -
                 scrn_to_cP_info.disp_y);

        /* convert from cartPROJ to latlon */

        data=pj_inv(data,proj_ref);
        
        if(data.u==HUGE_VAL)
        {
                ll.valid=INVALID_COORD;
        }
        else
        {
                data.u *=RAD_TO_DEG;
                data.v *=RAD_TO_DEG;
                if ( ((data.u>180) || (data.u<-180)) ||
                   ((data.v>90) || (data.v<-90)) )
                {
                        ll.valid=INVALID_COORD;
                }
                else
                {
                        ll.lon=data.u;
                        ll.lat=data.v;
                        ll.valid=VALID_COORD;
                }
        }

        return(ll);
}


map_coord_type screen_to_map (screen_coord_type from)
{
	latlon_coord_type coord;
        map_coord_type map_to;

        /* convert from screen to cartPROJ */

        data.u= (((double)from.x)*scrn_to_cP_info.scale_x -
                 scrn_to_cP_info.disp_x);
        data.v= (((double)from.y)*scrn_to_cP_info.scale_y -
                 scrn_to_cP_info.disp_y);

        /* convert from cartPROJ to latlon */

        data=pj_inv(data,proj_ref);
        
        if(data.u==HUGE_VAL)
        {
                map_to.row=INVALID_COORD;
                return(map_to);
        }
        
        
        data.u *=RAD_TO_DEG;
        data.v *=RAD_TO_DEG;
        

        /* convert from latlon to map */

	coord.lat = data.v;
	coord.lon = data.u;
	coord.valid = VALID_COORD;
	map_to = latlon_to_map (coord);

        return (map_to);
}


/*
*******************************************************************************
  COORD CONVERSION INIT FUNCTIONS
*******************************************************************************
call setup_conversions_PROJ() from outside this file, it calls the rest of
 these itself
*/


static void cP_to_scrn_init (int t,int b,int l,int r,
                double t_cp, double b_cp, double l_cp, double r_cp)
{
        
        double scale_x;
        double scale_y;
        
        scale_y=((double)(t-b))/(t_cp-b_cp);
        cP_to_scrn_info.scale_y=scale_y;
        cP_to_scrn_info.disp_y=b_cp*scale_y-b;

        scale_x=((double)(l-r))/(l_cp-r_cp);
        cP_to_scrn_info.scale_x=scale_x;
        cP_to_scrn_info.disp_x=r_cp*scale_x-r;

        return;
}


static void scrn_to_cP_init (int t,int b,int l,int r,
                double t_cp, double b_cp, double l_cp, double r_cp)
{
        
        double scale_x;
        double scale_y;
        
        scale_y=(t_cp-b_cp)/((double)(t-b));
        scrn_to_cP_info.scale_y=scale_y;
        scrn_to_cP_info.disp_y=b*scale_y-b_cp;

        scale_x=(l_cp-r_cp)/((double)(l-r));
        scrn_to_cP_info.scale_x=scale_x;
        scrn_to_cP_info.disp_x=r*scale_x-r_cp;

        return;
}


/* this draws a grid of the area of interest on the display window */
static void draw_earth_window (double n,double s,double e,double w,int steps)
{
        int i_x,i_y;
        double lat,lon;
        double delta_lat,delta_lon;
        latlon_coord_type point_ll;
        screen_coord_type point_srn;

        delta_lat=(s-n)/(double)(steps);
        delta_lon=(w-e)/(double)(steps);
        lat=n;
        lon=e;

        for (i_x=0;i_x<=steps;i_x++)
        {
                lat=n;
                for(i_y=0;i_y<=steps;i_y++)
                {
                        point_ll.lat=lat;
                        point_ll.lon=lon;
                        point_srn=latlon_to_screen(point_ll);

                        /* draw a cross at the location on the display */
                        R_move_abs((int)point_srn.x,(int)point_srn.y);
                        R_cont_rel(1,0);
                
                                        
                        lat=lat+delta_lat;
                }
                lon=lon+delta_lon;
        }

        R_flush();

        return;
}


static void latlon_to_map_init (void)
{
/* the code below is from the file cnversions.c, from the function D_do_conversions
    modified by updating variable names */

/* Key all coordinate limits off UTM window limits  */
        U_west  = earth_wind.west ;
        U_north = earth_wind.north;
        U_south = earth_wind.south ;
        ew_resolution=earth_wind.ew_res;
        ns_resolution=earth_wind.ns_res;

/* Calculate Array window limits from UTM limits */
 
        ARRAY_ROWS = (U_north - U_south + ns_resolution/2) / ns_resolution ;

        return;
}


static void get_extend_of_cartPROJ (double *t_cp, double *b_cp, 
				double *r_cp, double *l_cp)
{
         /* point variables */
        cartPROJ_coord_type point_cp;
        latlon_coord_type point_latlon;

        /* vars. for iterating through the region of the earth
                we want to draw on */
        int lat_i,lon_i;
        double s_lat,s_lon;

        int step=40; /* the number of rows & col in the grid */
        double delta_lon,delta_lat;

        delta_lon=(earth_wind.west-earth_wind.east)/step;
        delta_lat=(earth_wind.south-earth_wind.north)/step;
        
        /* project a evenly spaces bunch of lat/lon points
                (from the region of the earth we wish to map) into
                the cartPROJ coord-system. Then keep tract of the extend of
                these points. (ie. the top, bottom, right & left )*/

        s_lat=earth_wind.north;
        for (lat_i=0; lat_i<=step; lat_i++)
        {
                s_lon=earth_wind.east;
                for(lon_i=0; lon_i<=step; lon_i++)
                        {
                        point_latlon.lat=s_lat;
                        point_latlon.lon=s_lon;
                        point_cp=latlon_to_cartPROJ(point_latlon);
                        if ((lat_i==0) &&  (lon_i==0))
                        {
                                /* initilize the values */
                                *t_cp=point_cp.y;
                                *b_cp=*t_cp;
                                *l_cp=point_cp.x;
                                *r_cp=*l_cp;
                        }
                        else
                        {
                                /* adjust the extends if the new 
                                        point lie outside it */
                                if (point_cp.y<*t_cp)
                                        *t_cp=point_cp.y;
                                if (point_cp.y>*b_cp)
                                        *b_cp=point_cp.y;
                                if (point_cp.x<*l_cp)
                                        *l_cp=point_cp.x;
                                if (point_cp.x>*r_cp)
                                        *r_cp=point_cp.x;
                        }       

                        s_lon=s_lon+delta_lon;
                }
                s_lat=s_lat+delta_lat;
        }

        return;
}


/********** CALL THIS TO SETUP UP ALL COVERSIONS!!! ************/
int setup_conversions_viewproj (int draw_grid, int*t_out, int*b_out, 
			int*l_out, int*r_out)
{
        /* if draw_grid=1 it also draws some points of the window to represent
           the select region of the earth */

	int i;
        int t,b,l,r;                 /* edges of display window*/
        double t_cp,b_cp,r_cp,l_cp;  /* edges of cartPROJ region that maps to 
                                        display window */

        char *PROJ_parms[50]; /* parameters to init PROJ with the projection of interest */
        int num_of_PROJ_parms;

        if (G_get_set_window (&earth_wind) ==-1)
                G_fatal_error("Current window not available!");
        D_get_screen_window(&t,&b,&l,&r);

        /* make sure this is a lat/lon database */
        if (G_projection()!=3)
                G_fatal_error("viewproj routines only work with lat/lon databases\n");

        /* read in proj info file for current monitor */
        /* (this should be a seperate function, but the 'G_malloc'ed memory gets freed)*/
        {
                char *monitor_name;
                char mon_file_path[200];
                FILE *file_Ptr; 
               
                int num_parms;

                monitor_name=G_getenv("MONITOR");
                sprintf(mon_file_path,
                        "%s/%s/viewproj_states/%s",
                        G_location_path(), G_mapset(), monitor_name);

                file_Ptr=fopen(mon_file_path,"r");
                if (file_Ptr==NULL)
                {
                        char buff[128];
                        sprintf(buff,"Can't open %s\n",mon_file_path);
                        G_fatal_error(buff);
                }

                num_parms=0;
                PROJ_parms[num_parms]=(char *)G_malloc(sizeof(char)*50);

                while(fscanf(file_Ptr,"%s",PROJ_parms[num_parms])==1)
                {
                        /*printf("%s\n",PROJ_parms[num_parms]);*/
                        
                        num_parms++;
                        PROJ_parms[num_parms]=(char *)G_malloc(sizeof(char)*50);
                }
                fclose (file_Ptr);

                num_of_PROJ_parms=num_parms;
        }

        /* set up PROJ */
        if( !(proj_ref=pj_init(num_of_PROJ_parms, PROJ_parms)))
        {
		/* Exit due to PROJ error */
		fprintf(stderr, "pj_init error %d:  %s\n", 
			pj_errno, pj_strerrno(pj_errno));
                G_fatal_error("Proj init failed!");
        }

        fprintf (stdout, "Setting up coord-system transformations.\n");
        fprintf (stdout, "PROJ initialized: ");
	for (i=0; i<num_of_PROJ_parms; i++)
        	fprintf (stdout, " %s", PROJ_parms[i]);
        fprintf (stdout, "\n");
        fflush (stdout);
     
        /* determine which reqion of cartPROJ plane maps to display window*/
        get_extend_of_cartPROJ(&t_cp,&b_cp,&r_cp,&l_cp);

{  /* Section added by Tuan Tran */
   double excess, cp_aspect, win_aspect, new_width, new_height;


   cp_aspect = fabs((t_cp-b_cp)/(r_cp-l_cp));
   win_aspect = fabs((float)(t-b)/(float)(r-l));
   if(cp_aspect < win_aspect)
   {
        new_height=(b-t)*(cp_aspect/win_aspect);
        excess = (b-t)-new_height;
        t=t+(excess/2.0);
        b=b-(excess/2.0);

   }
   else
   {
        new_width=(r-l)*(win_aspect/cp_aspect);
        excess = (r-l)-new_width;
        l=l+(excess/2.0);
        r=r-(excess/2.0);
   }


   *t_out = t;
   *b_out = b;
   *l_out = l;
   *r_out = r;
}

        cP_to_scrn_init(t,b,l,r,b_cp,t_cp,l_cp,r_cp);
        scrn_to_cP_init(t,b,l,r,b_cp,t_cp,l_cp,r_cp);
        latlon_to_map_init();

        /* draw a `grid` on display window */
/*
        if (draw_grid==1)
        {
                draw_earth_window(earth_wind.north,earth_wind.south,
                                        earth_wind.east,earth_wind.west,
                                        80);
        }
*/

        /* return the number of rows in the data */
        /* (this is needed by the SDDA object, so that it can store
            disk row requests quickly )*/

        return(earth_wind.rows);
}


/* call me before exiting d.rast.PROJ, but after you no longer need PROJ*/      void free_conversions_viewproj (void)
{
        pj_free(proj_ref);  /*free PROJ memory */
        return;
}
