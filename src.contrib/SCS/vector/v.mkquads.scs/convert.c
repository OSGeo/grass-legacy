
/*
*  setup_proj ()  - select the projection for conversions to come
*  find_quad_point ()  -  find the lowest left quad point
*  convert_window_to_ll - convert the  window to lat, lon
*  convert_ll_to_proj()  - convert a lat/lon coordinate to proj
*  convert_proj_to_ll()  - convert a coordinate to lat/lon
*  other misc functions.
*  Written by GRASS Team, Fall of 88, -mh
*/

#include	<stdio.h>
#include	<math.h>
#include	<string.h>
#include	<ctype.h>
#include	"gis.h"
#include	"quad_structs.h"

#define PROJECTION_FILE "PROJ_INFO"
/********************* PARMS FOR PROJ **********************/
#define RAD_TO_DEG     57.29577951308232
#define STP_FT  0.30480060960121920243
typedef struct { double u, v; }     UV;


/**** routines for projection change ********/
extern UV (*ProjSetup())();
static char *oform = (char *)0; /* output format */
static UV (*proj1)();      /* projection function 1*/
static UV (*proj2)();      /* projection function 2*/
static double STP = 1.0;   /* State Plane factor to feet */
 
 do_INV(u,v) double *u, *v;
 {
	UV data;    /*        data structure supplied to proj conv. routines */
	/*DEBUG fprintf(stderr,"IN1 %lf %lf\n",*u,*v);*/
	data.u = *u * STP; data.v = *v * STP;
	data = (*proj1)(data);
	*u = data.u * RAD_TO_DEG; *v = data.v * RAD_TO_DEG;
	/*DEBUG fprintf(stderr,"OUT %lf %lf\n",*u,*v);*/
	}
			  
do_FOR(u,v) double *u, *v;
{
	UV data;    /*        data structure supplied to proj conv. routines */
	/*DEBUG fprintf(stderr,"IN2 %lf %lf\n",*u,*v);*/
	data.u = *u / RAD_TO_DEG; data.v = *v / RAD_TO_DEG;
	data = (*proj2)(data);
	*u = data.u / STP; *v = data.v / STP;
	/*DEBUG fprintf(stderr,"OUT %lf %lf\n",*u,*v);*/
}
/***********  End PROJECTION CHANGE  ********************/
						   


setup_proj (quads_info)
	struct  quads_description  *quads_info ;
{
	char buffa[100], ipath[256], proj_in[50];
	struct Key_Value *in_proj_keys;
	int in_stat, i;
 
	/*** Get projection info for current mapset ***/
	G__file_name (ipath, "", PROJECTION_FILE, G_mapset());
	while (access(ipath,0) != 0)
		{
		fprintf(stderr,"ifile %s\n",ipath);
		fprintf(stderr,"PROJ_INFO file not found  for mapset %s\n",
	  G_mapset());
		fprintf(stderr,
		"Projections are: utm,aea,stp,ll,lcc,merc,tmerc,xxx\n");
		fprintf(stderr,"Enter projection for input mapset : ");
		scanf("%s",proj_in);
		fprintf(stderr,"Running m.setproj\n");
		sprintf(buffa,"m.setproj set=%s proj=%s",G_mapset(),proj_in);
		system(buffa);
		}
	in_proj_keys = G_read_key_value_file(ipath,&in_stat);
	if (in_stat != 0)
		{
		sprintf(buffa,"ERROR in reading current mapset PROJ_INFO \n");
		G_fatal_error(buffa) ;
		}
	sprintf(proj_in,"%s", G_find_key_value("name",in_proj_keys));
	if (strcmp(proj_in,"Lat/Long") == 0) {
		quads_info->proj_cmd[0] = NULL;
		LAT_LON = 1;
		}
	else {
		for (i=1; i<=in_proj_keys->nitems-1; i++)
			{
			sprintf(buffa,"+%s=%s\t",
			in_proj_keys->key[i],in_proj_keys->value[i]);
			strcat(quads_info->proj_cmd,buffa);
			}
		G_free_key_value(in_proj_keys);
		}

	oform = "%.10f";

	return(0) ;

}


find_quad_point (Q, W_ll, flags)
	struct  quads_description  *Q ;
	struct  Cell_head  *W_ll ;
	struct  command_flags  *flags ;
{

    int  ret ;
    int  *zone ;
    int  num_lat_quads ;	/* num of quads from equator  */
    int  num_lon_quads ;
    double south, west ;

/*  lon is east/west,  lat is north/south  */

/* is it the northern hemisphere ?  */
	if (W_ll->south > W_ll->north) Q->north = 0;
	else Q->north = 1;

/*  find closet quad point in the window  */
	south = W_ll->south ;
	west = W_ll->west ;

	num_lat_quads = south/QUAD_SIZE;
	num_lon_quads = west/QUAD_SIZE;

	if (flags->encompass){
		if (Q->north)
		{
			if (num_lat_quads * QUAD_SIZE> south)
				--num_lat_quads ;
		}
		else
		{  
			if (num_lat_quads * QUAD_SIZE< south)
				++num_lat_quads ;
		}  

		if (num_lon_quads * QUAD_SIZE> west)
			--num_lon_quads ;
	}

	Q->origin_lat = num_lat_quads * QUAD_SIZE;
	Q->origin_lon = num_lon_quads * QUAD_SIZE;


/*  convert the user given point to currrent projection  */
	ret = convert_ll_to_proj ( Q->origin_lon, Q->origin_lat,
		 &Q->origin_x,  &Q->origin_y,
		 Q) ;

#ifdef DEBUG
print_quad(Q) ;
#endif

	if ( ret < 0)
	{
		fprintf( stderr, "\n Error: Couldn't convert quad point\n") ;
		exit (-1) ;
	}



	return(0) ;

}


convert_window_to_ll (W,Q)
	struct  Cell_head  *W ;
	struct  quads_description  *Q ;
{
/* if already lat/lon do nothing */
	if (LAT_LON) return(0);

/*  convert south west point of window  */
	convert_proj_to_ll( &W->west, &W->south, W->west, W->south,Q);

/*  convert north east point of window  */
	convert_proj_to_ll( &W->east,&W->north, W->east, W->north,Q);

#ifdef DEBUG
print_wind( W, " window of ll") ;
#endif DEBUG


	return(0) ;

}


convert_proj_to_ll( lon, lat, x, y, Q)
	double  *lon, *lat, x, y ;
	struct  quads_description  *Q ;
{
	double E,N;
	char cmdinv[300];

/* if already lat/lon do nothing */
	if (LAT_LON) return(0);


	sprintf(cmdinv,"%s +inv",Q->proj_cmd);
	set_proj(cmdinv,1);
	E = x; N= y;
	do_INV(&E,&N);
	*lon = E;
	*lat = N;
	return(0);
}
convert_ll_to_proj( lon, lat, x, y, Q)
	double  lon, lat, *x, *y ;
	struct  quads_description  *Q ;
{
	double E,N;

/* if already lat/lon do nothing */
	if (LAT_LON) return(0);

	set_proj(Q->proj_cmd,0);
	E = lon;
	N = lat;
	do_FOR(&E,&N);
	*x = E;
	*y = N;
	return(0);
}

ret_check( funct_no, ret_value)
	int  funct_no, ret_value ;
{

	if (ret_value >= 0)
		return(0) ;

	fprintf(stderr, " ERROR: %d) Couldn't convert coordinates\n", funct_no) ;

}

/*  debugging tools follow  */

print_quad( Q) 
	struct  quads_description  *Q ;
{
    printf("\n  ------  Printing quad ---- \n") ;
	printf("proj  %s\n",Q->proj_cmd);
    printf("  origin_lat: %lf,  and  origin_lon %lf\n",
		 Q->origin_lat,  Q->origin_lon) ;
    printf("  origin_x: %lf,  and  origin_y %lf\n",
		 Q->origin_x,  Q->origin_y) ;
    printf("  lat_shift: %lf,  and  lon_shift %lf\n",
		 Q->lat_shift,  Q->lon_shift) ;

}


print_wind( W, desc) 
	struct  Cell_head  *W ;
	char  *desc ;
{
    printf("\n  ------  Printing window: '%s'\n", desc) ;
    printf("  Zone: %d\n",
		 W->zone) ;

    printf("  north: %lf,  and  south %lf\n",
		 W->north,  W->south) ;

    printf("  east: %lf,  and  west %lf\n",
		 W->east,  W->west) ;

}


set_proj(cmnd,n)
char *cmnd;
int n;
{
static int INV=9999;
char *p;
/*DEBUG fprintf(stderr,"\nSET_UP __ %s\nN_%d n_%d\n",cmnd,INV,n);*/
if (INV != n) {
	INV = n;
	p = strtok(cmnd," \t");
	load_opt(p, 0);
	while ((p = strtok(NULL," \t")) != NULL){
	load_opt(p, 0);
		}

/* if already lat/lon do nothing */
	if (LAT_LON) return(0);

	if (n == 1) 
			proj1 = ProjSetup(1);
	else
		proj2 = ProjSetup(0);
	}
}

