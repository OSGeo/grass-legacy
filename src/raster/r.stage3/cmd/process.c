#include "netcdf.h"
#include <stdio.h>
#include "stage3.h"
static long start[2];
static long count[2];
static HRAP h_point;
static int x_origin, y_origin, left_column=367, bottom_row=263;
static double stlat, stlon;
static LL ll_point;
LL hraptolatlon();
#define RADIAN_TO_DEGREE 57.29
extern void place_points_in_map();
extern void close_map();
extern void map_initialize();

void
process_short(ncdfile, precip_id, precip_type)
int ncdfile;
int precip_id;
nc_type precip_type;
{
	int i, j;
	short *precip_vals;
	int no_rows, no_cols;
stlat = 60.0/RADIAN_TO_DEGREE;
stlon = 105.0;
	no_cols = getcols(ncdfile);
	no_rows = getrows(ncdfile);

	start[0] = 0; start[1] = 0;
	count[0] = 1; count[1] = no_cols;
	precip_vals = (short *)malloc(getsize(precip_type)*no_cols);
	y_origin = bottom_row;

	map_initialize(mapname);
	fprintf(stderr,"Percent complete: ");
	for(i=0;i< no_rows; i++){
		G_percent(i, no_rows, 1);
		start[0] = i;
		if(ncvarget(ncdfile, precip_id, start, count, (void *) precip_vals) == -1){
			fprintf(stderr, "Read error with NETCDF file\n");
			exit (-1);
	 	}		

		x_origin = left_column;
		h_point.y = y_origin;
		for(j=0;j< no_cols; j++){
			h_point.x = x_origin++;
			ll_point =  hraptolatlon(h_point, stlat, stlon);
			if(precip_vals[j] < 0)
				precip_vals[j] = 0;

			place_points_in_map((double)ll_point.lon, (double)ll_point.lat,(double) precip_vals[j]);
			/*
			fprintf(stdout,"%lf %lf %d\n",ll_point.lon, ll_point.lat,
			precip_vals[j]);
			*/
		}
		y_origin = bottom_row++;
			
	}	

	close_map(mapname);
}

void
process_long(ncdfile, precip_id, precip_type)
int ncdfile;
int precip_id;
nc_type precip_type;
{
	int i, j;
	long *precip_vals;
	int no_rows, no_cols;
stlat = 60.0/RADIAN_TO_DEGREE;
stlon = 105.0;
	no_cols = getcols(ncdfile);
	no_rows = getrows(ncdfile);

	start[0] = 0; start[1] = 0;
	count[0] = 1; count[1] = no_cols;
	precip_vals = (long *)malloc(getsize(precip_type)*no_cols);
	y_origin = bottom_row;

	map_initialize(mapname);
	fprintf(stderr,"Percent complete: ");
	for(i=0;i< no_rows; i++){
		start[0] = i;
		if(ncvarget(ncdfile, precip_id, start, count, (void *) precip_vals) == -1){
			fprintf(stderr, "Read error with NETCDF file\n");
			exit (-1);
	 	}		

		x_origin = left_column;
		h_point.y = y_origin;
		for(j=0;j< no_cols; j++){
			h_point.x = x_origin++;
			ll_point =  hraptolatlon(h_point, stlat, stlon);
			if(precip_vals[j] < 0)
				precip_vals[j] = 0;

			place_points_in_map((double)ll_point.lon, (double)ll_point.lat,(double) precip_vals[j]);
			/*
			fprintf(stdout,"%lf %lf %d\n",ll_point.lon, ll_point.lat,
			precip_vals[j]);
			*/
		}
		y_origin = bottom_row++;
			
	}	
}

void
process_float(ncdfile, precip_id, precip_type)
int ncdfile;
int precip_id;
nc_type precip_type;
{
	int i, j;
	float *precip_vals;
	int no_rows, no_cols;
stlat = 60.0/RADIAN_TO_DEGREE;
stlon = 105.0;
	no_cols = getcols(ncdfile);
	no_rows = getrows(ncdfile);

	start[0] = 0; start[1] = 0;
	count[0] = 1; count[1] = no_cols;
	precip_vals = (float *)malloc(getsize(precip_type)*no_cols);
	y_origin = bottom_row;

	map_initialize(mapname);
	fprintf(stderr,"Percent complete: ");
	for(i=0;i< no_rows; i++){
		start[0] = i;
		if(ncvarget(ncdfile, precip_id, start, count, (void *) precip_vals) == -1){
			fprintf(stderr, "Read error with NETCDF file\n");
			exit (-1);
	 	}		

		x_origin = left_column;
		h_point.y = y_origin;
		for(j=0;j< no_cols; j++){
			h_point.x = x_origin++;
			ll_point =  hraptolatlon(h_point, stlat, stlon);
			if(precip_vals[j] < 0)
				precip_vals[j] = 0;

			place_points_in_map((double)ll_point.lon, (double)ll_point.lat,(double) precip_vals[j]);
			/*
			fprintf(stdout,"%lf %lf %d\n",ll_point.lon, ll_point.lat,
			precip_vals[j]);
			*/
		}
		y_origin = bottom_row++;
			
	}	
}

void
process_double(ncdfile, precip_id, precip_type)
int ncdfile;
int precip_id;
nc_type precip_type;
{
	int i, j;
	double *precip_vals;
	int no_rows, no_cols;
stlat = 60.0/RADIAN_TO_DEGREE;
stlon = 105.0;
	no_cols = getcols(ncdfile);
	no_rows = getrows(ncdfile);

	start[0] = 0; start[1] = 0;
	count[0] = 1; count[1] = no_cols;
	precip_vals = (double *)malloc(getsize(precip_type)*no_cols);
	y_origin = bottom_row;

	map_initialize(mapname);
	fprintf(stderr,"Percent complete: ");
	for(i=0;i< no_rows; i++){
		start[0] = i;
		if(ncvarget(ncdfile, precip_id, start, count, (void *) precip_vals) == -1){
			fprintf(stderr, "Read error with NETCDF file\n");
			exit (-1);
	 	}		

		x_origin = left_column;
		h_point.y = y_origin;
		for(j=0;j< no_cols; j++){
			h_point.x = x_origin++;
			ll_point =  hraptolatlon(h_point, stlat, stlon);
			if(precip_vals[j] < 0)
				precip_vals[j] = 0;

			place_points_in_map((double)ll_point.lon, (double)ll_point.lat,(double) precip_vals[j]);
			/*
			fprintf(stdout,"%lf %lf %d\n",ll_point.lon, ll_point.lat,
			precip_vals[j]);
			*/
		}
		y_origin = bottom_row++;
			
	}	
}
