#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <float.h>
#include "dem.h"

/***************************************************************************/
/**** Reads the header from 'dem_ascii' and fills in 'head' appropriately. */
/***************************************************************************/
signed char read_dem_head(struct dem_head *head, FILE *dem_ascii){
  char d_str1[25], d_str2[25], *chrptr;
  unsigned int i;

  if (fscanf(dem_ascii, "%144c %i %i %i %i %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %i %i %u ",
	     head->name,
	     &head->dem_level,
	     &head->data_type,
	     &head->proj,
	     &head->zone_code,
	     &head->proj_params[0],  &head->proj_params[1], 
	     &head->proj_params[2],  &head->proj_params[3],
	     &head->proj_params[4],  &head->proj_params[5],
	     &head->proj_params[6],  &head->proj_params[7],
	     &head->proj_params[8],  &head->proj_params[9],
	     &head->proj_params[10], &head->proj_params[11],
	     &head->proj_params[12], &head->proj_params[13],
	     &head->proj_params[14],
	     &head->ground_units,
	     &head->elev_units,
	     &head->num_sides)
    != 23){
    fprintf(stderr, "Error: First Header Read Failed.");
    return -1;
  }
    head->name[144]='\0';

  if (! (head->corner_list = (double *)G_calloc(head->num_sides * 2, sizeof(double))))
    fprintf(stderr, "Error: malloc(corner coordinates) Failed.");

  for (i=0; i < head->num_sides * 2; i+=2){
    if (fscanf(dem_ascii, "%s %s ", d_str1, d_str2) != 2){
      fprintf(stderr, "Error: Second Header Read (corner points) Failed.");
      return -2;
    }
    if ((chrptr = strchr(d_str1, 'D'))) *chrptr = 'E';
    head->corner_list[i] = -(atof(d_str1)); /* Longitude, negated so that the 
					       coorconv library doesn't get
					       E/W reversed */
    if ((chrptr = strchr(d_str2, 'D'))) *chrptr = 'E';
    head->corner_list[i+1] = atof(d_str2);           /* Latitude  */
  }

  if (fscanf(dem_ascii, "%s %s ", d_str1, d_str2) != 2){
    fprintf(stderr, "Error: Third Header Read (min, max) Failed.");
    return -3;
  }
  if ((chrptr = strchr(d_str1, 'D'))) *chrptr = 'E';
  head->min = atof(d_str1);
  if ((chrptr = strchr(d_str2, 'D'))) *chrptr = 'E';
  head->max = atof(d_str2);

  /* In my test DEM, some of the floats ran into each other, thus giving bad 
     readings, so I have to specify the widths here to prevent over-reads. I 
     just hope all files are the same width format... */
  if (fscanf(dem_ascii, "%f %1i%12f%12f%12f %u %u ",
	     &head->axis_angle,
	     &head->accuracy_code,
	     &head->resolution[0], &head->resolution[1],
	     &head->resolution[2],
	     &head->rows, &head->cols)
      != 7){
    fprintf(stderr, "Error: Fourth Header Read Failed.");
    return -4;
  }
return 1;
}

/**************************************************************************/
/* Reads a single record from 'dem_ascii', assuming that the file         */
/* position is at the beginning of a record. row and col values are for   */
/* error-checking only.                                                   */
/**************************************************************************/
signed char read_dem_record(struct dem_data *record, const unsigned int row, const unsigned int col, FILE *dem_ascii){
  char *chrptr, d_str[4][30];
  unsigned int i;
  double realmax = DBL_MIN, realmin = DBL_MAX; /* defined in limits.h */

  /* Get record header info */
  if (fscanf(dem_ascii, "%u %u %u %u %29s %29s %f %29s %29s ",
	 &record->profile_id[0], &record->profile_id[1],
	 &record->rows, &record->cols,
	 d_str[0], d_str[1],
	 &record->local_datum,
	 d_str[2], d_str[3])
      != 9){
    fprintf (stderr, "Error: Profile Record Head Read Failed.");
    return -1;        /* Read failure */
  }
 /* Get start coordinates */
  for (i=0; i<4; i++) if ((chrptr = strchr(d_str[i], 'D'))) *chrptr = 'E';
  record->start_coord[0] = -(atof(d_str[0])); /* Invert lon for, as above */ 
  record->start_coord[1] = atof(d_str[1]);
  record->min = atof(d_str[2]);
  record->max = atof(d_str[3]);

  if (! (record->data = malloc(record->rows * record->cols * sizeof(*record->data)))){
    fprintf (stderr, "Error: malloc(profile data) Failed.");
    return -2;       /* Malloc failure */
  }
  for (i=0; i< (record->rows * record->cols); i++){
    if (fscanf(dem_ascii, "%i ", record->data + i) != 1){
      fprintf(stderr, "Error: Data Read Failed.");
      return -3;     /* Read failure */
    }
    if ((double)*(record->data+i) < realmin) realmin = record->min;
    else if ((double)*(record->data+i) > realmax) realmax = record->max;
  }
  if (row != record->profile_id[0] || col != record->profile_id[1]) 
    return -4;       /* Record ID mismatch */
  if (realmax != record->max) return -5;
  if (realmin != record->min) return -6;
  return 1;          /* Success */
}

/***************************************************************************/
/* Reads an accuracy code */
signed char read_dem_accuracy(struct dem_accuracy *acc, FILE *dem_ascii){

  if (! (fscanf(dem_ascii, "%i %i %i %i %i %i %i %i %i %i ",
		&acc->file_abs_availible,
		&acc->file_abs[0], &acc->file_abs[1], &acc->file_abs[2],
		&acc->file_abs_samp_size,
		&acc->DEM_file_availible,
		&acc->DEM_file[0], &acc->DEM_file[1], &acc->DEM_file[2],
		&acc->DEM_file_samp_size))
      != 10){
    fprintf(stderr, "Error: Accuracy Record Read Failed.");
    return -1;
  }
  return 1;
}



