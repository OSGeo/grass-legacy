/** Format info from http://edcwww.cr.usgs.gov/glis/hyper/guide/1_dgr_dem **/

#define DATA_REG_ELEV   1 /* data_type   		*/
#define PROJ_LL		0 /* coord_sys          	*/
#define XY_RADIANS      0 /* ground_units               */
#define XY_FEET         1
#define XY_METERS       2
#define XY_ARC_SEC	3
#define EL_METERS 	2 /* elev_units 		*/
#define ACC_CODE_NO	0 /* accuracy_code		*/
#define ACC_CODE_YES    1 

struct dem_head{ /******** type A record ********/
  double *corner_list;     /* 1 pair per side: lon, lat */
  double min, max;         /* superlative values */
  float proj_params[15];
  float axis_angle;        /* Counterclockwise angle from the primary axis */
  float resolution[3];	   /* x,y,z spatial resolution */
  int dem_level;
  int data_type;
  int proj;
  int zone_code;
  int ground_units;
  int elev_units;
  int accuracy_code;
  unsigned int num_sides;  /* Number of sides to DEM region (usually 4) 
			      of ground planimetric referenced to the primary
			      axis of the DEM local reference system. */
  unsigned int rows, cols;
  char name[145];
};
  
struct dem_data{ /******** type B record ********/
  double start_coord[2];       /* coords of first element in profile */
  double min, max;              /* superlative values */
  float local_datum;
  unsigned int profile_id[2];   /* row, col id of profile */
  unsigned int rows, cols;
  int *data;
};

struct dem_accuracy{ /******** type C record ********/
  int file_abs_availible; /* boolean for existence of file_abs */
  int file_abs[3];        /* RMSE of file datum rel. to absolute */
  int file_abs_samp_size;
  int DEM_file_availible; /* boolean for existence of DEM_file */
  int DEM_file[3];        /* RMSE of DEM data rel. to file datum */
  int DEM_file_samp_size;
};

signed char read_dem_head(struct dem_head *head, FILE *dem_ascii);
signed char read_dem_record(struct dem_data *record, const unsigned int row, const unsigned int col, FILE *dem_ascii);
signed char read_dem_accuracy(struct dem_accuracy *acc, FILE *dem_ascii);










