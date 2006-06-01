#define BUFFSIZE 256

#define METHOD_N         1
#define METHOD_MIN       2
#define METHOD_MAX       3
#define METHOD_RANGE     4
#define METHOD_SUM       5
#define METHOD_MEAN      6
#define METHOD_STDDEV    7
#define METHOD_VARIANCE  8
#define METHOD_COEFF_VAR 9

/* main.c */
int scan_bounds(FILE*, int, int, int, char*);

/* support.c */
int blank_array(void *, int, int, RASTER_MAP_TYPE, int);
int update_n(void *, int, int, int);
int update_min(void *, int, int, int, RASTER_MAP_TYPE, double);
int update_max(void *, int, int, int, RASTER_MAP_TYPE, double);
int update_sum(void *, int, int, int, RASTER_MAP_TYPE, double);
int update_sumsq(void *, int, int, int, RASTER_MAP_TYPE, double);
