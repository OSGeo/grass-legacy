/****************************************************************/
/*                                                              */
/*      This header file declares the global variables and the  */
/*      structures that are to be used for command line         */
/*      processing                                              */
/*                                                              */
/****************************************************************/

#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif

GLOBAL char *backdrop_layer;
GLOBAL char *base_layer;
GLOBAL char *dir_layer;
GLOBAL char *max_layer;
GLOBAL char *spotdist_layer;
GLOBAL char *mois_layer;
GLOBAL char *out_layer;
GLOBAL char *start_layer;
GLOBAL char *velocity_layer;
GLOBAL char *x_out_layer;
GLOBAL char *y_out_layer;

GLOBAL float comp_dens;
GLOBAL int display;
GLOBAL int init_time;
GLOBAL int least;
GLOBAL int spotting;
GLOBAL int time_lag;
GLOBAL int verbose;
GLOBAL int x_out;
GLOBAL int y_out;
