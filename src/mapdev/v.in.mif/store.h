#ifndef STORE_H_
#define STORE_H_

#define GET_VAL 0
#define SET_VAL 1


/* prototypes */

int G_process_snap_distance(int, double *);
int G_process_colinear_tolerance(int, double *);
int G_process_bbox_params(int, double *, double *);
int G_process_key_params(int, int *, double *, double *);
int G_process_scale_value(int, int *);

#endif  /* STORE_H_ */
