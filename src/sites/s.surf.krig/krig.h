#include "la.h"

struct p_n_t
{
  double north, east;
  double z;
  double dist;
};
typedef struct p_n_t Point;

/* d2nsamp.c */
void dist_to_n_smpls(double, double);
int cmp(const void *, const void *);
/* dbtnsamp.c */
void dist_btw_n_smpls(double);
/* krigvar.c */
double krig_var_est(void);
/* krigwts.c */
void krig_weights(void);
/* krigzest.c */
double krig_z_est(void);
/* newpoint.c */
int newpoint(double, double, double);
/* read_sites.c */
int read_sites(char *);
/* variogrm.c */
void variogram_model(int, double, double, double, double, double);
