struct p_n_t
{
  double north, east;
  double z;
  double dist;
};
typedef struct p_n_t Point;

int cmp ();
int krig_z_est ();
int krig_var_est ();
void dist_to_n_smpls ();
void dist_btw_n_smpls ();
void variogram_model ();
void krig_weights ();
double sphere_dist ();
float *vector ();
float **matrix ();
void lucmp ();
void lubsk ();

