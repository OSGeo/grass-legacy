/* NR.c */
void ludcmp(float **, int, int *, float *);
void lubksb(float **, int, int *, float []);
/* d2nsamp.c */
void dist_to_n_smpls(double, double);
int cmp(void *, void *);
/* dbtnsamp.c */
void dist_btw_n_smpls(double);
/* gaussj.c */
void gaussj(float **, int, float **, int);
/* krigvar.c */
int krig_var_est(int);
/* krigwts.c */
void krig_weights(void);
/* krigzest.c */
int krig_z_est(int);
/* main.c */
int main(int, char *[]);
/* misc.c */
void pr_utdm_v(double *, int, int, int);
double *dvector(int, int);
float *vector(int, int);
int *ivector(int, int);
float **matrix(int, int, int, int);
double **dmatrix(int, int, int, int);
/* newpoint.c */
int newpoint(double, double, double);
/* nrutil.c */
void nrerror(char []);
int **imatrix(int, int, int, int);
float **submatrix(float **, int, int, int, int, int, int);
void free_vector(float *, int, int);
void free_ivector(int *, int, int);
void free_dvector(double *, int, int);
void free_matrix(float **, int, int, int, int);
void free_dmatrix(double **, int, int, int, int);
void free_imatrix(int **, int, int, int, int);
void free_submatrix(float **, int, int, int, int);
float **convert_matrix(float *, int, int, int, int);
void free_convert_matrix(float **, int, int, int, int);
/* read_cell.c */
int read_cell(char *);
/* read_sites.c */
int read_sites(char *);
/* sphdist.c */
double sphere_dist(double, double, double, double);
/* variogrm.c */
void variogram_model(int, double, double, double, double, double);
/* NR.c */
void ludcmp(float **, int, int *, float *);
void lubksb(float **, int, int *, float []);
/* d2nsamp.c */
void dist_to_n_smpls(double, double);
int cmp(void *, void *);
/* dbtnsamp.c */
void dist_btw_n_smpls(double);
/* gaussj.c */
void gaussj(float **, int, float **, int);
/* krigvar.c */
int krig_var_est(int);
/* krigwts.c */
void krig_weights(void);
/* krigzest.c */
int krig_z_est(int);
/* main.c */
int main(int, char *[]);
/* misc.c */
void pr_utdm_v(double *, int, int, int);
double *dvector(int, int);
float *vector(int, int);
int *ivector(int, int);
float **matrix(int, int, int, int);
double **dmatrix(int, int, int, int);
/* newpoint.c */
int newpoint(double, double, double);
/* nrutil.c */
void nrerror(char []);
int **imatrix(int, int, int, int);
float **submatrix(float **, int, int, int, int, int, int);
void free_vector(float *, int, int);
void free_ivector(int *, int, int);
void free_dvector(double *, int, int);
void free_matrix(float **, int, int, int, int);
void free_dmatrix(double **, int, int, int, int);
void free_imatrix(int **, int, int, int, int);
void free_submatrix(float **, int, int, int, int);
float **convert_matrix(float *, int, int, int, int);
void free_convert_matrix(float **, int, int, int, int);
/* read_cell.c */
int read_cell(char *);
/* read_sites.c */
int read_sites(char *);
/* sphdist.c */
double sphere_dist(double, double, double, double);
/* variogrm.c */
void variogram_model(int, double, double, double, double, double);
