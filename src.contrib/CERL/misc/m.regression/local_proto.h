/* copy_data.c */
int main(void);
/* correlation.c */
int correlation(double *, double *, int, double *);
/* errfun.c */
double err2(double *, double *, int);
double rms(double *, double *, int);
void function(double *, int, double *, int);
int show_parms(FILE *, double *, int);
int yes(char *);
/* gasdev.c */
double gasdev(int *);
/* gaussj.c */
void gaussj(double **, int, double **, int);
/* main.c */
int main(int, char *[]);
/* modify.c */
int modify(char *, double *, int);
/* mrqcof.c */
int mrqcof(double [], int, double [], int, double [], int, double **, double [], double [], double *, void (*)(void));
/* mrqfit.c */
int mrqfit(double [], int, double [], int, double [], int, double **, double **, double *, void (*)(void), double *);
/* nlfit.c */
int nlsetup(int, int, int, void (*)(void));
int nlfinish(void);
int nlfit(double *, double *, double *, double *);
/* ran1.c */
double ran1(int *);
/* rational.c */
int rational(double *, int, double *, int, double *, double *);
/* read_data.c */
int read_data(FILE *, FILE *, double **, int *, double **, int *);
int nonlinear_data(double *, int *, double *, double *, int *, double **, int *);
/* solve.c */
solve(double **, double *, int);
/* svbksb.c */
void svbksb(double **, double [], double **, int, int, double [], double []);
/* svdcmp.c */
void svdcmp(double **, int, int, double *, double **);
/* svdfit.c */
void svdfit(double [], double [], int, double [], int, double **, double **, double [], void (*)(void));
/* svdfit2.c */
void svdfit2(double [], double [], double [], int, double [], int, double **, double **, double [], void (*)(void));
/* svdfitn.c */
void svdfitn(double [], int, double [], int, double [], int, double **, double **, double [], void (*)(void));
/* svdvar.c */
void svdvar(double **, double [], int, double **);
/* try_rational.c */
int main(int, char *[]);
double err2(double *, double *, int);
double rms(double *, double *, int);
double deviation(void);
/* try_xy.c */
int main(int, char *[]);
void basis(double, double, double *, int);
char *bname(int);
double evaluate(double, double, double *, double *, int);
double err2(double *, double *, int);
double rms(double *, double *, int);
int deviation(double *, double *);
int random_deviation(double *, double *, double);
int circle_deviation(double *, double *, double);
int print_eqn(char *, double *, double *, int);
/* utils.c */
void nrerror(char *);
int *ivector(int);
int **imatrix(int, int);
double *vector(int);
double **matrix(int, int);
int free_ivector(int *);
int free_imatrix(int **);
int free_vector(double *);
int free_matrix(double **);
/* xformation.c */
normalization_x1(double *, int *, int *, double **, int *);
int normalization_x2(double *, int *, int *, double **, int *);
int ndvi(double *, int *, int *, double **, int *);
int ndvi_intensity(double *, int *, int *, double **, int *);
int ndvi_reflectance(double *, int *, int *, double **, int *);
int rvi(double *, int *, int *, double **, int *);
int nonlinear_linearization(FILE *, double *, int *, int *, double *, double *, double **);
int nonlinear_linearization_NDVI(FILE *, double *, int *, int *, double *, double *, double **);
int relaxation(FILE *, double *, int *, int *, double *, double *);
int prediction_linear(FILE *, double *, double *, int, int, double *, int);
int prediction_linear_other(FILE *, char *, int, double *, int);
int prediction_nonlinear(FILE *, double *, double *, int, int, double *, int);
int prediction_nonlinear_other(FILE *, char *, double *, int);
