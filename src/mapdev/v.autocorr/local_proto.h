/* com.c */
void advnst(long);
void getsd(long *, long *);
long ignlgi(void);
void initgn(long);
void inrgcm(void);
void setall(long, long);
void setant(long);
void setsd(long, long);
/* geary_moran.c */
int geary_moran(double [], double *, double *, int);
/* linpack.c */
float sdot(long, float *, long, float *, long);
void spofa(float *, long, long, long *);
/* mean.c */
int mean(double [], double *, double *, double *, double *, int);
/* ranlib.c */
float genbet(double, double);
float genchi(double);
float genexp(double);
float genf(double, double);
float gengam(double, double);
void genmn(float *, float *, float *);
float gennch(double, double);
float gennf(double, double, double);
float gennor(double, double);
void genprm(long *, int);
float genunf(double, double);
void gscgn(long, long *);
void gsrgs(long, long *);
void gssst(long, long *);
long ignbin(long, double);
long ignpoi(double);
long ignuin(long, long);
long lennob(char *);
long mltmod(long, long, long);
void phrtsd(char *, long *, long *);
float ranf(void);
void setgmn(float *, float *, long, float *);
float sexpo(void);
float sgamma(double);
float snorm(void);
float fsign(double, double);
/* std_err.c */
int std_err(double [], double *, double *, double *, double *, int);

#ifdef GRASS_VECT_H
/* att2cat.c */
double att2cat(struct Map_info *, int);
/* c_matrix.c */
int c_matrix(struct Map_info *);
/* init_plus_s.c */
int init_plus_struct(struct Plus_head *);
int init_map_struct(struct Map_info *);
/* open_files.c */
int open_dig_files(char *, FILE **, struct Map_info *, struct Plus_head *);
/* printmatrix.c */
int printmatrix(FILE *, int);
#endif
