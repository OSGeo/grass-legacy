/* close_down.c */
int close_down(void);
/* disp_graph.c */
int disp_graph(int, int, int *, int);
int plot_cross(int, int);
int plot_box(int, int, int, int, int, int, int);
/* feature.c */
CELL feature(float *);
/* find_normal.c */
int find_normal(float **, double *);
int find_obs(CELL *, float *, double *);
int find_weight(double *);
/* init_graphics.c */
int init_graphics(void);
/* interface.c */
int interface(int, char *[]);
/* lubksub.c */
int lubksub(float **, int, int *, float *);
/* ludcomp.c */
int ludcomp(float **, int, int *);
/* nrutil.c */
void nrwarn(char []);
void nrerror(char []);
float *vector(long, long);
int *ivector(long, long);
unsigned char *cvector(long, long);
unsigned long *lvector(long, long);
double *dvector(long, long);
float **matrix(long, long, long, long);
double **dmatrix(long, long, long, long);
int **imatrix(long, long, long, long);
float **submatrix(float **, long, long, long, long, long, long);
float **convert_matrix(float *, long, long, long, long);
float ***f3tensor(long, long, long, long, long, long);
void free_vector(float *, long, long);
void free_ivector(int *, long, long);
void free_cvector(unsigned char *, long, long);
void free_lvector(unsigned long *, long, long);
void free_dvector(double *, long, long);
void free_matrix(float **, long, long, long, long);
void free_dmatrix(double **, long, long, long, long);
void free_imatrix(int **, long, long, long, long);
void free_submatrix(float **, long, long, long, long);
void free_convert_matrix(float **, long, long, long, long);
void free_f3tensor(float ***, long, long, long, long, long, long);
/* open_files.c */
int open_files(void);
/* param.c */
CELL param(int, float *);
/* process.c */
int process(void);
/* stats.c */
CELL stats(int, float *, CELL *, double *);
