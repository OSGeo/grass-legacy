extern char timefile[64] ;
extern int max,nsn,cbw,bw,noe;
extern int basin_no,no_stream,basinhydrograph,animate,infilmap;
extern int duration, mode,delta_t,out_node,monit_time;
extern int *nin,*nout,*sn,*stream_node;
extern double *easting,*northing,*le,*rhf,*capacitance,*manning,*slope,*exponent;
extern double width,nslope,base,rain_max,area;
extern short int **stiffness,*A;
extern char manningsmap[100],sat_cond_map[100],cap_suc_map[100],porosity_map[100],deg_of_s_map[100],slopemap[100];
extern  char element[64],filename[200];

/* band.c */
void find_row_no_and_element(int *, int *, int, int);
void store_matrix(short int **, short int *, int, int, int);
void band_multiply(short int *, double *, double *, int, int, int);
/* draw.c */
void draw_hydrograph(void);
void make_setup(void);
int putxy(double *, double *, int);
/* infiltration.c */
void infiltration(void);
/* main.c */
int absolute(int);
void calc_band_width(int *, int *, int, int *);
/* mstifgeo.c */
int compare(const void *, const void *);
void make_capacitance(void);
void makestiffness_capacitance(void);
/* other.c */
void get_cell_values(double *, double *, int, double *, char *);
void const_value(char *, int, double *, double, double);
/* timecalc.c */
void timecalc(void);
/* timedata.c */
void timedata(void);
