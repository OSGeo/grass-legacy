void get_stp_proj(char[]);
int get_stp_code(int, char[]);
int get_stp_num(void);
int ask_fips(FILE *, int *, int *, int *);
int set_proj_bool_opts(int index, char *parms, char *buffer);
int set_proj_nonbool_opts(int index, char *parms, char *buffer, double value);
int proj_index_in, proj_index_out;
int get_input(char *, char *, char *, char *, char *);
int put_output(char *, int);

#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif
GLOBAL FILE *In_file, *Out_file, *Mem_file;
/* GLOBAL static char aline[80], answer[24], null = '\0';
GLOBAL static char command[1024], sav_comd[1024], parms_in[256], parms_out[256];*/
GLOBAL char answer[24];
GLOBAL char parms_in[256], parms_out[256];
GLOBAL char buff[300], in_file[60], out_file[60];
GLOBAL char *ptr, STabbr[3], COname[30], FIPSfile[60], TXT_ZONE[5];
GLOBAL char units_in[50], units_out[50];
GLOBAL double unit_fact_in, unit_fact_out;
GLOBAL char ellps_name_in[20], ellps_name_out[20], proj_name_in[20], proj_name_out[20];
GLOBAL char proj_title_in[100], proj_title_out[100];
/* int ier, icode, SFIPS, CFIPS, fipscode, COzone, NUM_ZON, lookup; */
GLOBAL int icode, SFIPS, CFIPS, fipscode, COzone, NUM_ZON, lookup;
GLOBAL int record, reccnt;
GLOBAL int i, conv_typ, conv_way;
GLOBAL int input_typ, output_typ, rec_cnt;
GLOBAL int IDEG, IMIN, JDEG, JMIN, ST_zone;
GLOBAL float XDEG, XMIN, YDEG, YMIN, Q, XSEC, YSEC;
/* current lat and lon - equal either LAT and LON or LAT_res, LON_res */ ;
GLOBAL double LAT, LON, X, Y, LAT_res, LON_res, cur_LAT, cur_LON;
GLOBAL double NOR, EAS, NOR_res, EAS_res;
GLOBAL char check;
GLOBAL double radius_in, radius_out;

/*prototypes */
/* geo_func.c */
int init_used_table(void);
int get_enz(void);
int get_easting(void);
int get_northing(void);
int get_float(int,double *,int,int);
int get_ll(void);
int get_lat(void);
int get_lon(void);
int get_LL_stuff(int,double *,int,int,int);
int get_num(char *, int);
/* get_deg.c */
int get_deg(char *, double *, int);
/* get_stp.c */
void get_stp_proj(char[]);
/* main.c */
int min1(int, int);
/* process.c */
int process(int, char *, char *, char *, char *, double *, struct used_opt *, char *);
/* table.c */
int init_table(void);
int get_proj_index(char *);
int init_unit_table(void);
int get_sphere_radius(double *);
