/* get_prof.c */
int get_profile(void);
/* getgrid.c */
int getgrid(void);
int skip_columns(void);
int skip_rows(void);
int next_profile(void);
int next_elev(void);
/* usgs_buf.c */
int get_buf(void);
int skip_file(void);
int next_record(void);
/* usgs_hdr.c */
int get_hdr(void);
int hdr_list(FILE *);
int window_list(FILE *);
int fail_return(char *);
/* usgs_init.c */
int usgs_init(int);
/* usgs_num.c */
int get_int(int *);
int get_double(double *);
int get_float(float *);
