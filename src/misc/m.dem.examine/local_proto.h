/* profile.c */
int get_profile(void);
/* usgs_buf.c */
int get_buf(void);
int skip_file(void);
int opentape(char *);
int next_record(void);
/* usgs_hdr.c */
int get_hdr(void);
int hdr_list(FILE *);
int window_list(FILE *);
/* usgs_num.c */
int get_int(int *);
int get_dfloat(float *);
int get_efloat(float *);
int nget_int(int *);
int get_double(double *);
int get_float(float *);
