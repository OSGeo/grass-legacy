/* v.in.tig.bas.c */
int cmp_type2(const void *, const void *);
int usage(char *);
double mscale(double);
int make_sort_name(char *, FILE *);
int set_max_min(double *, double *, int, double *, double *, double *, double *);
int make_utms(double *, double *, int *, int);
int r_shape_file(struct r2 *, double *, double *, FILE *);
int ll_from_str(char *, double *, double *);
int my_atoi(char *, int);
int get_tiger_record(FILE *, int, char *);
int type2_number(FILE *);
int check_wanted(int, FILE *);
