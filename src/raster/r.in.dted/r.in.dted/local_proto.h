/* dted_read.c */
int sbytes_to_int(unsigned char *inc, int num);
int ubytes_to_int(unsigned char *inc, int num);
int dmshtodd(char *, double *);
int get_header(FILE *, struct Cell_head *, int *);
int add_dted_hist(FILE *, struct History *);
int read_record(FILE *, int, dted_d *);
int dted_zone_compute(double, long *);
int check_record(FILE *, int);
/* read_write.c */
int do_read_write(FILE *, int, int, int, int, char *, int);
