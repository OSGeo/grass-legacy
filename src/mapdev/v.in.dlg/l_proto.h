/* dlg_to_dig.c */
int compare(int, int);
int dlg_to_dig(FILE *, struct Map_info *, FILE *, char *, int, int);
/* head_conv.c */
int hd_dlg_to_dig(struct dlg *, struct dig_head *);
/* pad.c */
int pad(char *, int);
/* r_dlg_head.c */
int read_dlg_head(FILE *, struct dlg *);
/* read_write.c */
int read_int(FILE *, int, int []);
int write_int(FILE *, int, int *);
int read_doubles(FILE *, int, double []);
int write_doubles(FILE *, int, double *, double *);
int write_double(FILE *, int, double []);
int _put_dtype(char *, double *, int, int);
/* rw_bdig.c */
int dig_write_line(struct Map_info *, struct line_pnts *, double *, double *, int, int);
int dig_write_point(struct Map_info *, struct line_pnts *, double *, double *, int);
int breakout_xy(double *, int, double **, double **);
