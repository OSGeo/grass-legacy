/* %W% %G% */

struct UTM
{
    double north;
    double south;
    double east;
    double west;
    double ns_res;
    double ew_res;
    int ncols;
    int nrows;
    int bpc;
    int sflag;
};
/* input.c */
int input(char *, char *, char *, char *, char *);
/* isnumber.c */
int isnumber(char *);
int getrow(int, char *, int, int);
/* output.c */
int output(char *);
/* row_col.c */
int row_col(struct UTM *, double, double, int *, int *);
/* sph.c */
char *spheroid_list(void);
/* utm_value.c */
#ifdef GRASS_ROWIO_H
int utm_value(double, double, struct UTM *, ROWIO *);
#endif
/* value.c */
int value(register unsigned char *, int, int);
/* warning.c */
int warning(char *, int, char *);
