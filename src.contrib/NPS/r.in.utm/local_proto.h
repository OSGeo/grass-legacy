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
int utm_value(double, double, struct UTM *, ROWIO *);
/* value.c */
int value(register unsigned char *, int, int);
/* warning.c */
int warning(char *, int, char *);
