/* decl.c */
int scan_declination(char *, double *);
/* main.c */
int parse_units(char *);
int match(char *, char *, int);
/* trig.c */
double dcos(double);
double dsin(double);
/* vect.c */
int openvect(char *, char *);
int writeline(double *, double *, int, double, double, CELL, char *);
int closevect(void);
