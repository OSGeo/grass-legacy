/* draw_line.c */
int draw_line(int, int, int, int, int, int);
/* msurements.c */
int measurements(int, int, int, int, int);
int print_en(double, double, int);
int print_length(double, int, int);
int add_point(double **, double **, int *, int *, double, double);

#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif

GLOBAL int leftb, middleb, rightb;
GLOBAL char *lefts, *middles, *rights;
