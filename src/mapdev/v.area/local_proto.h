/* area_calc.c */
double area_calc(int, double *, double *);
/* clip.c */
short int clip(short int *, double *, double *, double *,
      double *, double, double, double, double);
/* do_v_stats.c */
int do_v_stats(int, int, char *);
int codes(int);
/* perimeter.c */
double perimeter(int, double *, double *);
/* where_am_i.c */
int where_am_i(char *, char *, char *, int, char *);

#ifdef GRASS_VECT_H
/* area_perim.c */
int area_perim(double, double, struct Map_info *,
    struct line_pnts *, char *, char *, char *, int, char *, struct Categories *cats);
#endif
