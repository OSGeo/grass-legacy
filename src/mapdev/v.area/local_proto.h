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
#define GET_VAL 0
#define SET_VAL 1
int area_perim(double, double, struct Map_info *,
    struct line_pnts *, char *, char *, char *, int, char *, struct Categories *cats);
double get_total_area_of_islands(struct Map_info *map1, plus_t a_indx);
int proc_i_flag(int opflag, int *if_val);
#endif
