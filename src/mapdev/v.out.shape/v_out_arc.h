/* albrll.c */
int albrll(float *, float *, double *, double *);
/* utmll.c */
int utmll(float *, float *, double *, double *, int *);
/* writ_a_lines.c */
int write_area_lines(struct Map_info *, FILE *);
/* write.c */
int start_coords(void);
int write_coords(FILE *, register int, double *, double *);
int end_coords(FILE *);
/* write_areas.c */
int write_areas(char *, char *, struct Map_info *, FILE *, FILE *, FILE *);
/* write_lines.c */
int write_lines(char *, char *, struct Map_info *, FILE *, FILE *);
