/* open.c */
FILE *open_vect(char *, char *);
int close_vect(FILE *);
/* plot1.c */
int plot1(struct Map_info *, int, int, struct cat_list *, int, int);
/* plot2.c */
int plot2(char *, char *, struct line_pnts *);
/* label.c */
int label(struct Map_info *, int, int, struct cat_list *, LATTR *);
/* use_plot1.c */
int use_plot1(char *, char *);
