FILE *open_vect(char *, char *);
int close_vect(FILE *);
int plot1(struct Map_info *, int, int, struct cat_list *, int, int, int);
int plot2(char *, char *, struct line_pnts *);
int label(struct Map_info *, int, int, struct cat_list *, LATTR *, int);
int topo(struct Map_info *, int, int, LATTR *);
int dir(struct Map_info *, int, struct cat_list *, int);
int darea(struct Map_info *, struct cat_list *, int, int, int);
int use_plot1(char *, char *);
