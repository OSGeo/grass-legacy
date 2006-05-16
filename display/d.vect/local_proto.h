#include <grass/symbol.h>

FILE *open_vect(char *, char *);
int close_vect(FILE *);
int plot1(struct Map_info *, int, int, struct cat_list *, int, int, int, SYMBOL *, int, int, int, int, char *, int, char *, double);
int label(struct Map_info *, int, int, struct cat_list *, LATTR *, int);
int topo(struct Map_info *, int, int, LATTR *);
int dir(struct Map_info *, int, struct cat_list *, int);
int darea(struct Map_info *, struct cat_list *, int, int, int, int, int, int, struct Cell_head *, char *, int, char *, double);
int attr(struct Map_info *, int, char *, struct cat_list *, LATTR *, int);
int zcoor(struct Map_info *, int, LATTR *);
int test_bg_color (const char*);
