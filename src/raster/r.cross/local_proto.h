/* cats.c */
int set_cat(CELL, CELL *, struct Categories *);
/* cross.c */
CELL cross(int [], int, int, int, int);
/* main.c */
int main(int, char *[]);
/* renumber.c */
int renumber(int, int, int);
/* store.c */
int store_reclass(CELL, int, CELL *);
/* tree.c */
int plant_tree(void);
int first_node(CELL **, CELL **);
int next_node(int, CELL **, CELL **);
CELL index_cat(register CELL);
int uproot_tree(void);
