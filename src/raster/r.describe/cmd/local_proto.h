/* describe.c */
int describe(char *, char *, int, int, char *, int, int, int, int);
/* dumplist.c */
int long_list(struct Cell_stats *, DCELL, DCELL, char *, RASTER_MAP_TYPE, int);
int compact_list(struct Cell_stats *, DCELL, DCELL, char *, RASTER_MAP_TYPE, int);
int compact_range_list(CELL, CELL, CELL, CELL, CELL, CELL, char *);
int range_list(CELL, CELL, CELL, CELL, CELL, CELL, char *);
/* main.c */
int main(int, char *[]);
/* percent.c */
int percent(int, int, int);
/* tree.c */
int plant_tree(void);
int add_node_to_tree(register CELL);
int first_node(void);
int next_node(void);
int first_cat(CELL *);
int next_cat(CELL *);
