/* accum_down.c */
int accum_down(OUTPUT *);
/* basin_maps.c */
int basin_maps(INPUT *, OUTPUT *);
/* com_line.c */
int com_line_Gwater(INPUT *, OUTPUT *);
int com_line_add(char **, char *, char *, char *);
int basin_com_add(char **, double, double, struct Cell_head *);
int com_add(char **, char *, int);
/* file_in.c */
int ar_file_in(char *, OUTPUT *);
/* free.c */
int free_input(INPUT *);
int free_output(OUTPUT *);
/* insert_cat.c */
int insert_cat(CAT *, CELL, int);
CAT *new_cat(CELL, int);
/* intro.c */
int intro(void);
/* print.c */
int print_output(OUTPUT *);
/* read.c */
int read_basins(char *, OUTPUT *);
/* valid.c */
int valid_basins(char *, OUTPUT *);
