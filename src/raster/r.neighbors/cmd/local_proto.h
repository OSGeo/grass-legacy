/* bufs.c */
int allocate_bufs(void);
int rotate_bufs(void);
/* c_var.c */
double d_var(register DCELL *, int, RASTER_MAP_TYPE);
/* gather */
int gather (DCELL *,int);
/* readcell.c */
int readcell(int, int, int, int);
/* sort_cell.c */
int sort_cell (DCELL *,int,int);
