
/****************************************************************************/

extern int map_type(const char *name, int mod);
extern int open_map(const char *name, int mod, int row, int col);
extern int setup_maps(void);
extern int close_maps(void);
extern int get_map_row(int idx, int mod, int row, int col, void *buf, int res_type);

extern int open_output_map(const char *name, int res_type);
extern int put_map_row(int fd, void *buf, int res_type);
extern int close_output_map(int fd);

/****************************************************************************/

