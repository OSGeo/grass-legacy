/* execute.c */
int execute_distance(int);
/* find_dist.c */
int begin_distance(int);
int find_distances(int);
int reset_distances(void);
int find_ll_distance_ncols(int);
/* init.c */
int init_grass(char *);
/* parse_dist.c */
int parse_distances(char **, double);
/* process_at.c */
int process_at(int, int, int, int);
/* process_left.c */
int process_left(int, int, int, int);
/* process_rite.c */
int process_right(int, int, int, int);
/* process_row.c */
int process_row(int, int);
/* read_map.c */
int read_input_map(char *, char *, int, int);
/* support.c */
int make_support_files(char *, char *);
/* write_map.c */
int write_output_map(char *, int, int);
