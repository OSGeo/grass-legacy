/* parse_line.c */
int parse_line(char *, char **, double *, double *, double *, double *, int);
/* process_line.c */
int process_line(int, CELL *, double, double, double, double, struct Cell_head *);
/* profile.c */
int init_profiles(int, char *);
int graph_point(int, int);
int add_point(int, int);
int profile(int, CELL *, int, int, int, int);
