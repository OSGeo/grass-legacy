/* eq.c */
int eq_grey_colors(char *, char *, struct Colors *, int);
/* log.c */
int log_grey_colors(char *, char *, struct Colors *, int, int, int);
int G_make_histogram_log_colors(struct Colors *, struct Cell_stats *, int, int);
/* main.c */
int main(int, char *[]);
int more_usage(void);
/* rules.c */
int read_color_rules(struct Colors *, int, DCELL, DCELL, int);
int read_rule(double *, int *, int *, int *, int *, int *, int *, DCELL, DCELL);
int badrule(char *, int);
int lookup_color(char *, int *, int *, int *);
int show_colors(FILE *);
