/* Pmap.c */
int Pmap(char *, int);
/* ask_bg.c */
int ask_background(FILE *);
/* ask_cell.c */
int ask_cell(FILE *);
/* ask_color.c */
int ask_color(char *, FILE *);
/* ask_grid.c */
int ask_grid(FILE *);
/* ask_icon.c */
int ask_for_icon(char *, FILE *);
float ask_icon_size(void);
/* ask_labels.c */
int ask_labels(FILE *);
/* ask_legend.c */
int ask_legend(FILE *, int);
/* ask_masked.c */
int ask_masked(char *, FILE *);
/* ask_scale.c */
int ask_scale(FILE *);
/* ask_sites.c */
int ask_sites(FILE *);
/* ask_vect.c */
int ask_vectors(FILE *);
/* ask_width.c */
int ask_width(char *, FILE *);
/* hitreturn.c */
int hitreturn(void);
/* input.c */
int input(char *);
/* newscreen.c */
int newscreen(int);
/* record.c */
int begin_record(char *);
int add_record(char *);
int end_record(void);
int print_record(void);
/* scan_color.c */
int scan_color(char *, int);
/* scan_scale.c */
int scan_scale(char *);
/* yes.c */
int yes(char *);
