#ifdef GRASS_GIS_H
/* colors.c */
int make_16_colors(struct Colors *);
struct Cell_head *get_cur_win(void);
/* write.c */
int write_window(struct Cell_head *);
/* getc.c */
int mygetc(FILE *);
#endif

/* cry_n_die.c */
int cry_and_die(char *);
/* falloc.c */
char *falloc(unsigned, unsigned, char *);
/* get_c_win.c */
int init_proj_win(void);
/* get_cats.c */
int get_cats(char *);
/* get_win.c */
int get_win(char *);
/* gis_lxcl.c */
int switch_input(char *);
int shell_escape(char *);
int yywrap(void);
int clear_input(void);
/* gis_pars.c */
int yyerror(char *);
int yyparse(void);
/* init_comb.c */
int init_comb(int, char *[]);
int at_console(void);
/* r_pr_hist.c */
int start_line_count(void);
int check_lines(void);
int do_tabs(int);
/* set_sigs.c */
int set_signals(void);
int reset_signals(void);
/* sigint.c */
void sigint(int);
