#include "gis.h"
#define NUL_STR "\0"
#define NUL_EXP  0

#define LIST_EXPR      1
#define PRNT_EXPR      2
#define ASSGN_EXPR     3
#define CHOOS_EXPR     4
#define UNCHOOS_EXPR   5
#define SAVE_EXPR      6
#define RECOV_EXPR     7
#define EXECUT_EXPR    8
#define QUIT_EXPR      9
#define MISC_EXPR     10
#define HELP_EXPR     11
#define NULL_EXPR     12
#define ERR_EXPR      13
#define COLR_EXPR     14

#define ADD			0
#define MULT		1

#define MAX_MAPS 6
struct mapdef
{
    int used ;
    char name[20] ;
    char mapset[20] ;
    struct Histogram histo ;
} ;

extern struct mapdef mapdef[] ;
extern int analysis_type ;
extern long normalize ;
extern char map_name[] ;
extern char input_buf[] ;

#define GETS mygets(input_buf)
char *mygets() ;
/* add_mult.c */
int set_to_add(void);
int set_to_mult(void);
/* ask_weight.c */
int ask_weights(char *);
/* assign.c */
int assign_single(char *, long, long);
int assign_mult(char *, long, long, long);
/* choose_map.c */
int choose_map(char *);
int unchoose_map(char *);
int release_map(int);
/* colors.c */
int make_colors(struct Colors *, CELL, CELL);
int select_colors(char *);
/* dots.c */
int dots(char *, char *, int);
/* execute.c */
int execute(void);
/* falloc.c */
char *falloc(unsigned, unsigned, char *);
/* getc.c */
int mygetc(FILE *);
/* gets.c */
char *mygets(char *);
/* gis_lxcl.c */
int yywrap(void);
/* gis_pars.c */
int yyerror(char *);
int yyparse(void);
/* histo.c */
int get_histo(char *, char *, struct Histogram *);
/* list_anal.c */
int list_analysis(int);
int print_range(FILE *, long, int, int, int);
/* list_cats.c */
int list_cats(char *);
/* list_maps.c */
int list_maps(void);
/* sav_rec.c */
int save(char *);
int recover(char *);
/* set_sigs.c */
int set_signals(int);
/* weighted.c */
int at_console(void);
/* write_supp.c */
int write_supp(char *, char *);
