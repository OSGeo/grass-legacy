/* elev.h */
#ifdef MAIN
     char *elev_layer;
     char *mapset_elev;
     char *tl;
     char *math_exp;
     char *units;
     char *nd;
#else
     extern char *elev_layer;
     extern char *mapset_elev;
     extern char *tl;
     extern char *math_exp;
     extern char *units;
     extern char *nd;
#endif

static int  which_env;
static int  stat;

/* ask_elev.c */
int ask_elev(char *, char *, char *);
/* main.c */
int select_current_env(void);
int select_target_env(void);
/* mod_elev.c */
int mod_elev_data(void);
