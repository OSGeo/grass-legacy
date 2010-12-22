/* env.c */
int select_current_env(void);
int select_target_env(void);

/* i.photo.rectify.c */
int rectify(char *);

/* run.c */
int run_etc_imagery(char *, char *);
int run_system(char *);

/* target.c */
int get_target(char *, struct Cell_head *);
