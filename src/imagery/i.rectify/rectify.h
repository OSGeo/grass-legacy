/* ask_files.c */
int ask_files(char *);
int dots(char *, int);
/* ask_files2.c */
int ask_file_from_list(char *, char *);
/* ask_wind.c */
int ask_window(struct Cell_head *);
/* compress.c */
int compress(char *);
/* cp.c */
int get_control_points(char *);
/* env.c */
int select_current_env(void);
int select_target_env(void);
int show_env(void);
/* exec.c */
int exec_rectify(void);
/* get_wind.c */
int get_target_window(void);
int georef_window(struct Cell_head *, struct Cell_head *);
/* mail.c */
int mail(char *);
/* matrix.c */
int compute_georef_matrix(struct Cell_head *, struct Cell_head *);
/* perform.c */
int perform_georef(int, void *);
/* rectify.c */
int rectify(char *, char *, char *);
/* report.c */
int report(char *, char *, char *, char *, long, long, int);
/* target.c */
int get_target(char *);
/* write.c */
int write_matrix(int, int);
int write_map(char *);
