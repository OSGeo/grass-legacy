/* check_resp.c */
int check_responses(void);
/* display.c */
int display_all(FILE *);
/* do_label.c */
int initialize_options(void);
int do_label(FILE *);
int show_it(void);
int scan_ref(char *);
/* gather.c */
int gather(FILE *);
/* get_loc.c */
int get_location(int, char *, char *);
/* in_window.c */
int in_window(char *, char *);
/* main.c */
int sav_file(char *, char *);
/* modify.c */
int modify(void);
/* opencell.c */
int opencell(char *, char *, char *);
/* output.c */
int output(FILE *, char *, char *);
/* proc_new.c */
int process_new(FILE *, char *, char *);
/* proc_old.c */
int process_old(FILE *, char *, char *);
/* setup.c */
int setup(char *, int);
/* show.c */
int show_utm(double, double);
int show_mouse(void);
int show_menu1(void);
int show_menu2(void);
int show_loc(void);
/* show_a_label.c */
int show_a_label(char *);
/* update.c */
int update(FILE *);
/* where.c */
int where(char *, char *);
int get_button(int);
