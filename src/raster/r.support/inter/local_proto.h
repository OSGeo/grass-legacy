/* ask_format.c */
int ask_format(char *, struct Cell_head *, long);
/* check.c */
int check_stats(char *);
/* check_new.c */
int check_new_compressed(struct Cell_head *, int);
/* check_old.c */
int check_old_compressed(struct Cell_head *, int);
/* check_un.c */
int check_uncompressed(struct Cell_head *, long);
/* factors.c */
int factors(FILE *, register long, int);
/* histo.c */
int do_histogram(char *, char *);
/* hitreturn.c */
int hitreturn(void);
/* row_addr.c */
int next_row_addr(int, long *, int);
/* support.c */
int G_clear_screen(void);
