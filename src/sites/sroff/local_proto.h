/* block.c */
int begin_block(void);
int add_block(char *);
int begin_block_title(void);
int add_block_title(char *);
int end_block(void);
/* center.c */
int center(char *, int);
/* column.c */
int setcols(char *);
int setcolsep(char *);
int columns(char *);
/* divider.c */
int begin_divider(void);
int add_divider(char *);
/* eject.c */
int eject(void);
/* output.c */
int output(char *);
/* parse.c */
int parse(char *, char *[], int, char *);
/* title.c */
int begin_title(void);
int add_title(char *);
/* top_page.c */
int top_of_page(void);
/* xalloc.c */
char *xalloc(int);
