/* basic.c */
void basic_relax(void);
int basic_relaxf(void);
void basic_error(void);
void basic_error_hook(void (*)(void));
int basic_kbytes(int);
double basic_mbytes(int);
char *basic_strip(char *);
void basic_system(void);
/* cb.c */
void basic_cb_putc(int);
char *basic_cb_str(void);
void basic_cb_clear(void);
int basic_cb_len(void);
int basic_cb_size(void);
void basic_cb_push_buffer(void);
void basic_cb_pop_buffer(void);
/* cb_doprnt.c */
int basic_cb_doprnt(register char *, va_list)
int basic_cb_do2prnt( int, register char *, va_list *)
/* counter.c */
char *basic_counter (Basic_counter);
void basic_counter_reset (Basic_counter *);
void basic_counter_plus_ ( Basic_counter *, int);
/* files.c */
int basic_access(char []);
/* getarg.c */
int basic_getarg_init(int, char **);
int basic_getarg_inite(int, char **);
int basic_getarg(char *);
/* isort.c */
int basic_isort2(int *, int *);
int basic_isort3(int *, int *, int *);
int basic_isort4p(int *, int *, int *, int *);
int basic_isort4(int *, int *, int *, int *);
int basic_isort5p(int *, int *, int *, int *, int *);
/* istaque.c */
int Basic_istaque_adt basic_istaque_new (int);
void basic_istaque_dispose (Basic_istaque_adts);
void basic_istaque_clear (Basic_istaque_adt);
int basic_istaque_empty (Basic_istaque_adt);
int basic_istaque_top (Basic_istaque_adt);
int basic_istaque_pop (Basic_istaque_adt);
void basic_istaque_push ( Basic_istaque_adt, int);
int basic_istaque_bot (Basic_istaque_adt);
int basic_istaque_get (Basic_istaque_adt);
int basic_istaque_length (Basic_istaque_adt);
void basic_istaque_print ( Basic_istaque_adt , FILE *);
double basic_istaque_resize ( Basic_istaque_adt , double );
/* malloc.c */
char *basic_malloc(int, char *, int);
char *basic_realloc(char *, int, char *, int);
void basic_free(char *, char *, int);
char *basic_strdup(char *, char *, int);
void basic_malloc_debug(int);
void basic_malloc_mark(char *, int);
int basic_malloc_marked_bytes(int, int);
/* math2.c */
int basic_ipower(int, int);
double log2(double);
double exp2(double);
double exp10(double);
/* qsort.c */
void basic_qsort(int [], int, int, int (*)(void));
/* time.c */
double basic_utime(void);
char *basic_hostname(void);
char *basic_date(void);
void basic_daytime(int *, int *, int *, int *);
double basic_seconds(void);
int basic_seed(void);
/* tokenize.c */
int basic_tokenize(char [], char *[], int);
/* uhash.c */
void basic_uhash_new(int, int, int *, int []);
